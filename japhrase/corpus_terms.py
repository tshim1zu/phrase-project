# coding: utf-8
"""
塊（チャンク）ごとに数えて統合する語抽出 — コーパス全体の「講義をまたぐ語」を出す。

なぜ必要か（2026-10-01 の実測）:
    PhraseExtractor.extract() は文字 n-gram を全部 DataFrame に載せたうえで
    hold_higherrank / remove_similar という O(n^2) の pandas 走査をする。
    psyzunda の台本 1 冊（198 発話, 8,937 字）で 125 秒（cProfile 下）、
    その 97 秒が hold_higherrank、28 秒が remove_similar で、n-gram の列挙と
    数え上げは合計 0.5 秒未満だった。全 96 冊（約 11,300 発話）をまとめると終わらない。

方針:
    1. コーパスを塊（本ごと・一定行数ごと・ディレクトリごと）に分ける。
    2. 塊ごとに n-gram を数える。Apriori 法で、長さ k の n-gram は
       「長さ k-1 の前半と後半がどちらも全体で生き残った」ものだけを数える。
       出現回数も「出る塊の数」も部分文字列ほど大きい（単調）ので、この枝刈りは厳密。
    3. 塊ごとの結果は cache_dir へ保存する（中断しても続きから。塊の本文が変われば作り直し）。
    4. 統合は「語 -> {塊ID: 回数}」の和集合なので、塊の順序に依存しない（決定的）。

出力は (語, 出現回数, 出る塊の数) の表。包含される短い語は捨て（closed）、
ひらがなで始まる／終わる断片は既定で捨てる（edge_filter）。
"""

from __future__ import annotations

import glob as _glob
import hashlib
import json
import os
import re
import unicodedata
from collections import Counter, defaultdict
from pathlib import Path
from typing import Dict, Iterable, List, Mapping, Optional, Sequence

import pandas as pd

CACHE_VERSION = 1
# 語の途中に入らない文字（句読点・括弧・空白・記号）で区切る。かな・漢字・英数・長音は語の文字
DEFAULT_SPLIT = r"[\W_]+"

TermTable = Dict[str, Dict[str, int]]  # 語 -> {塊ID: 回数}


def _is_hiragana(ch: str) -> bool:
    return "ぁ" <= ch <= "ゟ"


_RIGHT_STOP = frozenset("のをはがにでともやへてたねよかな")
_LEFT_STOP = frozenset("のをはがにでともやへ")  # edge_filter="loose" で語頭に来ない助詞
_BAD_START = frozenset("ーッャュョァィゥェォヮ")  # 長音・促音・拗音・小書き仮名では語は始まらない
_CONT_CLASSES = frozenset("KTA")  # 端の続きを断片判定に使う字種: 漢字 / カタカナ / 英数


def _script_class(ch: str) -> str:
    """K=漢字 H=ひらがな T=カタカナ(長音含む) A=英数 O=その他"""
    o = ord(ch)
    if "一" <= ch <= "鿿" or ch in "々〆ヶ" or "㐀" <= ch <= "䶿":
        return "K"
    if "ぁ" <= ch <= "ゟ":
        return "H"
    if "゠" <= ch <= "ヿ" or ch == "ｰ" or "ｦ" <= ch <= "ﾟ":
        return "T"
    if ch.isascii() and ch.isalnum() or "０" <= ch <= "９" or "Ａ" <= ch <= "Ｚ" or "ａ" <= ch <= "ｚ":
        return "A"
    return "O"


def merge_counts(*tables: Mapping[str, Mapping[str, int]]) -> TermTable:
    """語表を塊IDごとの和集合で統合する。順序に依存せず、同じ表を足し直しても変わらない（冪等）。

    同じ塊IDが複数の表にあるときは「同じ塊の数え直し」とみなし、大きい方を採る（同じ入力なら同値）。
    """
    out: TermTable = {}
    for table in tables:
        for term, per_chunk in table.items():
            dst = out.setdefault(term, {})
            for cid, n in per_chunk.items():
                if n > dst.get(cid, 0):
                    dst[cid] = int(n)
    return out


def _segments(texts: Iterable[str], split_re) -> List[str]:
    segs: List[str] = []
    for t in texts:
        for seg in split_re.split(str(t)):
            if seg:
                segs.append(seg)
    return segs


def _digest(parts: Iterable[str]) -> str:
    h = hashlib.sha1()
    for p in parts:
        h.update(p.encode("utf-8"))
        h.update(b"\x00")
    return h.hexdigest()


class ChunkedPhraseExtractor:
    """塊ごとに数え、統合して、講義（塊）をまたぐ語を返す。"""

    def __init__(
        self,
        min_length: int = 2,
        max_length: int = 12,
        min_count: int = 2,
        min_chunks: int = 2,
        edge_filter: str = "content",
        closed: bool = True,
        closed_ratio: float = 0.8,
        fragment_ratio: Optional[float] = 0.5,
        split_pattern: str = DEFAULT_SPLIT,
        cache_dir: Optional[os.PathLike] = None,
    ):
        """
        Parameters:
            min_length / max_length: 語の最小・最大文字数
            min_count: 全体での最小出現回数
            min_chunks: 出現する塊の最小数（講義数）
            edge_filter: "content"=ひらがなで始まる断片と、助詞（の・を・は・が・に・で・と・も・や・へ・て・た・ね・よ・か・な）で
                終わる断片を捨てる（仕組み・考え方のような送り仮名で終わる語は残る）。"loose"=語頭の助詞・語末の助詞だけ捨て、
                ひらがな始まりは残す（ずれ・だめ のようなかな書きの語を表記ゆれ検出に回すため。2文字の語は残す）。"kana"=ひらがなで始まる／終わるものを全部捨てる（語を漢字・カタカナ・
                英数の端を持つものに絞る。仕組み・考え方のような送り仮名つきの語も落ちる）。"none"=捨てない
            closed: True なら、1文字長い語が（出現回数の closed_ratio 倍以上で）ある短い断片を捨てる
            closed_ratio: closed の許容比（1.0 なら同じ回数のときだけ）
            fragment_ratio: 漢字・カタカナ・英数の端が「同じ字種の続き」である出現の割合がこれ以上の語を
                断片として捨てる（理学←心理学, ント←コメント）。None で無効。文脈の集計が追加で1周かかる
            split_pattern: 語をまたがせない区切りの正規表現
            cache_dir: 塊ごとの結果の保存先（None なら保存しない）
        """
        if edge_filter not in ("content", "kana", "loose", "none"):
            raise ValueError("edge_filter は 'content' / 'kana' / 'loose' / 'none'")
        if min_length < 1 or max_length < min_length:
            raise ValueError("min_length/max_length が不正")
        self.min_length = min_length
        self.max_length = max_length
        self.min_count = min_count
        self.min_chunks = min_chunks
        self.edge_filter = edge_filter
        self.closed = closed
        self.closed_ratio = closed_ratio
        self.fragment_ratio = fragment_ratio
        self.split_pattern = split_pattern
        self._split = re.compile(split_pattern)
        self.cache_dir = Path(cache_dir) if cache_dir is not None else None
        self.stats = {"computed": 0, "cached": 0}
        self.variant_table: TermTable = {}

    # ---- 塊ごとの数え上げ -------------------------------------------------
    def _cache_path(self, chunk_digest: str, k: int, surv_digest: str) -> Optional[Path]:
        if self.cache_dir is None:
            return None
        key = _digest([str(CACHE_VERSION), chunk_digest, str(k), surv_digest, self.split_pattern])
        return self.cache_dir / f"{key}.json"

    @staticmethod
    def _count_level(segs: Sequence[str], k: int, surv: Optional[frozenset]) -> Counter:
        c: Counter = Counter()
        for seg in segs:
            n = len(seg) - k + 1
            if n <= 0:
                continue
            if surv is None:
                c.update(seg[i:i + k] for i in range(n))
            else:
                for i in range(n):
                    g = seg[i:i + k]
                    if g[:-1] in surv and g[1:] in surv:
                        c[g] += 1
        return c

    def _chunk_level(self, segs, chunk_digest, k, surv, surv_digest) -> Dict[str, int]:
        path = self._cache_path(chunk_digest, k, surv_digest)
        if path is not None and path.exists():
            try:
                data = json.loads(path.read_text(encoding="utf-8"))
                if data.get("k") == k:
                    self.stats["cached"] += 1
                    return {str(g): int(n) for g, n in data["counts"].items()}
            except (OSError, ValueError, KeyError, TypeError, AttributeError):
                pass  # 壊れた・読めないキャッシュは作り直す
        counts = dict(self._count_level(segs, k, surv))
        self.stats["computed"] += 1
        if path is not None:
            self.cache_dir.mkdir(parents=True, exist_ok=True)
            tmp = path.with_suffix(".tmp%d" % os.getpid())
            tmp.write_text(json.dumps({"k": k, "counts": counts}, ensure_ascii=False), encoding="utf-8")
            os.replace(tmp, path)
        return counts

    def count_table(self, chunks: Mapping[str, Sequence[str]]) -> TermTable:
        """枝刈りなしの生の語表（語 -> {塊ID: 回数}）。出現回数・塊数の条件だけを満たす全 n-gram。"""
        return self._count_table(self._prepare(chunks))

    def _prepare(self, chunks):
        ids = sorted(chunks)  # 結果は順序に依らないが、処理順も固定して再現しやすくする
        segs = {cid: _segments(chunks[cid], self._split) for cid in ids}
        return ids, segs

    def _count_table(self, prepared) -> TermTable:
        ids, segs = prepared
        digests = {cid: _digest(segs[cid]) for cid in ids}
        table: TermTable = {}
        surv: Optional[frozenset] = None
        surv_digest = "all"
        for k in range(self.min_length, self.max_length + 1):
            per_chunk = {cid: self._chunk_level(segs[cid], digests[cid], k, surv, surv_digest) for cid in ids}
            total: Counter = Counter()
            nchunk: Counter = Counter()
            for counts in per_chunk.values():
                for g, n in counts.items():
                    total[g] += n
                    nchunk[g] += 1
            keep = {g for g, n in total.items() if n >= self.min_count and nchunk[g] >= self.min_chunks}
            if not keep:
                break
            for cid in ids:
                for g, n in per_chunk[cid].items():
                    if g in keep:
                        table.setdefault(g, {})[cid] = n
            surv = frozenset(keep)
            surv_digest = _digest(sorted(keep))
        return table


    def _context_pass(self, prepared, terms) -> Dict[str, tuple]:
        """語ごとに (出現数, 左が同じ字種の続きの数, 右が同じ字種の続きの数) を数える。"""
        ids, segs = prepared
        by_len: Dict[int, set] = defaultdict(set)
        for t in terms:
            by_len[len(t)].add(t)
        out: Dict[str, list] = {}
        cls = _script_class
        for cid in ids:
            for seg in segs[cid]:
                n = len(seg)
                for k, group in by_len.items():
                    for i in range(n - k + 1):
                        g = seg[i:i + k]
                        if g in group:
                            rec = out.get(g)
                            if rec is None:
                                rec = out[g] = [0, 0, 0]
                            rec[0] += 1
                            c0 = cls(g[0])
                            if i > 0 and c0 in _CONT_CLASSES and cls(seg[i - 1]) == c0:
                                rec[1] += 1
                            c1 = cls(g[-1])
                            if i + k < n and c1 in _CONT_CLASSES and cls(seg[i + k]) == c1:
                                rec[2] += 1
        return {g: tuple(v) for g, v in out.items()}

    # ---- 選別 -------------------------------------------------------------
    def select(self, table: Mapping[str, Mapping[str, int]], context: Optional[Mapping[str, tuple]] = None) -> pd.DataFrame:
        """端の断片・字種の途中で切れた断片・含まれる短い語を捨てて DataFrame にする。"""
        rows = {}
        eligible = set()
        for term, per in table.items():
            if len(term) < self.min_length:
                continue
            n = sum(per.values())
            c = len(per)
            if n < self.min_count or c < self.min_chunks:
                continue
            rows[term] = (n, c)
            if self.edge_filter == "kana" and (_is_hiragana(term[0]) or _is_hiragana(term[-1])):
                continue
            if self.edge_filter == "content" and (_is_hiragana(term[0]) or term[-1] in _RIGHT_STOP):
                continue
            if self.edge_filter == "loose" and len(term) >= 3 and (term[0] in _LEFT_STOP or term[-1] in _RIGHT_STOP):
                continue
            if self.edge_filter != "none" and term[0] in _BAD_START:
                continue
            if self.fragment_ratio is not None and context is not None:
                tot, lcont, rcont = context.get(term, (0, 0, 0))
                if tot and max(lcont, rcont) / tot >= self.fragment_ratio:
                    continue
            eligible.add(term)
        if self.closed:
            # 1文字長い語が closed_ratio 倍以上の回数である短い語は、その長い語の一部とみなして捨てる。
            # ただし長い語が助詞で終わる等で捨てられる側（eligible でない）なら、さらに長い eligible な語へ
            # 連鎖で届くときだけ捨てる（ストレス が ストレスの に食われて消えないように）。
            parents: Dict[str, List[str]] = defaultdict(list)
            for term, (n, _) in rows.items():
                if len(term) - 1 < self.min_length:
                    continue
                for sub in (term[1:], term[:-1]):
                    if sub in rows and n >= self.closed_ratio * rows[sub][0]:
                        parents[sub].append(term)
            reaches: Dict[str, bool] = {}
            for term in sorted(rows, key=lambda t: -len(t)):
                reaches[term] = term in eligible or any(reaches[q] for q in parents.get(term, ()))
            rows = {t: rows[t] for t in eligible
                    if not any(reaches[q] for q in parents.get(t, ()))}
        else:
            rows = {t: rows[t] for t in eligible}
        data = sorted(((t, n, c) for t, (n, c) in rows.items()), key=lambda r: (-r[1], -r[2], r[0]))
        return pd.DataFrame(
            {
                "seqchar": [r[0] for r in data],
                "freq": [r[1] for r in data],
                "chunks": [r[2] for r in data],
                "length": [len(r[0]) for r in data],
            },
            columns=["seqchar", "freq", "chunks", "length"],
        )

    def extract(self, chunks: Mapping[str, Sequence[str]]) -> pd.DataFrame:
        """塊 {ID: [テキスト...]} から (語, 出現回数, 出る塊の数) を返す。"""
        return self.extract_with_table(chunks)[0]

    def extract_with_table(self, chunks: Mapping[str, Sequence[str]]):
        """extract と同じ表に加え、選ばれた語の {語: {塊ID: 回数}} も返す（表記ゆれ検出の入力用）。"""
        prepared = self._prepare(chunks)
        table = self._count_table(prepared)
        context = self._context_pass(prepared, table) if self.fragment_ratio is not None else None
        df = self.select(table, context)
        # 表記ゆれ用の候補は、助詞の端だけ捨てて、かな書きの語は残す（ズレ/ずれ・ダメ/だめ を対象にする）
        saved, self.edge_filter = self.edge_filter, "loose"
        try:
            pool = self.select(table, context)
        finally:
            self.edge_filter = saved
        self.variant_table = {t: table[t] for t in pool["seqchar"]}
        return df, {t: table[t] for t in df["seqchar"]}


# ---- 入力 ------------------------------------------------------------------
def chunks_from_files(
    paths: Sequence[str],
    chunk_by: str = "file",
    lines_per_chunk: int = 100,
    line_regex: Optional[str] = None,
    encoding: str = "utf-8",
) -> Dict[str, List[str]]:
    """ファイル群を塊 {ID: [行...]} にする。

    chunk_by: "file"=1ファイル1塊 / "dir"=同じディレクトリを1塊 / "lines"=ファイル内を一定行数ごと
    line_regex: 指定すると、マッチした行の named group "text"（無ければ group 1）だけを本文にし、
        マッチしない行は捨てる。
    """
    if chunk_by not in ("file", "dir", "lines"):
        raise ValueError("chunk_by は file / dir / lines")
    rx = re.compile(line_regex) if line_regex else None
    chunks: Dict[str, List[str]] = {}
    for p in paths:
        try:
            text = Path(p).read_text(encoding=encoding)
        except (OSError, UnicodeDecodeError):
            continue
        lines: List[str] = []
        for raw in text.splitlines():
            line = raw.strip()
            if not line:
                continue
            if rx is not None:
                m = rx.search(line)
                if not m:
                    continue
                line = m.groupdict().get("text") or (m.group(1) if m.groups() else m.group(0))
            lines.append(line)
        if chunk_by == "file":
            chunks[str(p)] = lines
        elif chunk_by == "dir":
            chunks.setdefault(str(Path(p).parent), []).extend(lines)
        else:
            step = max(1, int(lines_per_chunk))
            for n, i in enumerate(range(0, len(lines), step)):
                chunks[f"{p}#{n}"] = lines[i:i + step]
    return chunks


# ---- 表記ゆれ ---------------------------------------------------------------
def normalize_key(term: str) -> str:
    """表記ゆれを吸収するキー: 全半角・大小文字・カタカナ/ひらがな・長音・中黒を畳む。"""
    s = unicodedata.normalize("NFKC", term).lower()
    out = []
    for ch in s:
        if "ァ" <= ch <= "ヶ":
            ch = chr(ord(ch) - 0x60)
        if ch in "ー・・ 　-":
            continue
        out.append(ch)
    return "".join(out)


def _script_set(term: str) -> frozenset:
    """長音・中黒を除いた字種の集合（ひらがな/カタカナ/漢字/英数）。"""
    return frozenset(_script_class(ch) for ch in unicodedata.normalize("NFKC", term) if ch not in "ー・")


def _is_orthographic_variant(a: str, b: str) -> bool:
    """同じ normalize_key の2語が、表記だけの違いか。

    長さが同じなら（かな/カナ・全半角・大小文字だけの違い）採る。長さが違う（長音・中黒の有無）ときは、
    字種の構成が同じものだけ採る（コンピューター/コンピュータ・うん/うーん は採り、データ/でた は捨てる）。
    """
    na, nb = unicodedata.normalize("NFKC", a), unicodedata.normalize("NFKC", b)
    if len(na) == len(nb):
        return True
    return _script_set(a) == _script_set(b)


def find_term_variants(
    table: Mapping[str, Mapping[str, int]],
    similarity_threshold: float = 0.8,
    min_length: int = 2,
    include_similar: bool = False,
    max_pairs: int = 50_000_000,
) -> List[dict]:
    """統合後の語表から表記ゆれ候補の組を返す。

    kind="same_key": 表記の違いだけ（かな/カタカナ・全半角・長音・中黒）で同じキーになる組。
    kind="similar"（include_similar=True のときだけ）: TextVariantDetector の編集距離系スコアが閾値以上の組。
        実コーパスでは「今日のテーマ/今日の内容」のような兄弟句ばかりで表記ゆれは出なかったので既定は無効。
        片方がもう片方の部分文字列の組（機械学習/機械学習者 など）は除く。
    cooccur_chunks: 両方が出る塊の数。同じ講義に両方出るなら別の語である可能性が高いので順位を下げる。
    """
    from .text_variant_detector import TextVariantDetector

    terms = [t for t in table if len(t) >= min_length]
    freq = {t: sum(table[t].values()) for t in terms}
    chunk_sets = {t: set(table[t]) for t in terms}
    pairs: Dict[frozenset, dict] = {}

    def add(a: str, b: str, kind: str, confidence: float) -> None:
        key = frozenset((a, b))
        if key in pairs and pairs[key]["kind"] == "same_key":
            return
        if freq[a] < freq[b] or (freq[a] == freq[b] and a > b):
            a, b = b, a
        pairs[key] = {
            "a": a, "b": b, "kind": kind,
            "freq_a": freq[a], "freq_b": freq[b],
            "chunks_a": len(chunk_sets[a]), "chunks_b": len(chunk_sets[b]),
            "cooccur_chunks": len(chunk_sets[a] & chunk_sets[b]),
            "confidence": round(float(confidence), 4),
        }

    by_key: Dict[str, List[str]] = defaultdict(list)
    for t in terms:
        by_key[normalize_key(t)].append(t)
    for key, group in by_key.items():
        if len(group) < 2 or not key:
            continue
        group = sorted(group)
        for i, a in enumerate(group):
            for b in group[i + 1:]:
                if _is_orthographic_variant(a, b):
                    add(a, b, "same_key", 1.0)

    if include_similar and terms:
        # 数字入りの語は桁違いの数どうしが「似ている」と出るだけなので除く
        sim_terms = [t for t in terms if not re.search(r"\d", t) and len(t) >= 3]
        df = pd.DataFrame({"seqchar": sim_terms, "freq": [freq[t] for t in sim_terms]})
        det = TextVariantDetector(similarity_threshold=similarity_threshold,
                                  max_comparison_pairs=max_pairs)
        for cand in det.detect_variants(df, top_n=None):
            for v in cand.variants:
                if cand.primary in v or v in cand.primary:
                    continue
                add(cand.primary, v, "similar", cand.confidence)

    rows = list(pairs.values())
    rows.sort(key=lambda r: (r["kind"] != "same_key", r["cooccur_chunks"] > 0, -r["confidence"],
                             -(r["freq_a"] + r["freq_b"]), r["a"], r["b"]))
    return rows


def expand_globs(patterns: Sequence[str]) -> List[str]:
    """glob パターン（再帰 ** 可）を展開して重複なし・昇順で返す。"""
    out = set()
    for pat in patterns:
        out.update(_glob.glob(pat, recursive=True))
    return sorted(out)
