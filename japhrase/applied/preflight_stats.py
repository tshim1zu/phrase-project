# coding: utf-8
"""
公開前プリフライトチェッカー（Preflight Stats）

EP を PF にアップロードする前に、統計的な品質スコアで Go/NoGo 判定。
ComplexityAnalyzer + audit_immersion パターンを統合。

使用例:
    >>> checker = PreflightChecker()
    >>> result = checker.check(text, lang='jp')
    >>> print(result.verdict)  # 'GO' or 'NOGO'
    >>> print(result.report())
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import re
import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

from ..complexity_metrics import ComplexityAnalyzer
from ..stylometry import StylometryAnalyzer

logger = logging.getLogger(__name__)


# ── 離脱要因パターン（audit_immersion.py と同等） ──────────────
_SKIP_JP = re.compile(
    r'[0-9０-９]+\.?[0-9０-９]*\s*'
    r'(％|%|度|秒|分間|時間|日間|日後|km|㎞|m/s|メートル|キロ|セルシウス)'
)
_SKIP_EN = re.compile(
    r'\b\d+\.?\d*\s*(%|°C|km|m/s|seconds?|minutes?|hours?|days?|meters?)\b',
    re.IGNORECASE,
)
_DRAG_JP = re.compile(r'（[^）]{12,}）')
_DRAG_EN = re.compile(
    r'\[\*[^\]]{8,}\*\]'
    r'|\(\*[^)]{8,}\*\)'
    r'|\[[A-Z*][^\]]{8,}\]'
    r'|\*\([^)]{8,}\)\*'
)
_DRAG_EN_STANDALONE = re.compile(r'^\s*\(([^)]{8,})\)\s*$', re.MULTILINE)


@dataclass
class ImmersionIssue:
    """離脱要因1件"""
    issue_type: str     # 'SKIP', 'DRAG', 'NOISE'
    line_no: int
    text_snippet: str
    severity: str       # 'warning', 'error'


@dataclass
class PreflightResult:
    """プリフライト結果"""
    # 複雑度指標
    perplexity: float
    compression_ratio: float
    lexical_density: float
    information_rate: float

    # 語彙指標
    mattr: float
    hapax_ratio: float
    simpsons_d: float

    # 離脱要因
    skip_count: int
    drag_count: int
    noise_count: int
    immersion_issues: List[ImmersionIssue] = field(default_factory=list)

    # 文字数
    char_count: int = 0
    content_char_count: int = 0

    # 判定
    verdict: str = 'GO'          # 'GO', 'NOGO', 'WARN'
    nogo_reasons: List[str] = field(default_factory=list)
    warn_reasons: List[str] = field(default_factory=list)

    # 総合スコア (0-100)
    quality_score: float = 0.0

    def report(self) -> str:
        """レポート文字列生成"""
        icon = {'GO': '✅', 'WARN': '⚠️', 'NOGO': '❌'}.get(self.verdict, '?')

        lines = [
            "=" * 60,
            f"【プリフライトチェック】 {icon} {self.verdict}",
            f"  総合スコア: {self.quality_score:.1f}/100",
            "=" * 60,
            "",
            "【複雑度】",
            f"  Perplexity:       {self.perplexity:8.2f}",
            f"  Compression:      {self.compression_ratio:8.4f}",
            f"  Lexical Density:  {self.lexical_density:8.4f}",
            f"  Information Rate: {self.information_rate:8.4f}",
            "",
            "【語彙】",
            f"  MATTR:            {self.mattr:8.4f}",
            f"  Hapax Ratio:      {self.hapax_ratio:8.4f}",
            f"  Simpson's D:      {self.simpsons_d:8.6f}",
            "",
            "【離脱要因】",
            f"  SKIP:  {self.skip_count} 件",
            f"  DRAG:  {self.drag_count} 件",
            f"  NOISE: {self.noise_count} 件",
            "",
            f"【文字数】 {self.content_char_count} 字（メタ除外）/ {self.char_count} 字（全体）",
        ]

        if self.nogo_reasons:
            lines.append("")
            lines.append("【❌ NOGO 理由】")
            for r in self.nogo_reasons:
                lines.append(f"  - {r}")

        if self.warn_reasons:
            lines.append("")
            lines.append("【⚠️ 警告】")
            for r in self.warn_reasons:
                lines.append(f"  - {r}")

        if self.immersion_issues:
            lines.append("")
            lines.append("【離脱要因詳細（上位10件）】")
            for issue in self.immersion_issues[:10]:
                lines.append(
                    f"  L{issue.line_no:4d} [{issue.issue_type}] "
                    f"{issue.text_snippet[:50]}"
                )

        lines.append("")
        lines.append("=" * 60)
        return "\n".join(lines)


class PreflightChecker:
    """
    公開前統計プリフライト

    使用例:
        >>> checker = PreflightChecker()
        >>> result = checker.check(ep_text, lang='jp')
        >>> if result.verdict == 'NOGO':
        ...     print("公開不可:", result.nogo_reasons)
    """

    # 閾値設定
    THRESHOLDS = {
        # NOGO 条件（これを満たすと公開不可）
        'nogo_skip': 5,              # SKIP 5件以上
        'nogo_drag': 8,              # DRAG 8件以上
        'nogo_compression_low': 0.20, # 圧縮率が極端に低い = 繰り返し
        'nogo_mattr_low': 0.40,      # MATTR が極端に低い = 語彙枯渇

        # WARN 条件
        'warn_skip': 2,
        'warn_drag': 3,
        'warn_lexical_low': 0.45,
        'warn_info_rate_low': 0.25,
        'warn_hapax_low': 0.20,
    }

    # PF別文字数制限
    PF_CHAR_LIMITS = {
        'sh': {'min': 1000, 'max': 50000, 'recommended': (2000, 8000)},
        'ream': {'min': 500, 'max': None, 'recommended': (1500, 6000)},
        'cien': {'min': 200, 'max': None, 'recommended': (1000, 5000)},
        'nocturne': {'min': 1000, 'max': None, 'recommended': (2000, 10000)},
        'royalroad': {'min': 1000, 'max': None, 'recommended': (2000, 5000)},
    }

    def __init__(self):
        self.complexity = ComplexityAnalyzer()
        self.stylometry = StylometryAnalyzer()

    def check(
        self,
        text: str,
        lang: str = 'jp',
        platform: Optional[str] = None,
    ) -> PreflightResult:
        """
        テキストのプリフライトチェック

        Parameters:
            text: チェック対象テキスト
            lang: 'jp' or 'en'
            platform: PF名（'sh', 'ream', 'cien', etc）— 文字数制限チェック用

        Returns:
            PreflightResult
        """
        # メタデータ除去（YAML frontmatter, code blocks）
        content = self._strip_metadata(text)
        char_count = len(text)
        content_char_count = len(re.sub(r'\s+', '', content))

        # 複雑度分析
        cx = self.complexity.analyze(content)

        # 語彙分析
        adv = self.stylometry.analyze_advanced_diversity(content)
        mattr_result = self.stylometry.analyze_mattr(content)

        # 離脱要因スキャン
        issues = self._scan_immersion(text, lang)
        skip_count = sum(1 for i in issues if i.issue_type == 'SKIP')
        drag_count = sum(1 for i in issues if i.issue_type == 'DRAG')
        noise_count = sum(1 for i in issues if i.issue_type == 'NOISE')

        result = PreflightResult(
            perplexity=cx['perplexity'],
            compression_ratio=cx['compression_ratio'],
            lexical_density=cx['lexical_density'],
            information_rate=cx['information_rate'],
            mattr=mattr_result['mattr'],
            hapax_ratio=adv['hapax_ratio'],
            simpsons_d=adv['simpsons_d'],
            skip_count=skip_count,
            drag_count=drag_count,
            noise_count=noise_count,
            immersion_issues=issues,
            char_count=char_count,
            content_char_count=content_char_count,
        )

        # 判定
        self._judge(result, platform)

        return result

    def _judge(self, result: PreflightResult, platform: Optional[str]):
        """Go/NoGo 判定"""
        T = self.THRESHOLDS
        nogo = []
        warn = []

        # NOGO チェック
        if result.skip_count >= T['nogo_skip']:
            nogo.append(f"SKIP {result.skip_count}件（上限{T['nogo_skip']}）")
        if result.drag_count >= T['nogo_drag']:
            nogo.append(f"DRAG {result.drag_count}件（上限{T['nogo_drag']}）")
        if result.compression_ratio < T['nogo_compression_low']:
            nogo.append(f"圧縮率 {result.compression_ratio:.3f}（下限{T['nogo_compression_low']}）— 繰り返しが極端に多い")
        if result.mattr < T['nogo_mattr_low']:
            nogo.append(f"MATTR {result.mattr:.4f}（下限{T['nogo_mattr_low']}）— 語彙が枯渇")

        # WARN チェック
        if result.skip_count >= T['warn_skip'] and result.skip_count < T['nogo_skip']:
            warn.append(f"SKIP {result.skip_count}件")
        if result.drag_count >= T['warn_drag'] and result.drag_count < T['nogo_drag']:
            warn.append(f"DRAG {result.drag_count}件")
        if result.lexical_density < T['warn_lexical_low']:
            warn.append(f"語彙密度 {result.lexical_density:.4f} — 機能語が多い")
        if result.information_rate < T['warn_info_rate_low']:
            warn.append(f"情報率 {result.information_rate:.4f} — 新情報が少ない")
        if result.hapax_ratio < T['warn_hapax_low']:
            warn.append(f"Hapax比 {result.hapax_ratio:.4f} — 語彙の洗練度が低い")

        # PF別文字数チェック
        if platform and platform in self.PF_CHAR_LIMITS:
            limits = self.PF_CHAR_LIMITS[platform]
            cc = result.content_char_count
            if limits['min'] and cc < limits['min']:
                nogo.append(f"{platform}最小文字数 {limits['min']} に未達（{cc}字）")
            if limits['max'] and cc > limits['max']:
                nogo.append(f"{platform}最大文字数 {limits['max']} 超過（{cc}字）")
            rec = limits.get('recommended')
            if rec and (cc < rec[0] or cc > rec[1]):
                warn.append(f"{platform}推奨範囲 {rec[0]}〜{rec[1]}字 から外れている（{cc}字）")

        result.nogo_reasons = nogo
        result.warn_reasons = warn

        if nogo:
            result.verdict = 'NOGO'
        elif warn:
            result.verdict = 'WARN'
        else:
            result.verdict = 'GO'

        # 総合スコア (0-100)
        result.quality_score = self._calc_quality_score(result)

    def _calc_quality_score(self, result: PreflightResult) -> float:
        """総合品質スコア (0-100)"""
        scores = []

        # 離脱要因: 0件=100, 10件以上=0
        immersion_total = result.skip_count + result.drag_count + result.noise_count
        scores.append(max(0, 100 - immersion_total * 10))

        # MATTR: 0.8以上=100, 0.4以下=0
        scores.append(min(100, max(0, (result.mattr - 0.4) / 0.4 * 100)))

        # Hapax: 0.5以上=100, 0.1以下=0
        scores.append(min(100, max(0, (result.hapax_ratio - 0.1) / 0.4 * 100)))

        # 情報率: 0.6以上=100, 0.1以下=0
        scores.append(min(100, max(0, (result.information_rate - 0.1) / 0.5 * 100)))

        # 圧縮率: 0.4〜0.7が最適。外れるほど減点
        cr = result.compression_ratio
        if 0.4 <= cr <= 0.7:
            scores.append(100)
        elif cr < 0.4:
            scores.append(max(0, cr / 0.4 * 100))
        else:
            scores.append(max(0, (1.0 - cr) / 0.3 * 100))

        return sum(scores) / len(scores) if scores else 0.0

    def _scan_immersion(self, text: str, lang: str) -> List[ImmersionIssue]:
        """離脱要因スキャン"""
        issues = []
        lines = text.split('\n')

        # コードブロック範囲の特定
        in_code = False
        code_lines = set()
        for i, line in enumerate(lines, 1):
            if line.strip().startswith('```'):
                in_code = not in_code
            if in_code:
                code_lines.add(i)

        for i, line in enumerate(lines, 1):
            if i in code_lines:
                continue

            # SKIP
            pat_skip = _SKIP_JP if lang == 'jp' else _SKIP_EN
            for m in pat_skip.finditer(line):
                issues.append(ImmersionIssue(
                    issue_type='SKIP', line_no=i,
                    text_snippet=m.group(), severity='warning',
                ))

            # DRAG
            pat_drag = _DRAG_JP if lang == 'jp' else _DRAG_EN
            for m in pat_drag.finditer(line):
                issues.append(ImmersionIssue(
                    issue_type='DRAG', line_no=i,
                    text_snippet=m.group()[:60], severity='warning',
                ))

            # EN standalone drag
            if lang == 'en':
                m = _DRAG_EN_STANDALONE.match(line)
                if m:
                    issues.append(ImmersionIssue(
                        issue_type='DRAG', line_no=i,
                        text_snippet=m.group()[:60], severity='warning',
                    ))

        return issues

    @staticmethod
    def _strip_metadata(text: str) -> str:
        """YAML frontmatter とコードブロックを除去"""
        # YAML frontmatter
        result = re.sub(r'^---\n.*?\n---\n', '', text, count=1, flags=re.DOTALL)
        # code blocks
        result = re.sub(r'```.*?```', '', result, flags=re.DOTALL)
        # scene separators
        result = re.sub(r'^---\s*$', '', result, flags=re.MULTILINE)
        return result
