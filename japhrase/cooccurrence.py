"""
共起語分析モジュール

特定のキーワード（ターゲット）の周辺に出現するフレーズを分析し、
そのターゲットと統計的に結びつきの強い特徴語（共起語）を抽出する。
"""

import logging
from typing import List, Dict, Tuple, Optional, Union
import pandas as pd
import numpy as np

from .extracter import PhraseExtracter

logger = logging.getLogger(__name__)


class CooccurrenceAnalyzer:
    """
    共起語分析クラス
    
    ターゲット語の周辺コンテキストを抽出し、全体コーパスと比較することで、
    「その言葉の周りにだけよく現れるフレーズ（特徴語）」を特定する。
    """

    def __init__(
        self,
        extractor: Optional[PhraseExtracter] = None,
        window_size: int = 50,
        min_cooccurrence: int = 3
    ):
        """
        Args:
            extractor: PhraseExtracterインスタンス（Noneならデフォルト生成）
            window_size: ターゲットの前後の抽出文字数（片側の半径）
            min_cooccurrence: 分析対象とする最小共起回数
        """
        # 共起分析では、あまりにレアな語はノイズになるためデフォルトで足切りする
        self.extractor = extractor or PhraseExtracter(min_count=min_cooccurrence)
        self.window_size = window_size
        self.min_cooccurrence = min_cooccurrence

    def extract_context(self, text: str, target: str) -> List[str]:
        """
        テキストからターゲット語の周辺コンテキストを抽出する
        
        Args:
            text: 全文テキスト
            target: ターゲット語
            
        Returns:
            List[str]: コンテキスト（断片）のリスト
        """
        if not target:
            raise ValueError("target は空文字列にできません")

        spans = []
        start = 0
        target_len = len(target)
        text_len = len(text)

        while True:
            # ターゲットを検索
            idx = text.find(target, start)
            if idx == -1:
                break

            # ウィンドウ範囲を決定（テキストの範囲外に出ないようにクリップ）
            left = max(0, idx - self.window_size)
            right = min(text_len, idx + target_len + self.window_size)
            spans.append((left, right))

            # 次の検索開始位置
            start = idx + target_len

        # ターゲットが近距離で連続する場合、ウィンドウが重複しうる。
        # 重複したまま別々のコンテキストとして数えると、同じ原文断片が
        # 何度もコピーされ、共起頻度が実際より水増しされてしまうため、
        # 重複・隣接するウィンドウはマージしてから切り出す。
        spans.sort()
        merged_spans = []
        for left, right in spans:
            if merged_spans and left <= merged_spans[-1][1]:
                merged_spans[-1] = (merged_spans[-1][0], max(merged_spans[-1][1], right))
            else:
                merged_spans.append((left, right))

        # 文脈を維持するため、改行などもそのまま含める
        contexts = [text[left:right] for left, right in merged_spans if right > left]

        return contexts

    def analyze(
        self, 
        text: str, 
        target_word: str, 
        top_n: int = 20,
        include_target: bool = False
    ) -> pd.DataFrame:
        """
        ターゲット語の共起分析を実行する

        Args:
            text: 分析対象の全文
            target_word: 分析したい単語（キャラ名、製品名など）
            top_n: 結果の件数
            include_target: 結果にターゲット語自体を含めるか（通常はFalse）

        Returns:
            DataFrame: 特徴フレーズと各種スコア（freq, lift, score）
        """
        logger.info(f"Analyzing co-occurrence for '{target_word}' (Window: +/-{self.window_size})...")

        # 1. コンテキスト抽出
        contexts = self.extract_context(text, target_word)
        if not contexts:
            logger.warning(f"Target word '{target_word}' not found in text.")
            return pd.DataFrame()
            
        logger.info(f"Found {len(contexts)} occurrences.")

        # 2. 全体コーパスでの頻度分析（ベースライン確率の計算）
        # ※本来は巨大なコーパスなら事前に計算しておくべきだが、
        #   ここでは汎用性重視で都度計算する（PhraseExtracterが高速なので許容）
        df_global = self.extractor.extract([text])
        if df_global.empty:
            return pd.DataFrame()

        total_freq_global = df_global['freq'].sum()
        # {phrase: probability} の辞書を作成
        global_probs = dict(zip(
            df_global['seqchar'], 
            df_global['freq'] / total_freq_global
        ))

        # 3. 周辺（ローカル）コーパスでの頻度分析
        df_local = self.extractor.extract(contexts)
        if df_local.empty:
            return pd.DataFrame()

        total_freq_local = df_local['freq'].sum()
        
        results = []
        for _, row in df_local.iterrows():
            phrase = row['seqchar']
            freq_local = row['freq']
            
            # ターゲット語自体の除外処理
            if not include_target and phrase == target_word:
                continue
            # ターゲット語を含むフレーズ（包含関係）もノイズになりやすいので除外
            if not include_target and target_word in phrase:
                continue
                
            # 確率計算
            prob_local = freq_local / total_freq_local
            prob_global = global_probs.get(phrase, 0.000001)  # ゼロ除算防止
            
            # Lift値: 「周辺」での出現率が「全体」より何倍高いか
            # Lift > 1.0 なら共起傾向あり
            lift = prob_local / prob_global
            
            # スコアリング:
            # 単純なLift値だと、低頻度語（たまたま1回出ただけ）が過大評価されるため、
            # 頻度の対数を掛けて補正する（Significance Score）
            score = np.log2(lift) * np.log(freq_local + 1) if lift > 0 else 0

            results.append({
                'phrase': phrase,
                'freq': int(freq_local),
                'lift': round(lift, 2),
                'score': round(score, 4)
            })

        # 4. 結果の整形とソート
        df_result = pd.DataFrame(results)
        if not df_result.empty:
            # スコア（重要度）順にソート
            df_result = df_result.sort_values('score', ascending=False).head(top_n)
            df_result = df_result.reset_index(drop=True)

        return df_result
