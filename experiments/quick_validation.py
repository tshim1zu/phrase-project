"""
Quick PMI・分岐エントロピー効果検証スクリプト（小規模版）
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

from japhrase import PhraseExtracter
import pandas as pd

# 小規模なサンプルテキスト（Wikipedia最初の数段落のみ）
text = """
人工知能（じんこうちのう、英: artificial intelligence）、AI（エーアイ）は、「『計算（computation）』という概念と『コンピュータ（computer）』という道具を用いて『知能』を研究する計算機科学（computer science）の一分野」を指す語。
「言語の理解や推論、問題解決などの知的行動を人間に代わってコンピュータに行わせる技術」、または、「計算機（コンピュータ）による知的な情報処理システムの設計や実現に関する研究分野」ともされる。

AIの研究開発は「人工知能学」とも呼ばれる。
AIに関する大学での研究や教育は「電気工学・コンピュータ科学部 人工知能・意思決定論科」、情報工学科や情報理工学科コンピュータ科学専攻などで行われている。

人間の知的能力をコンピュータ上で実現する、様々な技術・ソフトウェア群・コンピュータシステム、アルゴリズムとも言われる（知的エージェントも参照）。
人工知能の例は、人間の日常的な言語を扱う自然言語処理（機械翻訳・かな漢字変換・構文解析・大規模言語モデル等）、専門家の推論や判断を模倣するエキスパートシステム、画像のパターンを検出や抽出する画像認識等がある。

機械学習や深層学習は人工知能の重要な技術である。
ニューラルネットワークは深層学習の基礎となる技術である。
Pythonは機械学習の開発によく使われるプログラミング言語である。
"""

sentences = [line.strip() for line in text.split('\n') if line.strip()]

print("=" * 60)
print("PMI・分岐エントロピー 効果検証（クイック版）")
print("=" * 60)

configs = [
    ('ベースライン', {'use_pmi': False, 'use_branching_entropy': False}),
    ('PMI有効', {'use_pmi': True, 'use_branching_entropy': False}),
    ('BE有効', {'use_pmi': False, 'use_branching_entropy': True}),
    ('PMI+BE', {'use_pmi': True, 'use_branching_entropy': True}),
]

for name, params in configs:
    print(f"\n【{name}】")
    print("-" * 40)

    extractor = PhraseExtracter(
        min_count=2,
        max_length=16,
        **params,
        verbose=0
    )

    df = extractor.extract(sentences)

    if len(df) > 0:
        print(f"抽出フレーズ数: {len(df)}")
        print(f"データフレームのカラム: {list(df.columns)}")
        print("\n上位10フレーズ:")
        for idx in range(min(10, len(df))):
            row = df.iloc[idx]
            phrase = row['seqchar']
            freq = int(row.get('freq', 0))
            score = float(row.get('sc_index', 0))  # sc_index is the score column
            print(f"  {idx+1:2d}. {phrase:20s} (頻度:{freq:3d}, スコア:{score:8.2f})")
    else:
        print("フレーズが抽出されませんでした")

print("\n" + "=" * 60)
print("検証完了！")
print("=" * 60)
