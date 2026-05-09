#!/usr/bin/env python3
# coding: utf-8
"""
統合執筆支援デモ

Phase 1-2 の新機能を統合して使用する例
"""

import sys
import io
sys.path.insert(0, '../')

# UTF-8 出力対応
if sys.stdout.encoding != 'utf-8':
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')

from japhrase import (
    TextVariantDetector,
    EndingHeatmapGenerator,
    SubjectPOVDetector,
    SentenceVariationGenerator,
    TeniwohaLinter
)


def main():
    # サンプルテキスト
    text = """
    彼は毎日走った。彼は毎日走った。彼は毎日走った。
    私は公園にいた。私は公園にいた。
    
    その時、彼が私を呼んだ。彼が走ってきた。彼が笑った。
    あの人はどこへ行ったのか。その人は何をしていたのか。
    
    文末がだったで始まった。これもだったである。
    そしてそれもだっただった。また別のもだっただった。
    
    彼女は彼は彼女がを愛していた。
    """

    print("=" * 60)
    print("統合執筆支援システム デモンストレーション")
    print("=" * 60)

    # 1. てにをはLint
    print("\n【1】てにをはLint - 助詞の異常検出")
    print("-" * 60)
    linter = TeniwohaLinter(strict_mode=False)
    issues = linter.check(text)
    print(f"検出：{len(issues)}件の問題")
    if issues:
        print(linter.format_issues(issues, text)[:500])

    # 2. 表記ゆれ検出（推奨表記辞書デモ）
    print("\n【2】表記ゆれ検出 - 推奨表記辞書機能")
    print("-" * 60)
    detector = TextVariantDetector(similarity_threshold=0.7)
    
    # 推奨表記辞書を設定（デモ）
    detector.preferred_dictionary = {
        'だった': {
            'preferred': 'であった',
            'reason': 'より格調高い表記'
        }
    }
    print(f"推奨表記辞書：{len(detector.preferred_dictionary)}件登録")
    print(f"  - {list(detector.preferred_dictionary.items())[0]}")

    # 3. 文末表現ヒートマップ
    print("\n【3】冗長語尾のヒートマップ")
    print("-" * 60)
    heatmap_gen = EndingHeatmapGenerator(chunk_size=3)
    analysis = heatmap_gen.analyze(text)
    print(heatmap_gen.format_heatmap(analysis)[:600])
    
    # JSON出力デモ
    improvements = heatmap_gen.suggest_improvements(analysis, top_n=2)
    if improvements:
        print(f"\n改善提案：{len(improvements)}件")
        for imp in improvements[:1]:
            print(f"  - {imp['suggestion']}")

    # 4. 主語/視点ブレ検出
    print("\n【4】主語/視点ブレ検出")
    print("-" * 60)
    pov_detector = SubjectPOVDetector(sensitivity='medium')
    pov_issues = pov_detector.check(text)
    print(f"検出：{len(pov_issues)}件の問題")
    
    # 統計情報
    stats = pov_detector.get_statistics(text)
    print(f"視点の支配度スコア：{stats['dominant_pov_ratio']:.2%}")
    print(f"主語の安定性スコア：{stats['subject_stability_score']:.2%}")
    
    # 改善提案
    suggestions = pov_detector.suggest_improvements(text)
    if suggestions:
        print(f"改善提案：{len(suggestions)}件")
        for sugg in suggestions[:1]:
            print(f"  - {sugg['problem']}")
            if sugg['suggestions']:
                print(f"    例: {sugg['suggestions'][0]}")

    # 5. 同文反復検出とバリエーション生成
    print("\n【5】同文反復検出 - 自動バリエーション生成")
    print("-" * 60)
    var_gen = SentenceVariationGenerator(similarity_threshold=0.95, 
                                        min_repetitions=2)
    repetitions = var_gen.detect_repetitions(text)
    print(f"検出：{len(repetitions)}件の繰り返し")
    
    if repetitions:
        rep = repetitions[0]
        print(f"\n最も繰り返されている文（{rep['count']}回）:")
        print(f"  元の文：{rep['sentence']}")
        print(f"  バリエーション候補：")
        for i, var in enumerate(rep['variations'][:2], 1):
            print(f"    {i}. {var['text']}")
            print(f"       ({var['description']})")
        
        # 修正案の自動適用デモ
        print(f"\n  → 修正テキスト生成（最初の候補を自動適用）")
        corrected = var_gen.generate_correction_text(text, 
                                                    repetitions,
                                                    apply_all=True)
        print(f"     修正済みテキスト長：{len(corrected)} 文字")

    # 6. JSON出力デモ
    print("\n【6】JSON形式での出力")
    print("-" * 60)
    
    # 文末表現ヒートマップをJSON出力
    heatmap_gen.export_analysis_json(analysis, 
                                     'analysis_ending_heatmap.json',
                                     include_issues=True)
    print("✓ analysis_ending_heatmap.json に出力")
    
    # 主語/視点ブレをJSON出力
    pov_detector.export_issues_json(pov_issues, 
                                    'analysis_pov_issues.json')
    print("✓ analysis_pov_issues.json に出力")
    
    # 繰り返しをJSON出力
    var_gen.export_repetitions_json(repetitions, 
                                   'analysis_repetitions.json')
    print("✓ analysis_repetitions.json に出力")

    print("\n" + "=" * 60)
    print("デモンストレーション完了")
    print("=" * 60)


if __name__ == '__main__':
    main()
