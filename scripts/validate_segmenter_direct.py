#!/usr/bin/env python
"""
TextSegmenter 長文検証スクリプト（直接実行版）

小説・ウィキペディアなどの実テキストで TextSegmenter の動作を検証
"""

import sys
from pathlib import Path
import time
from typing import Dict
import numpy as np

# プロジェクトをパスに追加（モジュールから直接インポート）
sys.path.insert(0, str(Path(__file__).parent.parent))

# japhrase モジュールから直接インポート
from japhrase.segmenter import TextSegmenter


class LongTextValidator:
    """長文テキストでの動作検証"""
    
    def __init__(self):
        self.segmenter = TextSegmenter(window_size=4)
        self.results = []
    
    def load_sample_texts(self) -> Dict[str, str]:
        """サンプルテキストを取得"""
        texts = {}
        
        # 1. 例文データ（プロジェクト内の既存データ）
        example_file = Path(__file__).parent.parent / "examples" / "data" / "text.tsv"
        if example_file.exists():
            try:
                with open(example_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                    texts['examples_tsv'] = content[:5000]  # 最初の5000文字
            except Exception as e:
                print(f"⚠️  examples/data/text.tsv 読み込み失敗: {e}")
        
        # 2. 青空文庫サンプル（インラインで提供）
        aozora_sample = """
        こころは現代日本の代表的な長編小説である。作品は主人公の手紙の形式で書かれており、
        複雑な人間関係と心理描写が特徴である。明治時代の日本社会における心情の揺らぎと
        精神的危機が丁寧に描かれている。
        
        私は長年、ある先生を尊敬していた。その先生との関係が深まるにつれ、私の心の中に
        疑念が生じ始めた。先生の行動が本当に正しいのか、先生の言葉が本当の気持ちなのか。
        次第に信頼が揺らぎ始めた。
        
        最終的に先生は自殺を選んだ。その知らせは私に大きな衝撃を与えた。自分が何か
        できたのではないか、何か気づけたのではないか、そのような後悔の念に襲われた。
        人間関係の複雑さと心の奥底の闇が明らかになった。
        """
        texts['aozora_kokoro'] = aozora_sample.strip()
        
        # 3. 技術文書風テキスト（ウィキペディア風）
        wiki_sample = """
        機械学習は人工知能の一分野であり、コンピュータシステムがデータから学習し、
        明示的にプログラムされることなく改善される能力を持つ科学である。
        
        機械学習のアプローチは大きく3つに分類される。教師あり学習は正解ラベルが
        ついたデータを使って学習する方式である。教師なし学習はラベルなしの
        データから隠れた構造を発見する方式である。強化学習はエージェントが
        環境と相互作用しながら報酬を最大化するように学習する方式である。
        
        深層学習はニューラルネットワークの多層構造を利用する機械学習の分野である。
        画像認識や自然言語処理において顕著な成功を収めている。特にトランスフォーマー
        アーキテクチャの登場により、言語モデルの能力が飛躍的に向上した。
        
        自然言語処理は言語の意味と文脈を理解するプロセスである。単語埋め込みから
        始まり、BERTやGPTなどの大規模言語モデルへと進化してきた。これらモデルは
        テキスト分類、機械翻訳、質問応答など、様々なタスクで優れた性能を発揮している。
        """
        texts['wiki_ai'] = wiki_sample.strip()
        
        # 4. 長いテキスト（1000文字以上）
        long_sample = wiki_sample * 2  # 2倍にして長くする
        texts['wiki_ai_long'] = long_sample.strip()
        
        return texts
    
    def validate_text(self, text: str, text_name: str) -> Dict:
        """テキストの処理を検証"""
        print(f"\n{'='*70}")
        print(f"検証: {text_name}")
        print(f"{'='*70}")
        print(f"テキスト長: {len(text)} 文字")
        
        results = {
            'name': text_name,
            'length': len(text),
            'methods': {}
        }
        
        try:
            # 1. 閾値ベース分割
            print("\n[1] 閾値ベース分割 (threshold=0.5)")
            start = time.time()
            segments_threshold = self.segmenter.split_by_threshold(
                text, threshold=0.5, min_chunk_length=5
            )
            time_threshold = time.time() - start
            
            print(f"  ✅ 処理時間: {time_threshold:.3f}秒")
            print(f"  📊 分割数: {len(segments_threshold)}個")
            if segments_threshold:
                print(f"  📏 平均セグメント長: {np.mean([len(s) for s in segments_threshold]):.1f}文字")
                print(f"  📍 最小セグメント: {min([len(s) for s in segments_threshold])}文字")
                print(f"  📍 最大セグメント: {max([len(s) for s in segments_threshold])}文字")
                
                print("\n  最初の3セグメント:")
                for i, seg in enumerate(segments_threshold[:3]):
                    preview = seg[:50] + "..." if len(seg) > 50 else seg
                    print(f"    [{i}] {preview}")
            
            results['methods']['threshold'] = {
                'time': time_threshold,
                'segments': len(segments_threshold),
                'avg_length': float(np.mean([len(s) for s in segments_threshold])) if segments_threshold else 0,
            }
        
        except Exception as e:
            print(f"  ❌ エラー: {e}")
            import traceback
            traceback.print_exc()
            results['methods']['threshold'] = {'error': str(e)}
        
        try:
            # 2. Top-N分割
            print("\n[2] Top-N 分割 (n=3)")
            start = time.time()
            segments_topn = self.segmenter.split_top_n(
                text, n=3, min_chunk_length=10
            )
            time_topn = time.time() - start
            
            print(f"  ✅ 処理時間: {time_topn:.3f}秒")
            print(f"  📊 分割数: {len(segments_topn)}個")
            if segments_topn:
                print(f"  📏 平均セグメント長: {np.mean([len(s) for s in segments_topn]):.1f}文字")
                
                print("\n  各セグメント:")
                for i, seg in enumerate(segments_topn):
                    preview = seg[:40] + "..." if len(seg) > 40 else seg
                    print(f"    [{i}] ({len(seg)}文字) {preview}")
            
            results['methods']['topn'] = {
                'time': time_topn,
                'segments': len(segments_topn),
                'avg_length': float(np.mean([len(s) for s in segments_topn])) if segments_topn else 0,
            }
        
        except Exception as e:
            print(f"  ❌ エラー: {e}")
            import traceback
            traceback.print_exc()
            results['methods']['topn'] = {'error': str(e)}
        
        try:
            # 3. スマート分割
            print("\n[3] スマート分割 (適応的)")
            start = time.time()
            segments_smart = self.segmenter.smart_split(text)
            time_smart = time.time() - start
            
            print(f"  ✅ 処理時間: {time_smart:.3f}秒")
            print(f"  📊 分割数: {len(segments_smart)}個")
            if segments_smart:
                print(f"  📏 平均セグメント長: {np.mean([len(s) for s in segments_smart]):.1f}文字")
            
            results['methods']['smart'] = {
                'time': time_smart,
                'segments': len(segments_smart),
                'avg_length': float(np.mean([len(s) for s in segments_smart])) if segments_smart else 0,
            }
        
        except Exception as e:
            print(f"  ❌ エラー: {e}")
            import traceback
            traceback.print_exc()
            results['methods']['smart'] = {'error': str(e)}
        
        try:
            # 4. エントロピープロファイル分析
            print("\n[4] エントロピー分析")
            start = time.time()
            analysis = self.segmenter.analyze_entropy_profile(text)
            time_analysis = time.time() - start
            
            print(f"  ✅ 処理時間: {time_analysis:.3f}秒")
            if len(analysis['profile']) > 0:
                print(f"  📊 平均エントロピー: {analysis['mean']:.3f}")
                print(f"  📈 標準偏差: {analysis['std']:.3f}")
                print(f"  📉 範囲: {analysis['min']:.3f} - {analysis['max']:.3f}")
                print(f"  ⛰️ ピーク数: {len(analysis['peaks'])}個")
            
            results['methods']['analysis'] = {
                'time': time_analysis,
                'mean_entropy': float(analysis['mean']),
                'std_entropy': float(analysis['std']),
                'peaks': len(analysis['peaks']),
            }
        
        except Exception as e:
            print(f"  ❌ エラー: {e}")
            import traceback
            traceback.print_exc()
            results['methods']['analysis'] = {'error': str(e)}
        
        return results
    
    def run_all_tests(self):
        """全テキストで検証実行"""
        print("\n" + "="*70)
        print("TextSegmenter 長文検証テスト")
        print("="*70)
        
        texts = self.load_sample_texts()
        
        if not texts:
            print("⚠️  テキストが見つかりません")
            return
        
        print(f"\n利用可能なテキスト: {len(texts)}個")
        for name in texts.keys():
            print(f"  - {name}")
        
        # 各テキストで検証
        for text_name, text in texts.items():
            result = self.validate_text(text, text_name)
            self.results.append(result)
        
        # サマリー
        self.print_summary()
    
    def print_summary(self):
        """検証結果のサマリーを表示"""
        print("\n" + "="*70)
        print("📊 検証結果サマリー")
        print("="*70)
        
        for result in self.results:
            print(f"\n【{result['name']}】 (テキスト長: {result['length']} 文字)")
            
            for method, data in result['methods'].items():
                if 'error' in data:
                    print(f"  {method}: ❌ {data['error']}")
                else:
                    print(f"  {method}:")
                    print(f"    ⏱️  時間: {data['time']:.3f}秒")
                    print(f"    📊 分割数: {data['segments']}個")
                    if 'avg_length' in data and data['avg_length'] > 0:
                        print(f"    📏 平均セグメント長: {data['avg_length']:.1f}文字")
                    if 'mean_entropy' in data:
                        print(f"    🔢 平均エントロピー: {data['mean_entropy']:.3f}")


def main():
    """メイン実行"""
    validator = LongTextValidator()
    validator.run_all_tests()
    
    print("\n" + "="*70)
    print("✅ 検証完了")
    print("="*70)


if __name__ == '__main__':
    main()
