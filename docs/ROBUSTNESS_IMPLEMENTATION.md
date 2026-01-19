# 頑健性改善：完全実装ガイド

## 実装状況

### ✅ 完了した10の改善

|#|機能|ファイル|説明|
|---|---|---|---|
|⓵|共通utility層|`utils_robustness.py`|ScoreValidator, ConfigValidator, TextProcessor, ErrorHandler|
|⓶|JSON出力堅牢化|`utils_robustness.py`|safe_json_export() with CSV/TXT fallback|
|⓷|テキスト前処理統一|`utils_robustness.py`|TextProcessor class with 6 static methods|
|⓸|設定値バリデーション|各モジュール|ConfigValidator integration in __init__|
|⓹|重複検出・削除|`utils_deduplication.py`|DuplicateHandler with 5 core methods|
|⓺|コンテキスト分析|`utils_advanced.py`|ContextAnalyzer for section/genre analysis|
|⓻|位置情報信頼性|`utils_advanced.py`|PositionTracker for modification tracking|
|⓼|キャッシング機構|`utils_advanced.py`|CachingAnalyzer with LRU-like behavior|
|⓽|ストリーミング処理|`utils_advanced.py`|StreamingAnalyzer for large text handling|
|⓾|構造化ログ・メトリクス|`utils_advanced.py`|MetricsCollector for performance tracking|

---

## 実装詳細

### ⓵ 共通Utility層（utils_robustness.py）

```python
# スコア検証（ゼロ除算防止）
ScoreValidator.safe_divide(10, 0)  # → 0.5
ScoreValidator.validate_score(1.5)  # → (True, 1.0, 'スコアを1.0に修正')

# 設定値バリデーション
ConfigValidator.validate_sensitivity('invalid')  # → False
ConfigValidator.validate_chunk_size(-5)  # → 5（デフォルト）

# テキスト前処理
TextProcessor.normalize_whitespace("  多   スペース  ")  # → "多 スペース"

# エラーハンドリング
ErrorHandler.safe_json_export(result)  # JSON → CSV → TXT フォールバック
```

**統合されたモジュール:**
- `text_variant_detector.py`: スコア正規化
- `ending_heatmap.py`: JSON出力、chunk_size検証
- `subject_pov_detector.py`: POV計算、sensitivity検証

---

### ⓶ JSON出力の堅牢化

```python
# 失敗パターンも安全に処理
result = ErrorHandler.safe_json_export(
    large_result_with_unicode,
    fallback_dir='./fallback'
)
# JSON失敗 → CSV出力 → TXT出力
```

---

### ⓷ テキスト前処理統一

```python
# 6つのユーティリティメソッド
TextProcessor.validate_text(text)  # None, empty, encoding check
TextProcessor.normalize_whitespace(text)  # スペース正規化
TextProcessor.safe_substring(text, start, end)  # 位置チェック付き
TextProcessor.calculate_byte_position(text, char_pos)  # バイト位置計算
TextProcessor.calculate_char_position(text, byte_pos)  # 文字位置計算
TextProcessor.encode_safely(text, encoding='utf-8')  # エンコード安全化
```

---

### ⓸ 設定値バリデーション層

各クラスの`__init__`で実装：

```python
# ending_heatmap.py
def __init__(self, chunk_size: int = 5):
    self.chunk_size = ConfigValidator.validate_chunk_size(chunk_size)
    
# subject_pov_detector.py
def __init__(self, sensitivity: str = 'medium'):
    self.sensitivity = ConfigValidator.validate_sensitivity(sensitivity)
```

**検証内容:**
- chunk_size: 正の整数、デフォルト5
- sensitivity: {'low', 'medium', 'high'}、デフォルト'medium'
- threshold: 0.0-1.0の浮動小数点数

---

### ⓹ 重複検出・削除（utils_deduplication.py）

```python
# 重複候補の統合
DuplicateHandler.deduplicate_candidates(
    candidates,
    key_func=lambda c: c['original'],
    merge_func=lambda c1, c2: {...}
)

# スコア矛盾検出
conflicts = DuplicateHandler.detect_score_conflicts(candidates)
```

---

### ⓺ コンテキスト分析（utils_advanced.py）

```python
analyzer = ContextAnalyzer()

# セクション単位の統計
baseline = analyzer.analyze_section_baseline(
    text,
    section_markers=['##', '【', '章']
)

# ジャンル推定
genre = analyzer.detect_genre_pattern(text)
# → 'technical', 'narrative', 'academic', 'general'
```

---

### ⓻ 位置情報の信頼性（utils_advanced.py）

```python
tracker = PositionTracker()

# 修正を記録
new_pos = tracker.record_modification(
    (10, 20),
    "新しいテキスト",
    5
)

# 位置検証
is_valid = tracker.validate_positions(text, positions)

# 修正履歴取得
history = tracker.get_modification_history()
```

---

### ⓼ キャッシング機構

```python
cache = CachingAnalyzer(cache_size=256)

# キャッシュ保存
cache.set_cache(text, 'analyze', result)

# キャッシュ取得
result = cache.get_cached(text, 'analyze')

# 統計情報
stats = cache.get_cache_stats()
# {'cache_size': 5, 'max_size': 256, 'hit_rate': 0.8}
```

**統合:**
- `text_variant_detector.py`: 256サイズのキャッシュ
- `ending_heatmap.py`: 分析結果キャッシング
- `sentence_variation.py`: 変換パターンキャッシング

---

### ⓽ ストリーミング処理

```python
analyzer = StreamingAnalyzer(chunk_size=5000)

# 大きなテキストを効率的に処理
chunks = analyzer.stream_text_chunks(text)
# 各チャンク: {'text', 'start_pos', 'sentence_count', ...}

for chunk in chunks:
    # チャンク単位で処理
    result = process_chunk(chunk['text'])
```

**利点:**
- メモリ効率向上
- 大規模テキストのタイムアウト防止
- 段階的な結果出力可能

---

### ⓾ 構造化ログ・メトリクス

```python
metrics = MetricsCollector()

# 時間計測
metrics.start_timer('analysis')
# ... 処理 ...
elapsed = metrics.end_timer('analysis')

# スコア計算の根拠記録
metrics.log_score_calculation(
    'analyze_variants',
    'composite_score',
    0.85,
    'frequency-based calculation'
)

# メトリクス統計
stats = metrics.get_metrics()
# {'operation': {'count': 10, 'avg_time': 0.025, ...}}

# ファイル出力
metrics.export_metrics('metrics.json')
```

---

## テスト結果

```
======================== test session starts ========================
tests/test_advanced_features.py::test_context_analysis PASSED   [ 20%]
tests/test_advanced_features.py::test_position_tracking PASSED  [ 40%]
tests/test_advanced_features.py::test_caching PASSED            [ 60%]
tests/test_advanced_features.py::test_streaming PASSED          [ 80%]
tests/test_advanced_features.py::test_metrics PASSED            [100%]

========================= 5 passed in 3.54s =========================
```

---

## 各モジュールへの統合ポイント

### text_variant_detector.py
- ✅ キャッシング（analyze()の結果をキャッシュ）
- ✅ メトリクス（処理時間計測）
- ✅ コンテキスト分析（セクション単位での比較）
- ✅ スコア検証（_composite_score()の正規化）

### ending_heatmap.py
- ✅ キャッシング（分析結果キャッシング）
- ✅ 位置情報（チャンクの位置追跡）
- ✅ メトリクス（ヒートマップ生成時間計測）
- ✅ 設定値バリデーション（chunk_size検証）

### subject_pov_detector.py
- ✅ メトリクス（POV計算の根拠記録）
- ✅ 位置情報（発見位置の検証）
- ✅ 設定値バリデーション（sensitivity検証）
- ✅ スコア検証（_calculate_pov_dominance()の安全計算）

### sentence_variation.py
- ✅ キャッシング（変換パターンキャッシング）
- ✅ ストリーミング（大規模テキスト対応）
- ✅ メトリクス（処理統計記録）
- ✅ 重複検出（バリエーション生成時の重複排除）

### teniwoha_lint.py
- ✅ ストリーミング（大規模テキスト対応）
- ✅ メトリクス（チェック統計記録）
- ✅ テキスト前処理（正規化前の検証）

---

## 使用例：統合的な活用

```python
from japhrase import (
    TextVariantDetector,
    EndingHeatmapGenerator,
    SubjectPOVDetector,
    SentenceVariationGenerator,
    TeniwohaLinter
)
from japhrase.utils_advanced import MetricsCollector, StreamingAnalyzer

# メトリクス収集を開始
metrics = MetricsCollector()
metrics.start_timer('full_analysis')

# 大規模テキストをストリーム処理
streaming = StreamingAnalyzer(chunk_size=10000)
chunks = streaming.stream_text_chunks(large_text)

results = []
for chunk in chunks:
    # 各チャンクを分析
    detector = TextVariantDetector()
    heatmap_gen = EndingHeatmapGenerator()
    pov_checker = SubjectPOVDetector()
    
    results.append({
        'variants': detector.analyze(chunk['text']),
        'heatmap': heatmap_gen.analyze(chunk['text']),
        'pov_issues': pov_checker.check(chunk['text'])
    })

# メトリクス出力
elapsed = metrics.end_timer('full_analysis')
metrics.export_metrics('analysis_metrics.json')

print(f"分析完了: {elapsed:.2f}秒、{len(chunks)}チャンク")
```

---

## 今後の拡張ポイント

1. **分散キャッシング**: Redis連携でマルチプロセス対応
2. **非同期処理**: async/awaitでのストリーミング高速化
3. **詳細メトリクス**: Prometheusフォーマットでの監視
4. **カスタム検証ルール**: ConfigValidator の拡張
5. **グラフベース解析**: 複数モジュール間の結果統合

---

## よくある質問（FAQ）

### Q: キャッシュサイズはどう決める？
**A:** テキスト処理量に応じて：
- 小規模（<10文書）: 32-64
- 中規模（10-100文書）: 128-256
- 大規模（>100文書）: 256-512

### Q: ストリーミングはいつ使う？
**A:** テキスト長が3MB以上の場合、またはメモリ制約がある環境で使用。

### Q: 位置情報はどう検証する？
**A:** `validate_positions(text, positions)` で事前検証し、修正後は `record_modification()` で追跡。

### Q: メトリクスはオーバーヘッドにならない？
**A:** logging.debugレベルなので、本番環境ではレベルを上げてdisable可能。

---

## まとめ

✅ **エラー耐性**: 全計算がゼロ除算、型エラーから保護
✅ **スケーラビリティ**: ストリーミングとキャッシングで大規模テキスト対応
✅ **可視性**: メトリクスとログで処理状況の完全追跡
✅ **保守性**: 共通utility層で重複コード排除
✅ **信頼性**: 位置情報追跡で修正の正確性確保

10個すべての改善が実装され、すべてのテストが成功しました！
