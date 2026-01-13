# japhrase UX/DX改善案

## 現状分析

### 現在の機能体系（11個）

```
┌─ 基盤層 ─────────────────────────────┐
│ • PhraseExtracter                    │
│ • SimilarityAnalyzer                 │
└──────────────────────────────────────┘
         ↓
┌─ 分析層 ─────────────────────────────┐
│ • Evaluator (教師ありなし)           │
│ • Optimizer (教師ありなし)           │
└──────────────────────────────────────┘
         ↓
┌─ 応用層 ─────────────────────────────┐
│ • KWICAnalyzer                       │
│ • AbstractBodyChecker                │
│ • HabitDetector                      │
│ • RevisionHeatmap                    │
│ • RankingTrajectory                  │
│ • EditorConfigGenerator              │
│ • SelfRecommender                    │
└──────────────────────────────────────┘
```

### 現在のUX/DXの課題

1. **実行方法が分散**
   - Pythonスクリプトでしか実行できない
   - ユースケース別のクイックスタートがない
   - 複数機能の組み合わせ方が不明確

2. **コンテキストが不足**
   - ユーザーは「何をしたいのか」から「どの機能を使うか」を判断する必要がある
   - ユースケース駆動の設計ガイドがない

3. **出力形式の多様性**
   - DataFrameで返すもの、テキストレポート、JSONなど混在
   - 統一されたビジュアライゼーション方法がない

4. **設定の複雑さ**
   - 毎回パラメータを指定する必要がある
   - 用途別プリセット（SNS、論文など）が限定的

5. **バッチ処理の非効率性**
   - 複数ファイルの一括処理に対応していない
   - パイプライン化ができない

---

## 提案するUX/DX改善方案

### 方案1: CLI ツール化（優先度：高）

**目的**: Pythonコード不要で、コマンドラインから直接実行可能に

```bash
# 基本的なフレーズ抽出
$ japhrase extract --file text.txt --preset news

# KWIC検索
$ japhrase kwic --file text.txt --phrase "機械学習"

# あらすじチェック
$ japhrase check-divergence --abstract abstract.txt --body body.txt

# 口癖検出
$ japhrase detect-habits --file text.txt --reference corpus/

# 完全なレポート生成
$ japhrase analyze --file text.txt --output report.html

# バッチ処理
$ japhrase batch --config analysis.yaml --input-dir ./texts/ --output-dir ./reports/
```

**メリット**:
- Pythonの知識不要
- スクリプト化・自動化が容易
- エディタ/IDE との統合が可能

**必要な実装**:
- CLIフレームワーク（click/typer）
- コマンド群（12-15個）
- 引数バリデーション
- エラーハンドリング

---

### 方案2: ワークフロー/パイプラインエンジン（優先度：高）

**目的**: 複数タスクを定義して連鎖実行。設定ファイルベース

```yaml
# workflow.yaml
name: "完全原稿品質チェック"
description: "あらすじ、本文、過去記事の包括的分析"

tasks:
  - id: extract_abstract
    type: extract
    input: abstract.txt
    params:
      preset: default
      min_count: 3

  - id: extract_body
    type: extract
    input: body.txt
    params:
      preset: default
      min_count: 5

  - id: check_divergence
    type: abstract_body_checker
    depends_on: [extract_abstract, extract_body]

  - id: detect_habits
    type: habit_detector
    depends_on: extract_body
    params:
      z_score_threshold: 2.0

  - id: search_past
    type: self_recommender
    depends_on: extract_body
    input_dir: past_articles/
    params:
      top_n: 5

  - id: generate_report
    type: report_generator
    depends_on: [check_divergence, detect_habits, search_past]
    output: report.html
    format: html
```

**実行**:
```bash
$ japhrase workflow --config workflow.yaml --parallel
```

**メリット**:
- 複雑な分析フローを視覚的に定義
- 再利用可能な分析テンプレート
- 依存性の自動解決
- 並列実行サポート

**必要な実装**:
- DAGエンジン（taskgraphなど）
- YAML/JSON パーサー
- タスク依存性解決
- 進捗表示・ログ管理

---

### 方案3: Webダッシュボード/UI（優先度：中）

**目的**: ブラウザベースで分析・ビジュアライゼーション

```
┌─────────────────────────────────────────┐
│ japhrase Dashboard                      │
├─────────────────────────────────────────┤
│                                         │
│ 📊 最近の分析                           │
│ ┌─────────────────────────────────────┐ │
│ │ • 2024-01-05: novel_analysis        │ │
│ │ • 2024-01-04: divergence_check      │ │
│ └─────────────────────────────────────┘ │
│                                         │
│ 🔍 新規分析                             │
│ ┌─────────────────────────────────────┐ │
│ │ テキストファイルをアップロード      │ │
│ │ ┌─────────────────────────────────┐ │ │
│ │ │ [ファイル選択]                  │ │ │
│ │ └─────────────────────────────────┘ │ │
│ │                                     │ │
│ │ 分析タイプを選択:                   │ │
│ │ ○ フレーズ抽出                      │ │
│ │ ○ KWIC検索                          │ │
│ │ ○ あらすじチェック                  │ │
│ │ ○ 口癖検出                          │ │
│ │ ○ セルフリコメンデーション          │ │
│ │ ○ カスタムワークフロー              │ │
│ │                                     │ │
│ │ [分析開始]                          │ │
│ └─────────────────────────────────────┘ │
│                                         │
└─────────────────────────────────────────┘
```

**機能**:
- ファイルアップロード/ドラッグ&ドロップ
- リアルタイム分析実行
- インタラクティブなパラメータ調整
- ヒートマップ、グラフ表示
- 結果エクスポート（CSV/JSON/HTML）
- 分析履歴管理
- 比較ビュー

**必要な実装**:
- Webフレームワーク（FastAPI + React/Vue）
- リアルタイム通信（WebSocket）
- グラフ/チャートライブラリ（Plotly等）
- セッション管理

---

### 方案4: ユースケース駆動の統合インターフェース（優先度：高）

**目的**: ユースケースから逆算して、最適な機能組み合わせを自動提案

```python
from japhrase import WritingWorkflow

# ユースケース1: 「長い論文を執筆中。品質チェックしたい」
workflow = WritingWorkflow.for_use_case('academic_writing')
report = workflow.run(
    abstract_file='abstract.txt',
    body_file='chapter1.txt',
    past_corpus_dir='past_papers/',
)
print(report)

# ユースケース2: 「小説の推敲。文体のバランスを確認したい」
workflow = WritingWorkflow.for_use_case('novel_revision')
report = workflow.run(
    v1='draft1.txt',
    v2='draft2.txt',
    v3='draft3.txt',
)

# ユースケース3: 「SNS記事作成。表記ゆれを修正したい」
workflow = WritingWorkflow.for_use_case('sns_content')
config = workflow.generate_editor_config(
    texts=['post1.txt', 'post2.txt', 'post3.txt'],
    output_format='vscode'
)
```

**プリセット例**:
- `academic_writing` - 論文執筆向け（あらすじチェック + 口癖検出 + 過去論文検索）
- `novel_revision` - 小説推敲向け（ランキング推移 + 推敲偏り + KWIC）
- `blog_writing` - ブログ執筆向け（フレーズ抽出 + 表記ゆれ + 推敲偏り）
- `sns_content` - SNS向け（表記ゆれ + 短いフレーズ抽出）
- `editing` - 編集者向け（乖離検出 + 口癖 + 過去記事検索）

**必要な実装**:
- ユースケースデータベース
- メタ情報スキーマ
- プリセット管理
- 動的ワークフロー構築

---

### 方案5: 設定ファイルシステム（優先度：中）

**目的**: ユーザー/プロジェクト固有の設定を一元管理

```yaml
# .japhrase.yml
project: my_novel

presets:
  quick_check:
    description: "高速品質チェック"
    min_count: 5
    max_length: 20
    similarity_threshold: 0.6

  detailed_analysis:
    description: "詳細分析"
    min_count: 2
    max_length: 16
    include_habits: true
    include_divergence: true

editor_config:
  type: vscode
  auto_generate: true
  threshold: 0.75

corpus:
  paths:
    - past_articles/
    - archived_posts/
  exclude_patterns:
    - "draft_*"
    - "*.bak"

output:
  format: html
  style: professional
  include_charts: true
```

**使用**:
```bash
$ japhrase analyze --config .japhrase.yml --preset quick_check
```

**メリット**:
- プロジェクト単位での設定管理
- チーム間での設定共有
- バージョン管理との統合
- IDE/エディタとの統合

---

### 方案6: エディタプラグイン（優先度：中）

**目的**: VS Code等で直接分析・表記ゆれ修正

```
VS Code Extensions:
├─ japhrase-analyzer
│  └─ コマンドパレット統合
│     ├─ Extract Phrases (現在の選択テキスト)
│     ├─ Check Divergence (複数ファイル選択)
│     ├─ Detect Habits
│     └─ Generate Report
│
├─ japhrase-editor-config
│  └─ 自動表記ゆれ検出・波線表示
│
└─ japhrase-recommendations
   └─ 共通フレーズ提案・過去記事リンク
```

**サイドバーパネル**:
```
📝 japhrase Analyzer
─────────────────────
📊 Statistics
  フレーズ数: 245
  ユニーク度: 72%
  平均出現回数: 3.2

🔍 Recent Analysis
  • Habits: 12件検出
  • Divergence: 高
  • Related Articles: 5件

⚙️ Quick Actions
  [分析開始] [設定] [履歴]
```

---

### 方案7: バッチ処理・スケーラビリティ（優先度：中）

**目的**: 大量ファイルの効率的な処理

```bash
# 1000個のファイルを並列処理
$ japhrase batch --input-dir ./texts/ \
                  --output-dir ./reports/ \
                  --workers 8 \
                  --workflow batch_analysis.yaml

# 進捗表示:
# Processing: [████████░░░░░░░░░░] 45% (450/1000 files)
# Completed: 450
# Failed: 2
# Estimated time: 3m 22s
```

**機能**:
- マルチプロセッシング
- エラーリカバリー（失敗ファイルの自動リトライ）
- 中断・再開機能
- 結果の自動集約

---

### 方案8: プラグインシステム（優先度：低）

**目的**: ユーザー独自のカスタム分析モジュール追加

```python
# my_plugins/custom_analyzer.py
from japhrase.plugin import AnalyzerPlugin

class KeywordTrendAnalyzer(AnalyzerPlugin):
    """キーワードのトレンド分析プラグイン"""

    name = "keyword_trends"
    version = "1.0.0"

    def analyze(self, phrases_df, **kwargs):
        # カスタム分析ロジック
        return {
            'trending_up': [...],
            'trending_down': [...],
            'stable': [...]
        }

# .japhrase.yml
plugins:
  - my_plugins.custom_analyzer:KeywordTrendAnalyzer
```

---

## 実装ロードマップ（推奨順）

### Phase 1: CLIツール（1-2週間）
- [ ] Click/TyperベースのCLI実装
- [ ] 基本コマンド（extract, kwic, check-divergence等）
- [ ] ヘルプドキュメント

### Phase 2: ワークフローエンジン（2-3週間）
- [ ] YAML/JSON パーサー
- [ ] DAGエンジン
- [ ] タスク依存性解決
- [ ] ワークフロー実装例

### Phase 3: ユースケース駆動インターフェース（1-2週間）
- [ ] WritingWorkflow クラス設計
- [ ] プリセット定義（5-6個）
- [ ] 自動ワークフロー生成

### Phase 4: 設定ファイルシステム（1週間）
- [ ] YAML スキーマ定義
- [ ] 設定ファイルローダー
- [ ] バリデーション

### Phase 5: Webダッシュボード（3-4週間）
- [ ] FastAPI バックエンド
- [ ] React/Vue フロントエンド
- [ ] リアルタイム通信
- [ ] ビジュアライゼーション

### Phase 6: エディタプラグイン（2-3週間）
- [ ] VS Code Extension実装
- [ ] LSP統合
- [ ] コマンドパレット統合

### Phase 7: バッチ処理（1-2週間）
- [ ] マルチプロセッシング実装
- [ ] エラーハンドリング
- [ ] 進捗表示

---

## 優先度マトリックス

```
         高影響度
             ↑
     ┌──────┼──────┐
  高 │      │      │
     │ CLI  │WSF   │ CLI+WSF は
難度 │      │      │ユーザー満足度が
     │      │      │ 最も高い
     ├──────┼──────┤
  低 │ 設定 │ Web  │
     │      │      │
     └──────┼──────┘
     低影響度
```

**最優先**: CLI + ワークフローエンジン + ユースケース化
（この3つで70%の改善効果）

---

## まとめ

| 方案 | 目的 | 優先度 | 工数 | 効果 |
|------|------|--------|------|------|
| CLI | コマンド実行化 | 🔴 高 | 1-2w | 🟢🟢🟢 |
| Workflow | パイプライン化 | 🔴 高 | 2-3w | 🟢🟢🟢 |
| ユースケース | 分かりやすさ | 🔴 高 | 1-2w | 🟢🟢🟢 |
| 設定ファイル | 再利用性 | 🟡 中 | 1w | 🟢🟢 |
| Webダッシュボード | ビジュアル化 | 🟡 中 | 3-4w | 🟢🟢 |
| エディタプラグイン | IDE統合 | 🟡 中 | 2-3w | 🟢🟢 |
| バッチ処理 | スケール | 🟡 中 | 1-2w | 🟢 |
| プラグイン | 拡張性 | 🟢 低 | 1-2w | 🟢 |

**推奨実装順**: CLI → Workflow → ユースケース化 → 設定管理 → Web UI
