# japhrase アーキテクチャ改善 (Phase 1-3 完了)

本ドキュメントは、japhrase v0.1.3で実装された3フェーズのアーキテクチャ改善について説明します。

## 概要

japhrase は単なる日本語フレーズ抽出ライブラリから、**執筆支援のための統合プラットフォーム**へと進化しました。

| Phase | 成果 | 対象ユーザー |
|-------|------|-----------|
| Phase 1: CLI | コマンドラインツール化 | 開発者・スクリプト利用者 |
| Phase 2: Workflow | パイプライン処理 | 複雑な処理が必要なパワーユーザー |
| Phase 3: Use-Cases | シナリオ駆動インターフェース | 執筆者・編集者（非技術者） |

---

## Phase 1: CLIツール (japhrase/cli.py)

### 実装内容

Click フレームワークベースのCLIツール化により、スクリプト化・バッチ処理が可能に

**実装したコマンド:**

```
japhrase extract       - テキストからフレーズを抽出
japhrase kwic          - フレーズの逆引き検索
japhrase check-divergence - あらすじと本文の乖離をチェック
japhrase detect-habits - 個人の口癖を検出
japhrase analyze       - 統合分析レポートを生成
japhrase presets-list  - プリセット一覧を表示
japhrase workflow      - YAMLワークフローを実行
japhrase use-case      - ユースケース別ワークフローを実行
japhrase use-case-list - 利用可能なユースケース一覧
```

### 使用例

```bash
# 基本的な抽出
japhrase extract input.txt

# プリセット指定
japhrase extract input.txt --preset news --format csv -o output.csv

# KWIC検索
japhrase kwic input.txt --phrase "機械学習"

# 統合分析
japhrase analyze text.txt --abstract abstract.txt --corpus past/
```

### 利点

✅ スクリプト化・自動化が容易
✅ バッチ処理が可能
✅ CI/CDパイプライン統合が可能
✅ GUIなし環境での利用可能

---

## Phase 2: ワークフローエンジン (japhrase/workflow.py)

### 実装内容

YAML定義によるタスク依存関係管理エンジン

**コア機能:**

| クラス | 責務 |
|------|------|
| WorkflowDefinition | YAML解析、検証、依存関係管理 |
| WorkflowEngine | DAG実行、並列実行制御 |
| TaskRegistry | 関数登録・ディスパッチ |
| TaskStatus/TaskResult | 状態管理・結果追跡 |

**YAML例:**

```yaml
name: "Complete Manuscript Check"
tasks:
  - id: extract_phrases
    type: extract
    input: manuscript.txt
    output: results/phrases.csv
    params:
      min_count: 5
      max_length: 30

  - id: check_divergence
    type: check_divergence
    inputs: [abstract.txt, manuscript.txt]
    depends_on: [extract_phrases]

  - id: detect_habits
    type: detect_habits
    input: manuscript.txt
    depends_on: [extract_phrases]
```

### 実行方法

```bash
# 順序実行
japhrase workflow workflow.yaml

# 並列実行（8ワーカー）
japhrase workflow workflow.yaml --parallel --max-workers 8

# レポート保存
japhrase workflow workflow.yaml -o report.txt
```

### 提供される組み込みタスク

- `extract`: フレーズ抽出
- `kwic`: 逆引き検索
- `check_divergence`: あらすじ vs 本文チェック
- `detect_habits`: 口癖検出

### 利点

✅ 複雑な依存関係を自動管理
✅ DAG検証による循環依存検出
✅ 並列実行による処理時間短縮
✅ 宣言的なワークフロー定義
✅ 再利用可能なテンプレート

---

## Phase 3: ユースケース駆動インターフェース (japhrase/use_cases.py)

### 実装内容

執筆シナリオに特化したプリセットワークフロー

**実装されたユースケース:**

#### 1. 学位論文 (academic_writing)

```
論文執筆 → フレーズ抽出 → あらすじ確認 → 口癖検出 → 過去論文検索
```

```python
from japhrase import WritingWorkflow

workflow = WritingWorkflow.for_use_case('academic_writing')
report = workflow.run(
    body_file='thesis.txt',
    abstract_file='abstract.txt',
    past_corpus_dir='past_theses/'
)
print(report)
```

```bash
japhrase use-case academic_writing --body thesis.txt --abstract abstract.txt
```

#### 2. 小説推敲 (novel_revision)

```
複数版の比較 → フレーズランキング推移 → 推敲領域偏り
```

```python
workflow = WritingWorkflow.for_use_case('novel_revision')
report = workflow.run(
    v1='draft1.txt',
    v2='draft2.txt',
    v3='draft3.txt'
)
```

#### 3. ブログ執筆 (blog_writing)

```
ブログ記事 → フレーズ抽出 → 表記ゆれ検出 → 過去記事検索
```

```python
workflow = WritingWorkflow.for_use_case('blog_writing')
report = workflow.run(
    body_file='article.txt',
    past_corpus_dir='past_articles/'
)
```

#### 4. SNS投稿 (sns_content)

```
SNS投稿 → 短いフレーズ抽出 → 表記ゆれ検出
```

```python
workflow = WritingWorkflow.for_use_case('sns_content')
report = workflow.run(body_file='tweet.txt')
```

#### 5. 編集者向け (editing)

```
原稿確認 → フレーズ分析 → 乖離検出 → 口癖検出 → 過去記事検索
```

```python
workflow = WritingWorkflow.for_use_case('editing')
report = workflow.run(
    body_file='manuscript.txt',
    abstract_file='outline.txt'
)
```

### CLI使用法

```bash
# ユースケース一覧表示
japhrase use-case-list

# 学位論文の品質チェック
japhrase use-case academic_writing --body thesis.txt --abstract abstract.txt

# 小説推敲分析
japhrase use-case novel_revision --v1 draft1.txt --v2 draft2.txt --v3 draft3.txt

# ブログ最適化
japhrase use-case blog_writing --body article.txt --corpus past/

# SNS投稿表記統一
japhrase use-case sns_content --body tweet.txt -o report.txt
```

### 利点

✅ **ユーザーフレンドリー**: 技術知識不要、意図を指定するだけ
✅ **ベストプラクティス**: 各シナリオに最適なパラメータを自動設定
✅ **複雑性を隠蔽**: ツールの詳細知識不要
✅ **一貫性**: すべてのシナリオで統一されたインターフェース
✅ **拡張性**: 新しいユースケースを簡単に追加可能

---

## アーキテクチャレイヤー

```
┌─────────────────────────────────────────────────────────┐
│                   ユーザーレイヤー                        │
│  (執筆者・編集者・プログラマー)                           │
└────────────────┬──────────────────────────────────────┘
                 │
        ┌────────┴────────────────────────┐
        │                                  │
┌───────▼──────────────┐  ┌──────────────▼──────────┐
│   Use-case Layer    │  │   Workflow Layer         │
│  (Phase 3)           │  │   (Phase 2)              │
│ WritingWorkflow      │  │ WorkflowDefinition       │
│ - academic_writing   │  │ WorkflowEngine           │
│ - novel_revision     │  │ TaskRegistry             │
│ - blog_writing       │  │                          │
│ - sns_content        │  │ YAML-based pipeline      │
│ - editing            │  │ DAG execution            │
└────────────┬─────────┘  └──────────────┬───────────┘
             │                           │
             └───────────┬───────────────┘
                         │
            ┌────────────▼────────────┐
            │    CLI Layer (Phase 1)   │
            │  japhrase commands       │
            │ - extract               │
            │ - kwic                  │
            │ - analyze               │
            │ - workflow              │
            │ - use-case              │
            └────────────┬────────────┘
                         │
        ┌────────────────┴────────────────┐
        │                                 │
┌───────▼──────────────┐   ┌────────────▼──────────┐
│  Core Library Layer  │   │  Tool Integration     │
│                      │   │                       │
│ PhraseExtracter      │   │ Writing Assistant     │
│ Evaluation           │   │ - KWICAnalyzer        │
│ Optimization         │   │ - HabitDetector       │
│ Similarity           │   │ - etc.                │
│ Pattern Analysis     │   │                       │
│                      │   │ Writing Tools         │
│                      │   │ - EditorConfig        │
│                      │   │ - SelfRecommender     │
└──────────────────────┘   └───────────────────────┘
```

---

## API進化の比較

### Before (v0.1.2)

```python
from japhrase import PhraseExtracter

# ライブラリ知識が必要
extractor = PhraseExtracter(
    min_count=6,
    max_length=16,
    weight_freq=1.0,
    threshold_originality=0.5,
    # ... 多数のパラメータ
)

phrases = extractor.get_dfphrase(texts)
```

問題点:
- パラメータの意味を理解する必要がある
- 複数ツールの組み合わせが複雑
- シナリオに応じた設定が不明確

### After (v0.1.3)

**方法1: ユースケース（推奨）**

```python
from japhrase import WritingWorkflow

# ユースケースを指定するだけ
workflow = WritingWorkflow.for_use_case('academic_writing')
report = workflow.run(
    body_file='thesis.txt',
    abstract_file='abstract.txt'
)
```

**方法2: CLIコマンド**

```bash
japhrase use-case academic_writing --body thesis.txt --abstract abstract.txt
```

**方法3: YAMLワークフロー**

```bash
japhrase workflow workflow.yaml
```

**方法4: ライブラリAPI（パワーユーザー向け）**

```python
from japhrase import PhraseExtracter

# 従来のAPIは継続サポート
```

---

## テストカバレッジ

| フェーズ | テスト数 | カバレッジ |
|--------|--------|----------|
| Phase 1 | 10 | CLI機能全体 |
| Phase 2 | 16 | ワークフロー・DAG・並列実行 |
| Phase 3 | 12 | すべてのユースケース |
| その他 | 108 | 既存機能 |
| **合計** | **156** | **全機能** |

---

## 移行ガイド

### 既存ユーザー向け

既存のコード（ライブラリAPI）は完全互換です。
新しいインターフェースは**追加機能**であり、既存コードの動作を変えません。

```python
# v0.1.2で動作したコードはv0.1.3でも動作
from japhrase import PhraseExtracter
phrases = PhraseExtracter().get_dfphrase(texts)
```

### 新規ユーザー向け

新しいユースケース駆動インターフェースを推奨します:

```python
from japhrase import WritingWorkflow

# シンプルで直感的
workflow = WritingWorkflow.for_use_case('academic_writing')
report = workflow.run(body_file='...')
```

---

## 今後の展開

### Phase 4: 設定ファイルシステム (計画中)

プロジェクト固有の設定を `.japhrase.yml` で管理:

```yaml
# .japhrase.yml
project: my_novel

presets:
  quick_check:
    min_count: 5
    max_length: 20

workflow_templates:
  - name: manuscript_check
    path: workflows/check.yaml
```

### Phase 5: Webダッシュボード (計画中)

ブラウザベースのUI:
- リアルタイム分析
- グラフ・ビジュアライゼーション
- 分析履歴管理
- 比較ビュー

---

## 参考資料

- [USAGE.md](USAGE.md) - 基本的な使い方
- [examples/workflows/README.md](../examples/workflows/README.md) - ワークフロー例
- [japhrase/cli.py](../japhrase/cli.py) - CLI実装
- [japhrase/workflow.py](../japhrase/workflow.py) - ワークフローエンジン
- [japhrase/use_cases.py](../japhrase/use_cases.py) - ユースケース実装

---

## 技術仕様

### 依存関係（新規追加）

- **networkx** >= 2.6 - DAG実装
- **PyYAML** >= 6.0 - YAML解析
- **click** >= 8.0.0 - CLIツール（既存）

### Python互換性

- Python 3.7+ （既存と同じ）

### ライセンス

- MIT License

---

## まとめ

japhrase は3つのフェーズを通じて、**シンプルなコマンドラインツール**から**統合執筆支援プラットフォーム**へと進化しました。

```
v0.1.2: ライブラリ（パラメータ駆動）
  ↓
v0.1.3 Phase 1: CLI（コマンド駆動）
  ↓
v0.1.3 Phase 2: Workflow（宣言駆動）
  ↓
v0.1.3 Phase 3: Use-Cases（意図駆動）✅
```

ユーザーは自分のスキルレベルと用途に応じて、最適なインターフェースを選択できます。

- 非技術者・執筆者 → ユースケース（Phase 3）
- パワーユーザー → ワークフロー（Phase 2）
- スクリプター → CLI（Phase 1）
- 開発者 → API（従来通り）
