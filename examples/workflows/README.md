# japhrase ワークフロー例

このディレクトリには、japhrase のワークフロー機能を使用した実用例が含まれています。

## ワークフローについて

ワークフローは YAML 形式で複数のタスクを定義し、依存関係を自動的に解決して実行する機能です。

### 基本的な使い方

```bash
# 順序実行（デフォルト）
japhrase workflow academic_writing.yaml

# 並列実行
japhrase workflow academic_writing.yaml --parallel --max-workers 8

# レポートをファイルに保存
japhrase workflow academic_writing.yaml -o report.txt
```

## ワークフロー例

### 1. academic_writing.yaml - 学位論文品質チェック

学位論文の品質を多角的にチェックするワークフロー：

- **extract_phrases**: 本文からフレーズを抽出
- **check_abstract_divergence**: あらすじと本文の乖離度をチェック
- **detect_author_habits**: 個人の口癖を検出

**使用方法:**
```bash
# プロジェクトディレクトリに以下ファイルを配置
# - abstract.txt（あらすじ）
# - body.txt（本文）

japhrase workflow examples/workflows/academic_writing.yaml
```

### 2. novel_revision.yaml - 小説原稿推敲

小説の推敲時に重複表現を検出するワークフロー：

- **extract_key_phrases**: キーフレーズを抽出
- **detect_redundancy**: 重複表現を検出

**使用方法:**
```bash
# プロジェクトディレクトリに manuscript.txt を配置

japhrase workflow examples/workflows/novel_revision.yaml
```

### 3. complete_manuscript_check.yaml - 完全品質検査

4段階の包括的な検査パイプライン：

1. **extract_core_phrases**: コアフレーズの抽出
2. **check_abstract_body_consistency**: あらすじ/本文の一貫性確認（フレーズ抽出後）
3. **detect_personal_habits**: 口癖検出（フレーズ抽出後）
4. **find_related_previous_articles**: 過去原稿との関連性分析（一貫性確認後）

**依存関係:**
```
extract_core_phrases
├── check_abstract_body_consistency
│   └── find_related_previous_articles
└── detect_personal_habits
```

**使用方法:**
```bash
japhrase workflow examples/workflows/complete_manuscript_check.yaml -o results/report.txt
```

## ワークフロー定義の構造

```yaml
name: "ワークフロー名"
description: |
  ワークフローの説明
  複数行対応

tasks:
  - id: タスクID
    type: タスクタイプ          # extract, kwic, check_divergence, detect_habits
    input: 入力ファイル         # 単一入力
    inputs: [file1, file2]      # 複数入力
    output: 出力ファイル
    depends_on: [task1, task2]  # 依存タスク
    params:                      # タスク固有パラメータ
      min_count: 5
      max_length: 20
```

## 利用可能なタスクタイプ

### extract - フレーズ抽出
```yaml
- id: extract_phrases
  type: extract
  input: input.txt
  output: phrases.csv
  params:
    min_count: 6
    max_length: 16
```

### kwic - KWIC 検索（逆引き）
```yaml
- id: kwic_search
  type: kwic
  input: text.txt
  params:
    phrase: "検索フレーズ"
    context_lines: 1
```

### check_divergence - あらすじと本文の乖離チェック
```yaml
- id: divergence_check
  type: check_divergence
  inputs:
    - abstract.txt
    - body.txt
  output: divergence.json
```

### detect_habits - 口癖検出
```yaml
- id: habits
  type: detect_habits
  input: text.txt
  output: habits.csv
  params:
    z_score_threshold: 2.0
```

## 依存関係について

タスクの `depends_on` フィールドで、タスク間の依存関係を指定できます：

```yaml
tasks:
  - id: task1
    type: extract
    input: data.txt

  - id: task2
    type: kwic
    depends_on: [task1]  # task1の完了を待つ
```

複数の依存関係も可能：
```yaml
depends_on: [task1, task2, task3]
```

## 並列実行

タスクの実行順序は依存関係に基づいて自動的に最適化されます：

```bash
# 8個のワーカーで並列実行
japhrase workflow workflow.yaml --parallel --max-workers 8
```

依存関係のないタスクは自動的に並列実行されます。

## カスタムワークフロー作成

既存のワークフローをテンプレートとして使用し、プロジェクトに合わせてカスタマイズできます：

```bash
# 既存のワークフローをコピー
cp examples/workflows/academic_writing.yaml my_workflow.yaml

# エディタで編集
nano my_workflow.yaml

# 実行
japhrase workflow my_workflow.yaml -o results/report.txt
```

## トラブルシューティング

### ワークフロー検証エラー

```
❌ ワークフロー検証エラー:
   - タスク 'task2' の依存タスク 'task1' が見つかりません
   - 循環依存が検出されました
```

**原因と対策:**
1. 依存タスク ID が正しく指定されているか確認
2. 循環依存（A→B→A）がないか確認

### タスク実行エラー

```
❌ エラー: 未知のタスクタイプ: my_task
```

**原因と対策:**
- タスク `type` が `extract`, `kwic`, `check_divergence`, `detect_habits` のいずれかであることを確認

### ファイル読み込みエラー

```
❌ エラー: [Errno 2] No such file or directory: 'missing.txt'
```

**原因と対策:**
- ワークフロー実行ディレクトリを確認
- `input` パスが正しいか確認（相対パス/絶対パス）

## 関連ドキュメント

- [USAGE.md](../USAGE.md) - japhrase の基本的な使い方
- [japhrase/workflow.py](../japhrase/workflow.py) - ワークフロー実装
