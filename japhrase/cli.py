"""
japhrase コマンドラインインターフェース

CLIから各種分析機能を実行するメインエントリーポイント
"""

import click
import sys
from pathlib import Path
from typing import Optional, List
import logging
import pandas as pd

from .extracter import PhraseExtracter, PRESETS
from .incremental import IncrementalPhraseState
from .stats_utils import (
    compute_stats_data,
    flatten_stats_for_csv,
    resolve_phrase_column,
    resolve_frequency_column,
    ensure_length_column,
)
from .evidence import build_phrase_evidence
from .stats_report import render_stats_html
from .writing_assistant import (
    KWICAnalyzer,
    AbstractBodyChecker,
    HabitDetector,
    RevisionHeatmap,
    RankingTrajectory
)
from .writing_tools import EditorConfigGenerator, SelfRecommender
from .workflow import WorkflowDefinition, WorkflowEngine
from .use_cases import WritingWorkflow
from .config import JaphraseConfig
from .checker import QualityChecker
from .utils import read_file, export_to_csv, export_to_json
from .document_vectorizer import DocumentVectorizer

logger = logging.getLogger(__name__)

PRESET_CHOICES = list(PRESETS.keys()) + ["auto"]


def _resolve_preset_name(preset: Optional[str], texts: List[str]) -> Optional[str]:
    """自動指定ならテキストからプリセット名を推定して返す。"""
    if preset == "auto":
        return PhraseExtracter.infer_preset_name(texts)
    return preset


def _build_extractor(
    final_params: dict,
    min_count: Optional[int],
    max_length: Optional[int],
    verbose: bool,
    texts: List[str],
) -> PhraseExtracter:
    """設定値とCLI上書きを反映したフレーズ抽出器を構築する。"""
    preset_name = final_params.get("preset")
    if preset_name:
        preset_name = _resolve_preset_name(preset_name, texts)
        preset_overrides = {
            k: v
            for k, v in final_params.items()
            if k
            in [
                "min_count",
                "max_length",
                "min_length",
                "threshold_originality",
                "weight_freq",
                "weight_len",
                "removes",
                "knowns",
                "unnecesary",
                "use_pmi",
                "use_branching_entropy",
                "selection",
            ]
        }
        if min_count is not None:
            preset_overrides["min_count"] = min_count
        if max_length is not None:
            preset_overrides["max_length"] = max_length
        return PhraseExtracter.preset(
            preset_name, verbose=1 if verbose else 0, **preset_overrides
        )

    if "min_count" not in final_params:
        final_params["min_count"] = 6
    if "max_length" not in final_params:
        final_params["max_length"] = 16

    return PhraseExtracter(
        verbose=1 if verbose else 0,
        **{
            k: v
            for k, v in final_params.items()
            if k
            in [
                "min_count",
                "max_length",
                "min_length",
                "threshold_originality",
                "weight_freq",
                "weight_len",
                "removes",
                "knowns",
                "unnecesary",
                "use_pmi",
                "use_branching_entropy",
                "selection",
            ]
        },
    )


def _apply_incremental_state(
    extractor: PhraseExtracter,
    texts: List[str],
    state_path: Optional[str],
    resume: bool,
) -> Optional[IncrementalPhraseState]:
    """増分状態を必要に応じて復元・更新し、指定パスへ保存する。"""
    if not state_path:
        return None

    state = None
    if resume and Path(state_path).exists():
        state = IncrementalPhraseState.load(state_path)
    if state is None:
        state = IncrementalPhraseState.from_extractor(extractor)

    state.update(extractor, texts)
    state.save(state_path)
    return state


# ルートグループ
@click.group(invoke_without_command=True)
@click.version_option()
@click.pass_context
def cli(ctx):
    """
    japhrase - 日本語フレーズ抽出・分析プラットフォーム

    文章品質と執筆効率を向上させるための高度なNLP分析ツールです。

    【コア機能】
      - フレーズ自動抽出 - PMI/エントロピー基盤の統計分析
      - 表記ゆれ検出 - 表現の重複と多様性の分析
      - 執筆習癖検出 - パーソナルスタイルの可視化
      - あらすじ乖離度 - 本文との整合性チェック
      - セルフリコメンデーション - 過去原稿との関連性発見

    【使用シーン】
      学位論文・学術論文、小説推敲、ブログ最適化、SNS投稿統一、
      編集者向けチェック、執筆ワークフロー自動化

    【開始方法】
      japhrase COMMAND [OPTIONS] [ARGS]
      japhrase --help (詳細なヘルプを表示)

    【主要コマンド】
      extract       テキストから自動的にフレーズを抽出
      stats         フレーズ統計を計算（JSON/CSV出力対応）
      analyze       統合分析レポートを生成
      use-case      ユースケース別ワークフロー実行
      workflow      YAMLワークフロー定義を実行

    詳細は各コマンドの --help を参照してください。
    """
    if ctx.invoked_subcommand is None:
        click.echo(ctx.get_help())


# ===== コマンド: extract =====
@cli.command()
@click.argument('input_file', type=click.Path(exists=True), metavar='INPUT_FILE')
@click.option('-o', '--output', type=click.Path(), help='出力ファイルパス (CSV/JSON形式で保存)')
@click.option('--config', type=click.Path(exists=True), help='設定ファイル (.japhrase.toml / .japhrase.yml)')
@click.option('--preset', type=click.Choice(PRESET_CHOICES),
              help=f'パラメータプリセット ({", ".join(PRESET_CHOICES)} から選択)')
@click.option('--min-count', type=int, help='最小出現回数フィルタ (デフォルト: 6)')
@click.option('--max-length', type=int, help='最大フレーズ長フィルタ (デフォルト: 16)')
@click.option('--format', type=click.Choice(['csv', 'json', 'table']), default='table',
              help='出力形式: table=コンソール表示, csv/json=ファイル保存用')
@click.option('-v', '--verbose', is_flag=True, help='詳細なログを表示')
@click.option('--use-pmi', is_flag=True, help='PMI（自己相互情報量）を有効化')
@click.option('--use-entropy', is_flag=True, help='分岐エントロピーを有効化')
@click.option('--no-selection', is_flag=True, help='フィルタリングを無効化（全フレーズを出力）')
@click.option('--state-path', type=click.Path(), help='Incremental state JSON path')
@click.option('--resume', is_flag=True, help='Resume from existing state')
def extract(input_file, output, config, preset, min_count, max_length, format, verbose, use_pmi, use_entropy, no_selection, state_path, resume):
    """
    テキストからフレーズを自動抽出（PMI/エントロピー分析）

    入力テキストを自動的に解析し、統計的に意味のあるフレーズを抽出します。
    設定ファイルの自動検出に対応しています（.japhrase.toml/.yml）。

    CLIオプションは設定ファイルの値を上書きします（優先度: CLI > 設定ファイル > デフォルト）。

    出力フィールド:
      seqchar         - 抽出フレーズ
      freq            - 出現回数
      length          - 文字数
      pmi             - PMI（自己相互情報量）※--use-pmi有効時
      boundary_score  - 境界スコア（0-1）※--use-entropy有効時
      originality     - オリジナリティ係数 (0.0-1.0)

    使用例:
    \b
      # SNS投稿向けプリセットで抽出
      japhrase extract tweets.txt --preset sns -o result.csv --format csv

      # PMI・エントロピー分析を有効化
      japhrase extract paper.txt --min-count 5 --use-pmi --use-entropy --no-selection

      # カスタムパラメータで抽出
      japhrase extract paper.txt --min-count 10 --max-length 20 --format json -o output.json

      # 設定ファイルを使用（CLIオプションで上書き）
      japhrase extract draft.txt --config .japhrase.toml --preset novel -v
    """
    try:
        # 設定ファイルを読み込む
        cfg = JaphraseConfig(config)
        config_params = cfg.get_extractor_params()

        # ファイル読込
        texts = read_file(input_file, encoding='auto')
        click.echo(f"📖 {len(texts)}行を読み込みました", err=True)

        # パラメータをマージ（CLIオプション > 設定ファイル）
        final_params = config_params.copy()

        if preset is not None:
            final_params['preset'] = preset
        if min_count is not None:
            final_params['min_count'] = min_count
        if max_length is not None:
            final_params['max_length'] = max_length
        if use_pmi:
            final_params['use_pmi'] = True
        if use_entropy:
            final_params['use_branching_entropy'] = True
        if no_selection:
            final_params['selection'] = 0

        # PhraseExtracter init
        resolved_preset = _resolve_preset_name(final_params.get('preset'), texts)
        if final_params.get('preset') == 'auto':
            click.echo(f"Auto preset selected: {resolved_preset}", err=True)
        if resolved_preset:
            final_params['preset'] = resolved_preset

        extractor = _build_extractor(final_params, min_count, max_length, verbose, texts)

        # Extract
        click.echo("?? ????????...", err=True)
        state = _apply_incremental_state(extractor, texts, state_path, resume)
        if state:
            phrases_df = state.to_df(extractor)
        else:
            phrases_df = extractor.get_dfphrase(texts)


        if len(phrases_df) == 0:
            click.echo("❌ フレーズが見つかりませんでした", err=True)
            sys.exit(1)

        click.echo(f"✅ {len(phrases_df)}個のフレーズを検出しました", err=True)

        # 出力
        if format == 'table':
            # PMI/エントロピー列があれば追加して表示
            display_cols = [resolve_phrase_column(phrases_df), resolve_frequency_column(phrases_df), 'length']
            if 'pmi' in phrases_df.columns:
                display_cols.append('pmi')
            if 'boundary_score' in phrases_df.columns:
                display_cols.append('boundary_score')
            click.echo(ensure_length_column(phrases_df)[display_cols].head(20).to_string(index=False))
        elif format == 'csv' and output:
            export_to_csv(phrases_df, output)
            click.echo(f"💾 {output} に保存しました", err=True)
        elif format == 'json' and output:
            export_to_json(phrases_df, output)
            click.echo(f"💾 {output} に保存しました", err=True)
        else:
            # PMI/エントロピー列があれば追加して表示
            display_cols = [resolve_phrase_column(phrases_df), resolve_frequency_column(phrases_df), 'length']
            if 'pmi' in phrases_df.columns:
                display_cols.append('pmi')
            if 'boundary_score' in phrases_df.columns:
                display_cols.append('boundary_score')
            click.echo(ensure_length_column(phrases_df)[display_cols].to_string(index=False))

    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)


# ===== コマンド: stats =====
@cli.command()
@click.argument('input_file', type=click.Path(exists=True), metavar='INPUT_FILE')
@click.option('-o', '--output', type=click.Path(), help='統計結果出力先 (JSON/CSV形式)')
@click.option('--config', type=click.Path(exists=True), help='設定ファイル (.japhrase.toml / .japhrase.yml)')
@click.option('--preset', type=click.Choice(PRESET_CHOICES),
              help=f'パラメータプリセット ({", ".join(PRESET_CHOICES)} から選択)')
@click.option('--min-count', type=int, help='最小出現回数フィルタ (デフォルト: 6)')
@click.option('--max-length', type=int, help='最大フレーズ長フィルタ (デフォルト: 16)')
@click.option('--format', type=click.Choice(['csv', 'json', 'table']), default='json',
              help='出力形式 (デフォルト: json - NRE v7対応)')
@click.option('--top-n', type=int, default=20, help='出力する上位フレーズ数 (デフォルト: 20)')
@click.option('--state-path', type=click.Path(), help='Incremental state JSON path')
@click.option('--resume', is_flag=True, help='Resume from existing state')
def stats(input_file, output, config, preset, min_count, max_length, format, top_n, state_path, resume):
    """
    フレーズ統計を計算（JSON/CSV/テーブル形式対応）

    入力テキストを解析し、フレーズの統計情報を計算します。
    JSON形式での出力は NRE v7 の audit コマンドとシームレスに統合できます。

    計算される統計値:
      集計情報:
        - total_phrases        フレーズ総数
        - unique_phrases       ユニークフレーズ数
        - text_lines           テキスト行数

      頻度分析:
        - freq_mean            出現回数の平均
        - freq_median          出現回数の中央値
        - freq_std_dev         出現回数の標準偏差
        - freq_min/max         出現回数の最小/最大値

      長さ分析:
        - length_mean          フレーズ長の平均
        - length_median        フレーズ長の中央値
        - length_std_dev       フレーズ長の標準偏差

      多様性指標:
        - diversity_score      表現の多様性スコア
        - entropy              シャノンエントロピー

      上位フレーズ:
        - top_phrases          出現頻度上位のフレーズと統計

    JSON形式出力:
      NRE v7 の audit コマンドで自動的に YAML に変換可能です。
      API形式: { "status": "success", "data": {...}, "timestamp": "..." }

    使用例:
    \b
      # JSONで統計を出力（NRE v7連携用）
      japhrase stats manuscript.txt -o stats.json --format json

      # 上位100フレーズの統計をCSVで出力
      japhrase stats paper.txt --format csv -o phrase_stats.csv --top-n 100

      # コンソールに統計を表示
      japhrase stats text.txt --preset novel --format table

      # カスタムパラメータで統計計算
      japhrase stats corpus.txt --min-count 5 --max-length 20 -o output.json
    """
    try:
        # ???????????
        cfg = JaphraseConfig(config)
        config_params = cfg.get_extractor_params()

        # ??????
        texts = read_file(input_file, encoding='auto')
        click.echo(f"?? {len(texts)}?????????", err=True)

        # ??????????CLI????? > ???????
        final_params = config_params.copy()
        if preset is not None:
            final_params['preset'] = preset
        if min_count is not None:
            final_params['min_count'] = min_count
        if max_length is not None:
            final_params['max_length'] = max_length

        resolved_preset = _resolve_preset_name(final_params.get('preset'), texts)
        if final_params.get('preset') == 'auto':
            click.echo(f"Auto preset selected: {resolved_preset}", err=True)
        if resolved_preset:
            final_params['preset'] = resolved_preset

        extractor = _build_extractor(final_params, min_count, max_length, False, texts)

        # ????????
        click.echo("?? ????????...", err=True)
        state = _apply_incremental_state(extractor, texts, state_path, resume)
        if state:
            phrases_df = state.to_df(extractor)
            total_texts = state.total_texts
        else:
            phrases_df = extractor.get_dfphrase(texts)
            total_texts = len(texts)

        if len(phrases_df) == 0:
            click.echo("? ???????????????", err=True)
            sys.exit(1)

        click.echo(f"? {len(phrases_df)}?????????????", err=True)

        params = {
            'input_file': str(input_file),
            'min_count': final_params.get('min_count', 6),
            'max_length': final_params.get('max_length', 16),
            'preset': final_params.get('preset', 'custom'),
        }
        stats_data = compute_stats_data(
            phrases_df,
            texts,
            params,
            top_n=top_n,
            total_texts_override=total_texts,
        )

        # ??
        if format == 'json':
            if output:
                export_to_json(pd.DataFrame([stats_data]), output)
                click.echo(f"? JSON?????????: {output}", err=True)
            else:
                import json
                click.echo(json.dumps(stats_data, ensure_ascii=False, indent=2))
        elif format == 'csv':
            if output:
                csv_df = flatten_stats_for_csv(stats_data)
                export_to_csv(csv_df, output)
                click.echo(f"? CSV?????????: {output}", err=True)
            else:
                click.echo("? CSV???? -o/--output ?????????????????", err=True)
                sys.exit(1)
        else:
            click.echo("\n" + "=" * 70)
            click.echo("統計サマリ")
            click.echo("=" * 70)
            click.echo("\n基本統計")
            click.echo(f"  フレーズ数: {stats_data['summary']['total_phrases']}")
            click.echo(f"  行数: {stats_data['summary']['text_lines']}")
            click.echo(f"  出現総数: {stats_data['summary']['total_phrase_occurrences']}")

            click.echo("\n頻度統計")
            click.echo(f"  平均: {stats_data['frequency']['mean']:.2f}")
            click.echo(f"  中央値: {stats_data['frequency']['median']:.2f}")
            click.echo(f"  標準偏差: {stats_data['frequency']['std_dev']:.2f}")
            click.echo(f"  範囲: {stats_data['frequency']['min']}-{stats_data['frequency']['max']}")

            click.echo("\n長さ統計")
            click.echo(f"  平均: {stats_data['length']['mean']:.2f} 文字")
            click.echo(f"  中央値: {stats_data['length']['median']:.2f} 文字")
            click.echo(f"  範囲: {stats_data['length']['min']}-{stats_data['length']['max']} 文字")

            click.echo("\n多様性統計")
            click.echo(f"  エントロピー: {stats_data['diversity']['entropy']:.3f}")
            click.echo(f"  ジニ係数: {stats_data['diversity']['gini_coefficient']:.3f}")

            click.echo(f"\n上位頻出フレーズ {len(stats_data['top_phrases'])} 個")
            for i, phrase_info in enumerate(stats_data['top_phrases'], 1):
                click.echo(
                    f"  {i:2d}. {phrase_info['phrase']:<20} (頻度: {phrase_info['frequency']:3d} 回, 長さ: {phrase_info['length']:2d})"
                )
            click.echo("\n" + "=" * 70)
    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        import traceback
        traceback.print_exc()
        sys.exit(1)


# ===== Command: explain =====
@cli.command()
@click.argument('input_file', type=click.Path(exists=True), metavar='INPUT_FILE')
@click.option('--phrase', multiple=True, help='Target phrase (repeatable)')
@click.option('--top-n', type=int, default=10, help='Top N phrases to explain')
@click.option('--context-chars', type=int, default=20, help='Context characters around a phrase')
@click.option('--max-samples', type=int, default=3, help='Max samples per phrase')
@click.option('--format', type=click.Choice(['json', 'table']), default='json', help='Output format')
@click.option('-o', '--output', type=click.Path(), help='Output file path (JSON)')
@click.option('--config', type=click.Path(exists=True), help='Config file (.japhrase.toml / .japhrase.yml)')
@click.option('--preset', type=click.Choice(PRESET_CHOICES), help='Preset name')
@click.option('--min-count', type=int, help='Min count filter')
@click.option('--max-length', type=int, help='Max length filter')
@click.option('--state-path', type=click.Path(), help='Incremental state JSON path')
@click.option('--resume', is_flag=True, help='Resume from existing state')
@click.option('-v', '--verbose', is_flag=True, help='Verbose output')
def explain(
    input_file,
    phrase,
    top_n,
    context_chars,
    max_samples,
    format,
    output,
    config,
    preset,
    min_count,
    max_length,
    state_path,
    resume,
    verbose,
):
    """Explain phrases with PMI, entropy, and context samples."""
    try:
        cfg = JaphraseConfig(config)
        config_params = cfg.get_extractor_params()

        texts = read_file(input_file, encoding='auto')
        click.echo(f"?? {len(texts)}?????????", err=True)

        final_params = config_params.copy()
        if preset is not None:
            final_params['preset'] = preset
        if min_count is not None:
            final_params['min_count'] = min_count
        if max_length is not None:
            final_params['max_length'] = max_length

        resolved_preset = _resolve_preset_name(final_params.get('preset'), texts)
        if final_params.get('preset') == 'auto':
            click.echo(f"Auto preset selected: {resolved_preset}", err=True)
        if resolved_preset:
            final_params['preset'] = resolved_preset

        extractor = _build_extractor(final_params, min_count, max_length, verbose, texts)

        click.echo("?? ????????...", err=True)
        state = _apply_incremental_state(extractor, texts, state_path, resume)
        if state:
            phrases_df = state.to_df(extractor)
        else:
            phrases_df = extractor.get_dfphrase(texts)

        if phrases_df.empty:
            click.echo("? ???????????????", err=True)
            sys.exit(1)

        phrases_df = ensure_length_column(phrases_df)
        phrase_col = resolve_phrase_column(phrases_df)
        freq_col = resolve_frequency_column(phrases_df)

        if phrase:
            target_phrases = list(phrase)
        else:
            target_phrases = (
                phrases_df.nlargest(min(top_n, len(phrases_df)), freq_col)[phrase_col]
                .astype(str)
                .tolist()
            )

        phrase_freqs = {
            str(row[phrase_col]): int(row[freq_col])
            for _, row in phrases_df.iterrows()
        }

        evidence = build_phrase_evidence(
            texts,
            target_phrases,
            phrase_freqs,
            context_chars=context_chars,
            max_samples=max_samples,
        )

        if format == 'json':
            if output:
                Path(output).parent.mkdir(parents=True, exist_ok=True)
                import json
                with open(output, 'w', encoding='utf-8') as f:
                    json.dump(evidence, f, ensure_ascii=False, indent=2)
                click.echo(f"? JSON???????: {output}", err=True)
            else:
                import json
                click.echo(json.dumps(evidence, ensure_ascii=False, indent=2))
        else:
            for item in evidence:
                click.echo(f"\n[{item['phrase']}] freq={item['frequency']} pmi={item['pmi']:.3f} boundary={item['boundary_score']:.3f}")
                for sample in item['samples']:
                    click.echo(f"  L{sample['line_num']}: {sample['context']}")

    except Exception as e:
        click.echo(f"? ???: {e}", err=True)
        sys.exit(1)


# ===== Command: stats-report =====
@cli.command(name='stats-report')
@click.argument('input_file', type=click.Path(exists=True), metavar='INPUT_FILE')
@click.option('-o', '--output', type=click.Path(), required=True, help='HTML output path')
@click.option('--title', type=str, default='Phrase Stats Report', help='Report title')
@click.option('--config', type=click.Path(exists=True), help='Config file (.japhrase.toml / .japhrase.yml)')
@click.option('--preset', type=click.Choice(PRESET_CHOICES), help='Preset name')
@click.option('--min-count', type=int, help='Min count filter')
@click.option('--max-length', type=int, help='Max length filter')
@click.option('--top-n', type=int, default=20, help='Top N phrases to include')
@click.option('--state-path', type=click.Path(), help='Incremental state JSON path')
@click.option('--resume', is_flag=True, help='Resume from existing state')
@click.option('-v', '--verbose', is_flag=True, help='Verbose output')
def stats_report(
    input_file,
    output,
    title,
    config,
    preset,
    min_count,
    max_length,
    top_n,
    state_path,
    resume,
    verbose,
):
    """Generate an HTML stats report."""
    try:
        cfg = JaphraseConfig(config)
        config_params = cfg.get_extractor_params()

        texts = read_file(input_file, encoding='auto')
        click.echo(f"?? {len(texts)}?????????", err=True)

        final_params = config_params.copy()
        if preset is not None:
            final_params['preset'] = preset
        if min_count is not None:
            final_params['min_count'] = min_count
        if max_length is not None:
            final_params['max_length'] = max_length

        resolved_preset = _resolve_preset_name(final_params.get('preset'), texts)
        if final_params.get('preset') == 'auto':
            click.echo(f"Auto preset selected: {resolved_preset}", err=True)
        if resolved_preset:
            final_params['preset'] = resolved_preset

        extractor = _build_extractor(final_params, min_count, max_length, verbose, texts)

        click.echo("?? ????????...", err=True)
        state = _apply_incremental_state(extractor, texts, state_path, resume)
        if state:
            phrases_df = state.to_df(extractor)
            total_texts = state.total_texts
        else:
            phrases_df = extractor.get_dfphrase(texts)
            total_texts = len(texts)

        if phrases_df.empty:
            click.echo("? ???????????????", err=True)
            sys.exit(1)

        params = {
            'input_file': str(input_file),
            'min_count': final_params.get('min_count', 6),
            'max_length': final_params.get('max_length', 16),
            'preset': final_params.get('preset', 'custom'),
        }
        stats_data = compute_stats_data(
            phrases_df,
            texts,
            params,
            top_n=top_n,
            total_texts_override=total_texts,
        )

        html = render_stats_html(stats_data, title=title)
        Path(output).parent.mkdir(parents=True, exist_ok=True)
        with open(output, 'w', encoding='utf-8') as f:
            f.write(html)
        click.echo(f"? HTML???????????: {output}", err=True)

    except Exception as e:
        click.echo(f"? ???: {e}", err=True)
        sys.exit(1)


@cli.command()
@click.argument('input_file', type=click.Path(exists=True), metavar='INPUT_FILE')
@click.option('--phrase', required=True, help='検索するフレーズ')
@click.option('-o', '--output', type=click.Path(), help='検索結果出力先')
@click.option('--context', type=int, default=2, help='前後の文脈行数 (デフォルト: 2)')
def kwic(input_file, phrase, output, context):
    """
    フレーズの出現箇所を文脈付きで検索 (KWIC: Keyword in Context)

    指定したフレーズがテキスト内のどこに出現するかを、前後の文脈と共に表示します。
    表記ゆれの確認や、フレーズの使用方法の検証に有用です。

    出力形式:
      出現番号    - 何番目の出現か（全体に対する位置）
      行番号      - テキスト内での行番号
      文字位置    - その行内での文字位置
      context     - 前後の文脈（フレーズは【】で囲まれる）

    使用例:
    \b
      # 「機械学習」の出現箇所を確認
      japhrase kwic paper.txt --phrase "機械学習" --context 2

      # 「表記ゆれ」の出現箇所を保存
      japhrase kwic manuscript.txt --phrase "表記ゆれ" -o kwic_results.txt
    """
    try:
        texts = read_file(input_file, encoding='auto')
        click.echo(f"📖 {len(texts)}行を読み込みました", err=True)

        kwic = KWICAnalyzer(texts, context_lines=context)
        results = kwic.find_phrase(phrase)

        if len(results) == 0:
            click.echo(f"❌ フレーズ「{phrase}」は見つかりませんでした", err=True)
            sys.exit(1)

        click.echo(f"✅ {len(results)}件検出しました\n", err=True)

        # 表示
        for idx, row in results.iterrows():
            click.echo(f"【出現 {row['occurrence_num']}/{row['total_occurrences']}】")
            click.echo(f"行番号: {row['line_num']}, 文字位置: {row['char_pos']}")
            click.echo(row['context'])
            click.echo("-" * 70)

        # ファイル出力
        if output:
            kwic.export_kwic_results(phrase, output)
            click.echo(f"💾 {output} に保存しました", err=True)

    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)


# ===== コマンド: check-divergence =====
@cli.command()
@click.argument('abstract_file', type=click.Path(exists=True))
@click.argument('body_file', type=click.Path(exists=True))
@click.option('-o', '--output', type=click.Path(), help='レポート出力先')
def check_divergence(abstract_file, body_file, output):
    """
    あらすじと本文の乖離をチェック

    使用例:
    \b
        japhrase check-divergence abstract.txt body.txt
        japhrase check-divergence abstract.txt body.txt -o report.txt
    """
    try:
        abstract_text = '\n'.join(read_file(abstract_file, encoding='auto'))
        body_text = '\n'.join(read_file(body_file, encoding='auto'))

        click.echo("🔍 乖離分析を実行中...", err=True)
        checker = AbstractBodyChecker(abstract_text, body_text)

        report = checker.generate_report()
        click.echo(report)

        if output:
            checker.export_report(output)
            click.echo(f"\n💾 {output} に保存しました", err=True)

    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)


# ===== コマンド: detect-habits =====
@cli.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--reference-dir', type=click.Path(exists=True), help='参照コーパスディレクトリ')
@click.option('--top-n', type=int, default=10, help='表示上位件数')
@click.option('-o', '--output', type=click.Path(), help='出力先（CSV）')
def detect_habits(input_file, reference_dir, top_n, output):
    """
    個人の口癖・習癖を検出

    使用例:
    \b
        japhrase detect-habits text.txt
        japhrase detect-habits text.txt --reference-dir corpus/ -o habits.csv
    """
    try:
        texts = read_file(input_file, encoding='auto')
        click.echo(f"📖 {len(texts)}行を読み込みました", err=True)

        # 参照テキスト取得
        reference_texts = None
        if reference_dir:
            ref_files = list(Path(reference_dir).glob('*.txt'))
            reference_texts = []
            for f in ref_files:
                try:
                    reference_texts.extend(read_file(str(f), encoding='auto'))
                except:
                    pass

        click.echo("🔍 口癖を検出中...", err=True)
        detector = HabitDetector(texts, reference_texts=reference_texts)
        habits = detector.detect_habits(limit=top_n)

        if len(habits) == 0:
            click.echo("✅ 特別な口癖は見つかりませんでした", err=True)
        else:
            click.echo(f"✅ {len(habits)}個の習癖を検出しました\n", err=True)
            click.echo(habits.to_string(index=False))

            if output:
                habits.to_csv(output, index=False, encoding='utf-8-sig')
                click.echo(f"\n💾 {output} に保存しました", err=True)

    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)


# ===== コマンド: analyze =====
@cli.command()
@click.argument('input_file', type=click.Path(exists=True), metavar='INPUT_FILE')
@click.option('--abstract', type=click.Path(exists=True),
              help='あらすじファイル (あらすじ vs 本文の乖離度を分析)')
@click.option('--corpus-dir', type=click.Path(exists=True),
              help='過去原稿ディレクトリ (セルフリコメンデーション用)')
@click.option('-o', '--output', type=click.Path(), help='分析レポート出力先')
@click.option('--preset', type=click.Choice(PRESET_CHOICES), default='default',
              help=f'抽出パラメータプリセット (デフォルト: default)')
def analyze(input_file, abstract, corpus_dir, output, preset):
    """
    複数の分析機能を組み合わせた統合レポート生成

    1つのコマンドで以下の分析を順序実行します：
      [1] フレーズ自動抽出 (preset で指定)
      [2] あらすじ乖離度分析 (--abstract オプション指定時)
      [3] 個人の執筆習癖検出
      [4] セルフリコメンデーション (--corpus-dir 指定時)

    各セクションは統合レポートに含まれ、実行結果はコンソール表示と
    ファイル保存の両方に対応しています。

    レポート内容:
      📋 フレーズ抽出結果
      🔄 あらすじ乖離度（%）
      👤 検出された個人の習癖（上位5件）
      🔍 関連記事（過去原稿との類似度）

    使用例:
    \b
      # 基本的な使用 (フレーズ抽出のみ)
      japhrase analyze manuscript.txt -o report.txt

      # あらすじ乖離度も分析
      japhrase analyze manuscript.txt --abstract abstract.txt -o report.txt

      # 過去原稿との比較も含める
      japhrase analyze draft.txt --abstract abstract.txt \\
                 --corpus-dir past_articles/ -o full_report.txt

      # 特定プリセットを使用
      japhrase analyze novel.txt --preset novel --corpus-dir corpus/ -o analysis.txt
    """
    try:
        texts = read_file(input_file, encoding='auto')
        text = '\n'.join(texts)

        click.echo("📊 統合分析を開始します...\n", err=True)

        report = []
        report.append("=" * 70)
        report.append("japhrase 統合分析レポート")
        report.append("=" * 70)
        report.append("")

        # 1. フレーズ抽出
        click.echo("  [1/4] フレーズを抽出中...", err=True)
        extractor = PhraseExtracter.preset(preset, verbose=0)
        phrases_df = extractor.get_dfphrase(texts)
        report.append(f"\n📋 フレーズ抽出\n")
        report.append(f"検出フレーズ数: {len(phrases_df)}個")
        if len(phrases_df) > 0:
            report.append(f"上位フレーズ: {', '.join(phrases_df.head(5)['seqchar'].tolist())}")

        # 2. あらすじチェック
        if abstract:
            click.echo("  [2/4] あらすじをチェック中...", err=True)
            abstract_text = '\n'.join(read_file(abstract, encoding='auto'))
            checker = AbstractBodyChecker(abstract_text, text)
            divergence = checker.get_divergence_score()
            report.append(f"\n🔄 あらすじ vs 本文\n")
            report.append(f"乖離度: {divergence:.1%}")
        else:
            click.echo("  [2/4] スキップ (あらすじなし)", err=True)

        # 3. 口癖検出
        click.echo("  [3/4] 口癖を検出中...", err=True)
        detector = HabitDetector(texts)
        habits = detector.detect_habits(limit=5)
        report.append(f"\n👤 個人の習癖\n")
        if len(habits) > 0:
            report.append(f"検出数: {len(habits)}個")
        else:
            report.append("検出なし")

        # 4. セルフリコメンデーション
        if corpus_dir:
            click.echo("  [4/4] 関連記事を検索中...", err=True)
            recommender = SelfRecommender(corpus_dir, min_count=1)
            related = recommender.find_related_articles(text, top_n=3)
            report.append(f"\n🔍 関連記事\n")
            if len(related) > 0:
                report.append(f"見つかった記事: {len(related)}件")
            else:
                report.append("見つかりません")
        else:
            click.echo("  [4/4] スキップ (コーパスなし)", err=True)

        report.append("\n" + "=" * 70)
        report_text = "\n".join(report)

        click.echo(report_text)

        if output:
            with open(output, 'w', encoding='utf-8-sig') as f:
                f.write(report_text)
            click.echo(f"\n💾 {output} に保存しました", err=True)

    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)


# ===== コマンド: workflow =====
@cli.command()
@click.argument('workflow_file', type=click.Path(exists=True))
@click.option('--parallel', is_flag=True, help='タスクを並列実行')
@click.option('--max-workers', type=int, default=4, help='並列実行時のワーカー数')
@click.option('-o', '--output', type=click.Path(), help='レポート出力先')
@click.option('-v', '--verbose', is_flag=True, help='詳細出力')
def workflow(workflow_file, parallel, max_workers, output, verbose):
    """
    YAMLで定義されたワークフローを実行

    ワークフロー定義ファイルに複数のタスクを記述し、
    依存関係を自動的に解決しながら順序実行または並列実行します。

    使用例:
    \b
        japhrase workflow manuscript.yaml
        japhrase workflow manuscript.yaml --parallel --max-workers 8
        japhrase workflow manuscript.yaml -o report.txt
    """
    try:
        click.echo(f"📋 ワークフローを読込中: {workflow_file}", err=True)

        # ワークフロー定義を読込
        wf = WorkflowDefinition.from_yaml(workflow_file)

        click.echo(f"✅ ワークフロー「{wf.name}」をロードしました", err=True)
        click.echo(f"   タスク数: {len(wf.tasks)}個", err=True)

        # ワークフロー検証
        valid, errors = wf.validate()
        if not valid:
            click.echo(f"❌ ワークフロー検証エラー:", err=True)
            for error in errors:
                click.echo(f"   - {error}", err=True)
            sys.exit(1)

        # 実行順序を表示
        execution_order = wf.get_execution_order()
        click.echo(f"📊 実行順序: {' → '.join(execution_order)}", err=True)

        # ワークフロー実行
        if parallel:
            click.echo(f"🚀 並列実行を開始します (ワーカー数: {max_workers})...", err=True)
        else:
            click.echo("🚀 順序実行を開始します...", err=True)

        engine = WorkflowEngine()
        results = engine.execute(wf, parallel=parallel, max_workers=max_workers)

        # レポート生成
        report = engine.get_report()
        click.echo(report)

        # ファイル出力
        if output:
            with open(output, 'w', encoding='utf-8-sig') as f:
                f.write(report)
            click.echo(f"\n💾 {output} に保存しました", err=True)

        # 完了状況をチェック
        completed = sum(1 for r in results.values() if r.status.value == 'completed')
        failed = sum(1 for r in results.values() if r.status.value == 'failed')

        if failed > 0:
            click.echo(f"\n⚠️ {completed}/{len(results)}個のタスクが完了しました", err=True)
            sys.exit(1)
        else:
            click.echo(f"\n✅ すべてのタスクが正常に完了しました", err=True)

    except FileNotFoundError as e:
        click.echo(f"❌ ファイルが見つかりません: {e}", err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        if verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)


# ===== コマンド: vectorize =====
@cli.command()
@click.argument('files', nargs=-1, type=click.Path(exists=True), required=True)
@click.option('--n-topics', '-t', type=int, default=10, help='NMFのトピック数')
@click.option('--output', '-o', type=click.Path(), required=True, help='出力ディレクトリ')
@click.option('--feature-mode', '-m',
              type=click.Choice(['tfidf', 'low_pmi', 'high_pmi', 'hybrid']),
              default='tfidf',
              help='特徴抽出モード')
@click.option('--pmi-threshold', type=float, default=3.0, help='PMI閾値')
@click.option('--min-count', type=int, default=6, help='最小出現回数')
@click.option('--visualize', is_flag=True, help='可視化を生成')
@click.option('--encoding', default='auto', help='ファイルエンコーディング')
@click.option('-v', '--verbose', is_flag=True, help='詳細出力')
def vectorize(files, n_topics, output, feature_mode, pmi_threshold, min_count,
              visualize, encoding, verbose):
    """
    複数のドキュメントをNMFでベクトル化し、差分を可視化

    特徴モード:
    \b
    - tfidf: 標準TF-IDF（意味的な差を検出）
    - low_pmi: 低PMIフレーズのみ（文体・手癖の差を検出）
    - high_pmi: 高PMIフレーズのみ（意味的な表現の差）
    - hybrid: 複合特徴空間

    使用例:
    \b
        japhrase vectorize doc1.txt doc2.txt doc3.txt -t 10 -o results/
        japhrase vectorize author1.txt author2.txt -t 10 -o habits_analysis/ \\
            --feature-mode low_pmi --pmi-threshold 3.0 --visualize
    """
    try:
        from pathlib import Path
        import os

        # 出力ディレクトリを作成
        Path(output).mkdir(parents=True, exist_ok=True)

        click.echo(f"📊 ドキュメント ベクトル化を開始します", err=True)
        click.echo(f"   ファイル数: {len(files)}", err=True)
        click.echo(f"   トピック数: {n_topics}", err=True)
        click.echo(f"   特徴モード: {feature_mode}", err=True)

        # DocumentVectorizerを作成
        vectorizer = DocumentVectorizer(
            n_topics=n_topics,
            feature_mode=feature_mode,
            pmi_threshold=pmi_threshold,
            min_count=min_count,
            verbose=1 if verbose else 0
        )

        # ファイルからベクトル化
        click.echo("📖 ファイルを読み込み中...", err=True)
        result = vectorizer.from_files(
            list(files),
            n_topics=n_topics,
            feature_mode=feature_mode,
            encoding=encoding
        )

        click.echo(f"✅ ベクトル化完了: {result.document_topic_matrix.shape[0]}件 × {result.document_topic_matrix.shape[1]}トピック", err=True)

        # 結果を保存
        result_pkl = os.path.join(output, 'vectorization_result.pkl')
        result.save(result_pkl)

        # ドキュメント-トピック行列を保存
        doc_topic_csv = os.path.join(output, 'document_topic_matrix.csv')
        vectorizer.export_matrix(result, doc_topic_csv, format='csv', matrix_type='normalized')

        # 上位タームを保存
        top_terms_json = os.path.join(output, 'top_terms.json')
        vectorizer.export_top_terms(result, top_terms_json, n_terms=20, format='json')

        # テキストベースの分析結果を表示
        click.echo("\n" + "=" * 80)
        text_report = vectorizer.format_document_profiles_as_text(result)
        click.echo(text_report)

        click.echo("\n" + vectorizer.format_topic_summary_as_text(result, n_terms=15))

        # ペアワイズ距離を計算・表示
        distances = vectorizer.calculate_pairwise_distances(result, metric='cosine')
        distances_csv = os.path.join(output, 'pairwise_distances.csv')
        distances.to_csv(distances_csv, encoding='utf-8-sig')

        click.echo("\n" + "=" * 80)
        click.echo("📏 ドキュメント間の距離（コサイン距離）")
        click.echo("=" * 80)
        click.echo(distances.round(3).to_string())

        # 最初のドキュメントとの差分を計算・表示
        if len(files) > 1:
            diffs = vectorizer.calculate_differences(result, 0, list(range(1, len(files))))
            diff_text = vectorizer.format_differences_as_text(diffs, top_n_topics=None)
            click.echo("\n" + diff_text)

            # 差分行列を保存
            diff_csv = os.path.join(output, 'topic_differences.csv')
            diffs.to_csv(diff_csv, encoding='utf-8-sig')

        # 可視化を生成（オプション）
        if visualize:
            click.echo("\n🎨 可視化を生成中...", err=True)

            # ドキュメント-トピック ヒートマップ
            heatmap_path = os.path.join(output, 'document_topic_heatmap.png')
            vectorizer.export_heatmap(result, heatmap_path)
            click.echo(f"  ✅ {Path(heatmap_path).name}", err=True)

            # 複数ドキュメント比較（レーダープロット）
            if len(files) >= 2 and len(files) <= 5:
                radar_path = os.path.join(output, 'document_comparison_radar.png')
                doc_indices = list(range(min(5, len(files))))
                vectorizer.export_radar_plot(result, doc_indices, radar_path)
                click.echo(f"  ✅ {Path(radar_path).name}", err=True)

            # 最初のドキュメントとの差分を可視化
            if len(files) > 1:
                diffs = vectorizer.calculate_differences(result, 0, list(range(1, len(files))))

                # 差分 ヒートマップ
                diff_heatmap = os.path.join(output, 'difference_heatmap.png')
                vectorizer.export_difference_heatmap(diffs, diff_heatmap)
                click.echo(f"  ✅ {Path(diff_heatmap).name}", err=True)

                # 差分 バーチャート
                diff_bars = os.path.join(output, 'difference_bars.png')
                vectorizer.export_difference_bars(diffs, diff_bars, top_n_topics=10)
                click.echo(f"  ✅ {Path(diff_bars).name}", err=True)

        # 最終サマリー
        click.echo("\n" + "=" * 80, err=True)
        click.echo("✅ ベクトル化処理完了", err=True)
        click.echo("=" * 80, err=True)
        click.echo(f"出力ディレクトリ: {output}", err=True)
        click.echo(f"\n📁 出力ファイル一覧:", err=True)
        click.echo(f"  基本出力:", err=True)
        click.echo(f"    - vectorization_result.pkl (Python再利用用オブジェクト)", err=True)
        click.echo(f"    - document_topic_matrix.csv (正規化されたドキュメント-トピック行列)", err=True)
        click.echo(f"    - pairwise_distances.csv (ドキュメント間の距離)", err=True)
        click.echo(f"    - top_terms.json (各トピックの上位20ターム)", err=True)
        if len(files) > 1:
            click.echo(f"    - topic_differences.csv (ドキュメント間の差分)", err=True)
        if visualize:
            click.echo(f"  可視化:", err=True)
            click.echo(f"    - document_topic_heatmap.png", err=True)
            if len(files) >= 2 and len(files) <= 5:
                click.echo(f"    - document_comparison_radar.png", err=True)
            if len(files) > 1:
                click.echo(f"    - difference_heatmap.png", err=True)
                click.echo(f"    - difference_bars.png", err=True)

    except FileNotFoundError as e:
        click.echo(f"❌ ファイルが見つかりません: {e}", err=True)
        sys.exit(1)
    except ValueError as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        if verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)


# ===== ユーティリティコマンド =====
@cli.command()
def presets_list():
    """
    利用可能なプリセット一覧を表示
    """
    click.echo("利用可能なプリセット:")
    click.echo("=" * 70)
    for name, config in PRESETS.items():
        click.echo(f"\n[{name}]")
        click.echo(f"  {config['description']}")
        click.echo(f"  パラメータ: min_count={config['min_count']}, "
                   f"max_length={config['max_length']}")


@cli.command()
def version():
    """
    バージョン情報を表示
    """
    from . import __version__
    click.echo(f"japhrase version {__version__}")


# ===== コマンド: use-case =====
@cli.command(name='use-case')
@click.argument('use_case', type=click.Choice([
    'academic_writing', 'novel_revision', 'blog_writing', 'sns_content', 'editing'
]))
@click.option('--body', type=click.Path(exists=True), help='本文ファイル')
@click.option('--abstract', type=click.Path(exists=True), help='あらすじファイル')
@click.option('--v1', type=click.Path(exists=True), help='バージョン1（小説推敲用）')
@click.option('--v2', type=click.Path(exists=True), help='バージョン2（小説推敲用）')
@click.option('--v3', type=click.Path(exists=True), help='バージョン3（小説推敲用）')
@click.option('--corpus', type=click.Path(exists=True), help='過去原稿ディレクトリ')
@click.option('-o', '--output', type=click.Path(), help='レポート出力先')
def use_case(use_case, body, abstract, v1, v2, v3, corpus, output):
    """
    ユースケース別ワークフローを実行

    特定の執筆シナリオに最適化されたワークフローを実行します。

    利用可能なユースケース:
    \b
    - academic_writing: 学位論文・学術論文の品質チェック
    - novel_revision: 小説原稿の推敲
    - blog_writing: ブログ記事の最適化
    - sns_content: SNS投稿の表記統一
    - editing: 編集者向けチェック

    使用例:
    \b
        japhrase use-case academic_writing --body paper.txt --abstract abstract.txt
        japhrase use-case novel_revision --v1 draft1.txt --v2 draft2.txt
        japhrase use-case blog_writing --body article.txt --corpus past_articles/
        japhrase use-case sns_content --body tweet.txt -o report.txt
    """
    try:
        click.echo(f"📋 ユースケース: {use_case}", err=True)

        # ワークフローを生成
        workflow = WritingWorkflow.for_use_case(use_case)

        # ワークフローを実行
        report = workflow.run(
            body_file=body,
            abstract_file=abstract,
            v1=v1,
            v2=v2,
            v3=v3,
            past_corpus_dir=corpus,
            output_dir='results'
        )

        # レポートを表示
        click.echo(report)

        # ファイル出力
        if output:
            with open(output, 'w', encoding='utf-8-sig') as f:
                f.write(report)
            click.echo(f"\n💾 {output} に保存しました", err=True)

        click.echo("\n✅ 分析完了", err=True)

    except ValueError as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)
    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)


@cli.command(name='use-case-list')
def use_case_list():
    """
    利用可能なユースケース一覧を表示
    """
    click.echo("利用可能なユースケース:")
    click.echo("=" * 70)

    usecases = WritingWorkflow.list_usecases()
    for use_case_id, description in usecases.items():
        click.echo(f"\n[{use_case_id}]")
        click.echo(f"  {description}")


# ===== コマンド: config =====
@cli.command()
@click.option('--file', type=click.Path(exists=True), help='設定ファイルパス')
@click.option('--show-defaults', is_flag=True, help='デフォルト値も表示')
def config(file, show_defaults):
    """
    設定ファイルの内容を表示

    使用例:
    \b
        japhrase config
        japhrase config --file .japhrase.toml
        japhrase config --show-defaults
    """
    try:
        cfg = JaphraseConfig(file)

        if cfg.config:
            click.echo(cfg.display_config())
        else:
            click.echo("アクティブな設定ファイルが見つかりません", err=True)
            if show_defaults:
                click.echo("\n=== デフォルト設定 ===")
                click.echo("min_count: 6")
                click.echo("max_length: 16")
                click.echo("threshold_originality: 0.5")

    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)


# ===== コマンド: check =====
@cli.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.option('--config', type=click.Path(exists=True), help='設定ファイルパス (.toml/.yml)')
@click.option('-o', '--output', type=click.Path(), help='レポート出力先')
def check(input_file, config, output):
    """
    文書品質チェック（Linter モード）

    設定ファイルのルールに基づいて文書品質をチェック。
    - 禁止ワード検出
    - 必須キーワード確認
    - 表記ゆれ検出
    - 文書長チェック
    - 段落構成チェック

    使用例:
    \b
        japhrase check document.txt --config .japhrase.toml
        japhrase check document.txt --config .japhrase.toml -o report.txt
    """
    try:
        # テキスト読み込み
        texts = read_file(input_file, encoding='auto')
        text = '\\n'.join(texts)
        click.echo(f"📖 {len(texts)}行を読み込みました", err=True)

        # 設定ファイル読み込み
        cfg = JaphraseConfig(config)
        if not cfg.config:
            click.echo("❌ 設定ファイルが見つかりません", err=True)
            sys.exit(1)

        # チェッカー実行
        click.echo("🔍 品質チェックを実行中...", err=True)
        checker = QualityChecker(text, cfg.config)
        success, errors, warnings = checker.run_all_checks()

        # レポート表示
        report = checker.get_report()
        click.echo(report)

        # ファイル出力
        if output:
            with open(output, 'w', encoding='utf-8-sig') as f:
                f.write(report)
            click.echo(f"💾 {output} に保存しました", err=True)

        # 結果に応じて終了コードを設定
        if not success:
            sys.exit(1)

        click.echo("✅ チェック完了", err=True)

    except Exception as e:
        click.echo(f"❌ エラー: {e}", err=True)
        sys.exit(1)


def main():
    """CLIのメインエントリーポイント"""
    cli()


if __name__ == '__main__':
    main()
