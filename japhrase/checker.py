"""
品質チェック・Linter モジュール

禁止ワード検出、キーワード不足警告など、
文書品質をテスト的に検証する機能を提供
"""

from typing import Dict, List, Tuple, Any
import re
import logging

logger = logging.getLogger(__name__)


class QualityChecker:
    """文書品質チェッカー（Linter モード）"""

    def __init__(self, text: str, config: Dict[str, Any] = None):
        """
        初期化

        Args:
            text: チェック対象のテキスト
            config: チェックルール設定
        """
        self.text = text
        self.config = config or {}
        self.errors: List[Dict[str, Any]] = []
        self.warnings: List[Dict[str, Any]] = []

    def check_forbidden_phrases(self, forbidden_list: List[str]) -> bool:
        """
        禁止ワード検出

        Args:
            forbidden_list: 禁止ワードのリスト

        Returns:
            エラーが無かった場合 True
        """
        has_error = False

        for phrase in forbidden_list:
            # 大文字小文字を区別しない検索
            pattern = re.compile(re.escape(phrase), re.IGNORECASE)
            matches = pattern.finditer(self.text)

            for match in matches:
                # 行番号を計算
                line_num = self.text[:match.start()].count('\n') + 1
                col_num = match.start() - self.text.rfind('\n', 0, match.start())

                self.errors.append({
                    'type': 'forbidden_phrase',
                    'phrase': phrase,
                    'line': line_num,
                    'column': col_num,
                    'message': f'禁止ワードが見つかりました: "{phrase}"'
                })
                has_error = True

        return not has_error

    def check_required_keywords(self, required_keywords: Dict[str, int]) -> bool:
        """
        必須キーワード チェック

        Args:
            required_keywords: {キーワード: 最小出現回数}

        Returns:
            エラーが無かった場合 True
        """
        has_warning = False

        for keyword, min_count in required_keywords.items():
            # 大文字小文字を区別しない検索
            pattern = re.compile(re.escape(keyword), re.IGNORECASE)
            matches = list(pattern.finditer(self.text))
            actual_count = len(matches)

            if actual_count < min_count:
                self.warnings.append({
                    'type': 'missing_keyword',
                    'keyword': keyword,
                    'expected': min_count,
                    'actual': actual_count,
                    'message': f'キーワード "{keyword}" の出現が不足しています (期待: {min_count}回, 実際: {actual_count}回)'
                })
                has_warning = True

        return not has_warning

    def check_spelling_consistency(self, spelling_rules: Dict[str, str]) -> bool:
        """
        表記ゆれ チェック

        Args:
            spelling_rules: {標準表記: [非標準表記のリスト]}

        Returns:
            エラーが無かった場合 True
        """
        has_error = False

        for standard, variants in spelling_rules.items():
            for variant in variants:
                if variant == standard:
                    continue

                # 大文字小文字を区別しない検索
                pattern = re.compile(re.escape(variant), re.IGNORECASE)
                matches = pattern.finditer(self.text)

                for match in matches:
                    line_num = self.text[:match.start()].count('\n') + 1

                    self.errors.append({
                        'type': 'spelling_inconsistency',
                        'found': variant,
                        'standard': standard,
                        'line': line_num,
                        'message': f'表記ゆれ: "{variant}" を "{standard}" に統一してください'
                    })
                    has_error = True

        return not has_error

    def check_length_limits(self, min_length: int = None, max_length: int = None) -> bool:
        """
        文書長チェック

        Args:
            min_length: 最小文字数
            max_length: 最大文字数

        Returns:
            エラーが無かった場合 True
        """
        text_length = len(self.text)
        has_error = False

        if min_length and text_length < min_length:
            self.errors.append({
                'type': 'too_short',
                'length': text_length,
                'min': min_length,
                'message': f'文書が短すぎます (最小: {min_length}字, 実際: {text_length}字)'
            })
            has_error = True

        if max_length and text_length > max_length:
            self.errors.append({
                'type': 'too_long',
                'length': text_length,
                'max': max_length,
                'message': f'文書が長すぎます (最大: {max_length}字, 実際: {text_length}字)'
            })
            has_error = True

        return not has_error

    def check_paragraph_structure(self, min_paragraphs: int = None) -> bool:
        """
        段落構成チェック

        Args:
            min_paragraphs: 最小段落数

        Returns:
            エラーが無かった場合 True
        """
        # 2行以上の空白で段落を分割
        paragraphs = [p.strip() for p in self.text.split('\n\n') if p.strip()]
        num_paragraphs = len(paragraphs)

        if min_paragraphs and num_paragraphs < min_paragraphs:
            self.warnings.append({
                'type': 'few_paragraphs',
                'count': num_paragraphs,
                'min': min_paragraphs,
                'message': f'段落数が少なすぎます (最小: {min_paragraphs}個, 実際: {num_paragraphs}個)'
            })
            return False

        return True

    # run_all_checks が認識するルールキー（このいずれも設定されていなければ「チェックルール0件」）
    _RULE_KEYS = (
        'forbidden_phrases',
        'required_keywords',
        'spelling_rules',
        'min_length',
        'max_length',
        'min_paragraphs',
    )

    def run_all_checks(self) -> Tuple[bool, List[str], List[str]]:
        """
        設定に基づいてすべてのチェックを実行

        [check] セクションが存在しない、またはルールキーが1つも無い場合は
        「何もチェックしていないのに合格」という暗黙のfail-openを避けるため、
        設定エラーとして扱い失敗させる。意図的にチェックを行わない場合は
        `[check] enabled = false` を明示すること。

        Returns:
            (成功フラグ, エラーメッセージリスト, 警告メッセージリスト)
        """
        check_config = self.config.get('check', {})

        if check_config.get('enabled') is False:
            # 明示的な無効化は合法的なスキップとして扱う（暗黙の0件とは区別する）
            self.warnings.append({
                'type': 'checks_disabled',
                'message': '品質チェックは設定で無効化されています (check.enabled=false)',
            })
        elif not any(key in check_config for key in self._RULE_KEYS):
            self.errors.append({
                'type': 'no_rules_configured',
                'message': (
                    '有効なチェックルールが定義されていません。'
                    '[check]セクションに forbidden_phrases 等のルールを指定するか、'
                    '意図的にチェックしない場合は check.enabled=false を明示してください。'
                ),
            })
        else:
            # 禁止ワード
            if 'forbidden_phrases' in check_config:
                self.check_forbidden_phrases(check_config['forbidden_phrases'])

            # 必須キーワード
            if 'required_keywords' in check_config:
                self.check_required_keywords(check_config['required_keywords'])

            # 表記ゆれ
            if 'spelling_rules' in check_config:
                self.check_spelling_consistency(check_config['spelling_rules'])

            # 文書長
            if 'min_length' in check_config or 'max_length' in check_config:
                self.check_length_limits(
                    check_config.get('min_length'),
                    check_config.get('max_length')
                )

            # 段落構成
            if 'min_paragraphs' in check_config:
                self.check_paragraph_structure(check_config['min_paragraphs'])

        error_messages = [e['message'] for e in self.errors]
        warning_messages = [w['message'] for w in self.warnings]

        success = len(self.errors) == 0

        return success, error_messages, warning_messages

    def get_report(self) -> str:
        """チェック結果レポートを取得"""
        lines = []
        lines.append("=" * 70)
        lines.append("品質チェック レポート")
        lines.append("=" * 70)
        lines.append("")

        # エラー
        if self.errors:
            lines.append(f"❌ エラー ({len(self.errors)}件):")
            for error in self.errors:
                lines.append(f"  - {error['message']}")
            lines.append("")

        # 警告
        if self.warnings:
            lines.append(f"⚠️ 警告 ({len(self.warnings)}件):")
            for warning in self.warnings:
                lines.append(f"  - {warning['message']}")
            lines.append("")

        # 結果
        if not self.errors and not self.warnings:
            lines.append("✅ すべてのチェックに合格しました")
        elif not self.errors:
            lines.append(f"✅ エラーなし（警告: {len(self.warnings)}件）")
        else:
            lines.append(f"❌ チェック失敗（エラー: {len(self.errors)}件, 警告: {len(self.warnings)}件）")

        lines.append("=" * 70)
        return "\n".join(lines)
