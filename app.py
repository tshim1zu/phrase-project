"""
japhrase GUI - Streamlit版
テキスト分析ツール（WritingHabitDetector, DialogueAnalyzer, StylometryAnalyzer）
"""

import streamlit as st
import pandas as pd
import json
import os
from pathlib import Path
from typing import Dict, List
from collections import Counter

# japhrase modules
from japhrase import (
    PhraseExtracter,
    WritingHabitDetector,
    DialogueAnalyzer,
    StylometryAnalyzer,
    OrthographyVariantDetector,
    PRESETS
)

# ============================================================================
# ページ設定
# ============================================================================
st.set_page_config(
    page_title="japhrase - 文章分析ツール",
    page_icon="📝",
    layout="wide",
    initial_sidebar_state="expanded"
)

st.title("📝 japhrase - 文章分析ツール")
st.markdown("""
**自動で、あなたの手癖を見つける。** あなたの知らない執筆パターンを数字で突きつけます。
""")

# ============================================================================
# グローバル設定・キャッシュ
# ============================================================================
EXCLUSION_FILE = Path("custom_exclusion.json")
HISTORY_FILE = Path("analysis_history.jsonl")

@st.cache_resource
def load_exclusion_list() -> Dict[str, List[str]]:
    """カスタム除外リストを読み込み"""
    if EXCLUSION_FILE.exists():
        with open(EXCLUSION_FILE, 'r', encoding='utf-8') as f:
            return json.load(f)
    return {"phrases": [], "keywords": []}

@st.cache_resource
def load_presets() -> Dict[str, dict]:
    """PRESETS を辞書に変換"""
    return {
        "SNS": PRESETS["SNS"],
        "Novel": PRESETS["Novel"],
        "Report": PRESETS["Report"]
    }

def save_exclusion_list(data: Dict):
    """カスタム除外リストを保存"""
    with open(EXCLUSION_FILE, 'w', encoding='utf-8') as f:
        json.dump(data, f, ensure_ascii=False, indent=2)

# ============================================================================
# メイン UI
# ============================================================================
col1, col2 = st.columns([2, 1])

with col1:
    st.subheader("📖 テキストを貼り付けてください")
    text_input = st.text_area(
        "ここに文章を貼り付け",
        height=250,
        placeholder="小説、ブログ、レポート... なんでも",
        label_visibility="collapsed"
    )

with col2:
    st.subheader("⚙️ 設定")

    # PRESETS選択
    presets = load_presets()
    selected_preset = st.selectbox(
        "プリセット選択",
        ["カスタム", "SNS", "Novel", "Report"],
        help="出力フォーマットとパラメータが自動調整されます"
    )

    # パラメータ
    if selected_preset != "カスタム":
        min_count = presets[selected_preset]["min_count"]
        st.info(f"**{selected_preset}**: min_count={min_count}")
    else:
        min_count = st.slider("最小出現回数", 1, 20, 5)

    # 分析対象選択
    analysis_types = st.multiselect(
        "分析対象",
        ["書き癖検出", "会話分析", "文体診断", "表記ゆれ"],
        default=["書き癖検出", "会話分析"],
        help="複数選択可能"
    )

# ============================================================================
# 実行ボタン
# ============================================================================
if st.button("🚀 分析開始", type="primary", use_container_width=True):
    if not text_input.strip():
        st.error("テキストを入力してください。")
    else:
        st.divider()

        # 前処理
        sentences = text_input.split('\n')
        sentences = [s.strip() for s in sentences if s.strip()]
        text_joined = '\n'.join(sentences)

        tabs = st.tabs(["📊 結果"] + analysis_types + ["💾 除外管理"])

        # ============================================================================
        # 1. 書き癖検出
        # ============================================================================
        if "書き癖検出" in analysis_types:
            with tabs[analysis_types.index("書き癖検出") + 1]:
                st.subheader("🎯 あなたの「手癖」が見つかりました")

                try:
                    # PhraseExtracter で抽出
                    extractor = PhraseExtracter(
                        min_count=min_count,
                        max_length=16,
                        use_pmi=True
                    )
                    df_phrases = extractor.get_dfphrase(sentences)

                    if df_phrases.empty:
                        st.warning("フレーズが見つかりませんでした。テキストをもう少し長くしてください。")
                    else:
                        # WritingHabitDetector
                        habit_detector = WritingHabitDetector()
                        habits = habit_detector.detect(df_phrases)

                        if habits:
                            # トップ10を表示
                            col_habits = st.columns([2, 1, 1, 1])
                            col_habits[0].markdown("**フレーズ**")
                            col_habits[1].markdown("**スコア**")
                            col_habits[2].markdown("**度数**")
                            col_habits[3].markdown("**除外**")

                            st.divider()

                            # 除外リスト取得
                            exclusion = load_exclusion_list()

                            for idx, (phrase, score, count) in enumerate(habits[:10]):
                                cols = st.columns([2, 1, 1, 1])

                                with cols[0]:
                                    st.write(f"**{idx+1}. {phrase}**")

                                with cols[1]:
                                    st.metric("", f"{score:.3f}", label_visibility="collapsed")

                                with cols[2]:
                                    st.metric("", f"{count}回", label_visibility="collapsed")

                                with cols[3]:
                                    if st.button("❌", key=f"exclude_{idx}", help="除外リストに追加"):
                                        if phrase not in exclusion["phrases"]:
                                            exclusion["phrases"].append(phrase)
                                            save_exclusion_list(exclusion)
                                            st.success(f"「{phrase}」を除外リストに追加しました")

                            # ダウンロード
                            st.divider()
                            df_export = pd.DataFrame(
                                habits[:10],
                                columns=["フレーズ", "スコア", "度数"]
                            )
                            csv = df_export.to_csv(index=False, encoding='utf-8-sig')
                            st.download_button(
                                "📥 CSVでダウンロード",
                                csv,
                                "habits.csv",
                                "text/csv"
                            )
                        else:
                            st.info("特に強い手癖は検出されませんでした。")

                except Exception as e:
                    st.error(f"エラーが発生しました: {e}")

        # ============================================================================
        # 2. 会話分析
        # ============================================================================
        if "会話分析" in analysis_types:
            with tabs[analysis_types.index("会話分析") + 1]:
                st.subheader("💬 会話 vs 地の文のバランス")

                try:
                    analyzer = DialogueAnalyzer()
                    result = analyzer.analyze(text_joined, extract_features=True)

                    # メトリクス表示
                    col_metrics = st.columns(4)
                    col_metrics[0].metric("総文字数", result["total_characters"])
                    col_metrics[1].metric("会話比率", f"{result['dialogue_ratio']:.1%}")
                    col_metrics[2].metric("地の文比率", f"{result['narrative_ratio']:.1%}")
                    col_metrics[3].metric("会話数", result["dialogue_count"])

                    # グラフ
                    st.divider()
                    ratio_data = {
                        "会話文": result["dialogue_ratio"] * 100,
                        "地の文": result["narrative_ratio"] * 100
                    }
                    st.bar_chart(ratio_data)

                    # 特徴語
                    if result.get("dialogue_keywords"):
                        st.markdown("**会話文の特徴語:**")
                        st.write(", ".join(result["dialogue_keywords"][:10]))

                    if result.get("narrative_keywords"):
                        st.markdown("**地の文の特徴語:**")
                        st.write(", ".join(result["narrative_keywords"][:10]))

                    # サマリー
                    st.divider()
                    st.info(analyzer.get_summary(result))

                except Exception as e:
                    st.error(f"エラーが発生しました: {e}")

        # ============================================================================
        # 3. 文体診断
        # ============================================================================
        if "文体診断" in analysis_types:
            with tabs[analysis_types.index("文体診断") + 1]:
                st.subheader("✍️ あなたの「文体」の特徴")

                try:
                    analyzer = StylometryAnalyzer()

                    col_style = st.columns(3)

                    with col_style[0]:
                        st.markdown("**語彙の豊かさ**")
                        vocab = analyzer.analyze_vocabulary_richness(text_joined)
                        st.metric("TTR", f"{vocab['ttr']:.3f}", help="Type-Token Ratio: 高いほど語彙豊か")
                        st.metric("Yule's K", f"{vocab['yules_k']:.1f}")
                        st.success(vocab["assessment"])

                    with col_style[1]:
                        st.markdown("**文字種比率**")
                        char_type = analyzer.analyze_char_type_ratio(text_joined)
                        style_data = {
                            "漢字": char_type["kanji_ratio"] * 100,
                            "ひらがな": char_type["hiragana_ratio"] * 100,
                            "カタカナ": char_type["katakana_ratio"] * 100,
                        }
                        st.bar_chart(style_data)

                    with col_style[2]:
                        st.markdown("**文長統計**")
                        sent = analyzer.analyze_sentence_length(text_joined)
                        st.metric("平均文長", f"{sent['avg_length']:.1f}字")
                        st.metric("文の数", sent['sentence_count'])
                        st.metric("標準偏差", f"{sent['std_length']:.1f}")

                    st.divider()
                    st.info(f"**文体**: {char_type['style_type']}")

                except Exception as e:
                    st.error(f"エラーが発生しました: {e}")

        # ============================================================================
        # 4. 表記ゆれ
        # ============================================================================
        if "表記ゆれ" in analysis_types:
            with tabs[analysis_types.index("表記ゆれ") + 1]:
                st.subheader("🔄 表記ゆれを検出")

                try:
                    detector = OrthographyVariantDetector(similarity_threshold=0.75)
                    issues = detector.check(text_joined)

                    if issues:
                        st.warning(f"{len(issues)}件の表記ゆれが見つかりました")

                        for issue in issues[:20]:  # Top 20
                            with st.expander(
                                f"**{issue.get('type', 'unknown')}**: {' / '.join(issue.get('variants', [])[:3])}"
                            ):
                                st.write(issue.get('message', ''))

                        # ダウンロード
                        summary = detector.get_summary(issues)
                        st.download_button(
                            "📥 レポートをダウンロード",
                            summary,
                            "orthography_report.txt",
                            "text/plain"
                        )
                    else:
                        st.success("表記ゆれは検出されませんでした！")

                except Exception as e:
                    st.error(f"エラーが発生しました: {e}")

        # ============================================================================
        # 5. 除外管理
        # ============================================================================
        with tabs[-1]:
            st.subheader("🚫 除外リスト管理")

            exclusion = load_exclusion_list()

            st.markdown("**除外済みフレーズ**")
            if exclusion["phrases"]:
                for phrase in exclusion["phrases"]:
                    col_ex = st.columns([4, 1])
                    with col_ex[0]:
                        st.write(f"• {phrase}")
                    with col_ex[1]:
                        if st.button("削除", key=f"remove_{phrase}"):
                            exclusion["phrases"].remove(phrase)
                            save_exclusion_list(exclusion)
                            st.rerun()
            else:
                st.info("除外フレーズはありません")

            # 手動追加
            st.divider()
            new_phrase = st.text_input("新しいフレーズを追加")
            if st.button("追加") and new_phrase.strip():
                if new_phrase not in exclusion["phrases"]:
                    exclusion["phrases"].append(new_phrase)
                    save_exclusion_list(exclusion)
                    st.success(f"「{new_phrase}」を追加しました")
                    st.rerun()

# ============================================================================
# フッター
# ============================================================================
st.divider()
st.markdown("""
**japhrase v0.2.0** | Made by Takeshi SHIMIZU
- GitHub: https://github.com/tshim1zu/phrase-project
- Docs: TEXT_MINING_GUIDE.md 参照
""")
