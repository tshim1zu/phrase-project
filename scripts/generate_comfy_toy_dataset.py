"""
ComfyUI プロンプト最適化用の人工検証データセット生成スクリプト
Good 10枚 + Bad 10枚 の構造化されたプロンプトを生成
"""

from pathlib import Path
import json

# Good Settings - 強力な品質タグ + 明確な画風指定
GOOD_PROMPTS = [
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, blue hair, twin tails, holding magic staff, library background, looking at viewer",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, red hair, long hair, casting fire magic, forest background, dynamic pose",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, silver hair, short hair, reading spell book, magic tower background, sitting",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, blonde hair, braided hair, flying on broom, night sky background, full moon",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, pink hair, ponytail, mixing potion, alchemy lab background, smile",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, black hair, hime cut, summoning familiar, magic circle background, serious face",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, white hair, messy hair, sleeping on desk, classroom background, closed eyes",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, green hair, bob cut, gardening with magic, greenhouse background, sunlight",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, purple hair, drill hair, duel with rival, ruins background, fighting stance",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    },
    {
        "positive": "masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading, 1girl, witch, orange hair, side ponytail, shopping in town, fantasy street background, waving hand",
        "negative": "low quality, worst quality, bad anatomy, bad hands, text, error, missing fingers, extra digit, fewer digits, cropped, jpeg artifacts, signature, watermark, username, blurry"
    }
]

# Bad Settings - 品質タグ欠落 + ノイズ混入
BAD_PROMPTS = [
    {
        "positive": "1girl, witch, blue hair, twin tails, holding magic staff, library, rough sketch, monochrome, low resolution",
        "negative": "nsfw"
    },
    {
        "positive": "1girl, witch, red hair, long hair, fire magic, forest, blurry background, out of focus, jpeg artifacts",
        "negative": "text"
    },
    {
        "positive": "1girl, witch, silver hair, short hair, reading book, tower, draft, unfinished, simple background",
        "negative": "low quality"
    },
    {
        "positive": "1girl, witch, blonde hair, broom, night sky, grainy, noise, bad anatomy, mutated hands",
        "negative": ""
    },
    {
        "positive": "1girl, witch, pink hair, potion, lab, distorted face, bad eyes, sketch style",
        "negative": "normal quality"
    },
    {
        "positive": "1girl, witch, black hair, magic circle, dark, underexposed, bad lighting, flat color",
        "negative": "bad art"
    },
    {
        "positive": "1girl, witch, white hair, sleeping, desk, cropped, head out of frame, worst quality",
        "negative": "3d"
    },
    {
        "positive": "1girl, witch, green hair, garden, overexposed, too bright, washed out colors",
        "negative": "realism"
    },
    {
        "positive": "1girl, witch, purple hair, ruins, fighting, motion blur, shaky, amateur drawing",
        "negative": "photo"
    },
    {
        "positive": "1girl, witch, orange hair, town, waving, extra limbs, messy lines, doodle",
        "negative": "watermark"
    }
]


def create_toy_dataset_dir():
    """データセット格納ディレクトリ作成"""
    data_dir = Path(__file__).parent.parent / "data" / "toy_dataset"
    data_dir.mkdir(parents=True, exist_ok=True)
    return data_dir


def save_toy_dataset(output_dir: Path):
    """トイデータセットをテキストファイルに保存"""
    
    # Good プロンプト
    good_file = output_dir / "good_prompts.txt"
    with open(good_file, "w", encoding="utf-8") as f:
        for i, item in enumerate(GOOD_PROMPTS, 1):
            f.write(f"=== Good #{i} ===\n")
            f.write(f"Positive: {item['positive']}\n")
            f.write(f"Negative: {item['negative']}\n\n")
    print(f"✅ Good prompts saved: {good_file}")
    
    # Bad プロンプト
    bad_file = output_dir / "bad_prompts.txt"
    with open(bad_file, "w", encoding="utf-8") as f:
        for i, item in enumerate(BAD_PROMPTS, 1):
            f.write(f"=== Bad #{i} ===\n")
            f.write(f"Positive: {item['positive']}\n")
            f.write(f"Negative: {item['negative']}\n\n")
    print(f"✅ Bad prompts saved: {bad_file}")
    
    # JSON形式でも保存（プログラマティックアクセス用）
    json_file = output_dir / "toy_dataset.json"
    dataset = {
        "good": GOOD_PROMPTS,
        "bad": BAD_PROMPTS,
        "metadata": {
            "theme": "ファンタジー風の魔法使いの少女",
            "good_count": len(GOOD_PROMPTS),
            "bad_count": len(BAD_PROMPTS),
            "good_characteristics": [
                "強力な『共通品質タグ』",
                "明確な『画風指定』",
                "アレンジ部（キャラ・場所）のみ変化"
            ],
            "bad_characteristics": [
                "品質タグの欠落",
                "ノイズタグの混入（rough, draft, blurry等）",
                "弱いネガティブプロンプト"
            ],
            "expected_findings": {
                "winning_factors": [
                    "masterpiece",
                    "best quality",
                    "ultra detailed",
                    "cinematic lighting",
                    "8k resolution",
                    "anime style",
                    "cel shading"
                ],
                "failure_factors": [
                    "rough sketch",
                    "monochrome",
                    "blurry",
                    "draft",
                    "unfinished",
                    "bad anatomy",
                    "mutated hands",
                    "distorted face",
                    "sketch style"
                ]
            }
        }
    }
    
    with open(json_file, "w", encoding="utf-8") as f:
        json.dump(dataset, f, ensure_ascii=False, indent=2)
    print(f"✅ JSON format saved: {json_file}")
    
    return good_file, bad_file, json_file


def extract_positive_only(output_dir: Path):
    """Positive プロンプトのみを抽出（差分分析用）"""
    
    good_text = "\n".join([item["positive"] for item in GOOD_PROMPTS])
    bad_text = "\n".join([item["positive"] for item in BAD_PROMPTS])
    
    good_pos_file = output_dir / "good_positive.txt"
    with open(good_pos_file, "w", encoding="utf-8") as f:
        f.write(good_text)
    
    bad_pos_file = output_dir / "bad_positive.txt"
    with open(bad_pos_file, "w", encoding="utf-8") as f:
        f.write(bad_text)
    
    print(f"✅ Good positive-only: {good_pos_file}")
    print(f"✅ Bad positive-only: {bad_pos_file}")
    
    return good_pos_file, bad_pos_file


if __name__ == "__main__":
    print("🧪 ComfyUI トイデータセット生成\n")
    
    output_dir = create_toy_dataset_dir()
    print(f"📁 Output directory: {output_dir}\n")
    
    save_toy_dataset(output_dir)
    print()
    extract_positive_only(output_dir)
    
    print("\n✅ Dataset generation complete!")
    print(f"   Total: {len(GOOD_PROMPTS)} good + {len(BAD_PROMPTS)} bad = {len(GOOD_PROMPTS) + len(BAD_PROMPTS)} prompts")
