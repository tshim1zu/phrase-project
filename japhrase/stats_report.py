# coding: utf-8
"""
Generate a minimal HTML report from stats data.
"""

from __future__ import annotations

from html import escape
from typing import Dict, List


def _svg_bar_chart(
    items: List[Dict],
    label_key: str,
    value_key: str,
    width: int = 720,
    height: int = 320,
    bar_color: str = "#2a9d8f",
) -> str:
    if not items:
        return "<svg width=\"{0}\" height=\"{1}\"></svg>".format(width, height)

    max_value = max(item[value_key] for item in items) or 1
    padding = 40
    chart_width = width - padding * 2
    chart_height = height - padding * 2
    bar_width = chart_width / max(len(items), 1)

    bars = []
    labels = []
    for idx, item in enumerate(items):
        value = item[value_key]
        bar_height = (value / max_value) * chart_height
        x = padding + idx * bar_width
        y = padding + (chart_height - bar_height)
        bars.append(
            f'<rect x="{x:.1f}" y="{y:.1f}" width="{bar_width - 4:.1f}" '
            f'height="{bar_height:.1f}" fill="{bar_color}"></rect>'
        )
        label = escape(str(item[label_key]))
        labels.append(
            f'<text x="{x + 2:.1f}" y="{height - 8}" font-size="10" '
            f'fill="#333" transform="rotate(45 {x + 2:.1f},{height - 8})">{label}</text>'
        )

    axis = (
        f'<line x1="{padding}" y1="{padding}" x2="{padding}" y2="{height - padding}" '
        f'stroke="#444" stroke-width="1" />'
        f'<line x1="{padding}" y1="{height - padding}" x2="{width - padding}" '
        f'y2="{height - padding}" stroke="#444" stroke-width="1" />'
    )

    return (
        f'<svg width="{width}" height="{height}" viewBox="0 0 {width} {height}" '
        f'role="img" aria-label="Bar chart">'
        + axis
        + "".join(bars)
        + "".join(labels)
        + "</svg>"
    )


def render_stats_html(stats_data: Dict, title: str = "Phrase Stats Report") -> str:
    summary = stats_data.get("summary", {})
    frequency = stats_data.get("frequency", {})
    length = stats_data.get("length", {})
    diversity = stats_data.get("diversity", {})
    top_phrases = stats_data.get("top_phrases", [])

    bar_chart = _svg_bar_chart(top_phrases, "phrase", "frequency")

    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{escape(title)}</title>
  <style>
    :root {{
      --bg: #f6f2ea;
      --panel: #fffaf2;
      --ink: #2f2a24;
      --accent: #2a9d8f;
      --muted: #7a6f63;
    }}
    body {{
      margin: 0;
      font-family: "Spectral", "Georgia", serif;
      background: radial-gradient(circle at top, #f2e6d8 0%, var(--bg) 60%);
      color: var(--ink);
    }}
    main {{
      max-width: 980px;
      margin: 40px auto;
      padding: 24px;
    }}
    h1 {{
      font-size: 32px;
      margin: 0 0 16px 0;
      letter-spacing: 0.5px;
    }}
    .panel {{
      background: var(--panel);
      border: 1px solid #e7d7c6;
      border-radius: 14px;
      padding: 18px;
      margin-bottom: 20px;
      box-shadow: 0 10px 24px rgba(60, 45, 30, 0.12);
    }}
    .stats-grid {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
      gap: 12px;
    }}
    .stat {{
      background: #fff;
      border-radius: 12px;
      padding: 10px 12px;
      border: 1px solid #efe1d2;
    }}
    .stat h3 {{
      margin: 0 0 6px 0;
      font-size: 12px;
      color: var(--muted);
      text-transform: uppercase;
      letter-spacing: 0.08em;
    }}
    .stat p {{
      margin: 0;
      font-size: 18px;
      font-weight: 600;
    }}
    .chart {{
      overflow-x: auto;
    }}
  </style>
</head>
<body>
  <main>
    <h1>{escape(title)}</h1>
    <div class="panel">
      <div class="stats-grid">
        <div class="stat"><h3>Total Phrases</h3><p>{summary.get("total_phrases", 0)}</p></div>
        <div class="stat"><h3>Text Lines</h3><p>{summary.get("text_lines", 0)}</p></div>
        <div class="stat"><h3>Frequency Mean</h3><p>{frequency.get("mean", 0):.2f}</p></div>
        <div class="stat"><h3>Length Mean</h3><p>{length.get("mean", 0):.2f}</p></div>
        <div class="stat"><h3>Entropy</h3><p>{diversity.get("entropy", 0):.3f}</p></div>
      </div>
    </div>
    <div class="panel">
      <h2>Top Phrases</h2>
      <div class="chart">{bar_chart}</div>
    </div>
  </main>
</body>
</html>
"""
