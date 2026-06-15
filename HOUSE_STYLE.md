# House Style

This file is the enforceable visual specification for El Quantificador charts.

For Codex or any other agent:

- Follow this house style exactly.
- Do not introduce deviations, optical tweaks, or per-chart exceptions on your own.
- Only the user may authorize an exception.

## Canvas

- Standard export: `4 × 5 inches`
- Resolution: `300 dpi`
- Output format: `PNG`

## Typography

- `plot.title`: `12.5 pt`, bold
- `plot.subtitle`: `9 pt`
- `plot.caption`: `6.5 pt`
- `axis.text`: `7.5 pt`
- `axis.title.x`: `7 pt`
- `axis.title.y`: `7 pt`
- Default `geom_text()` / `annotate("text")` size: `3`

## Lineheight

- `plot.title`: default ggplot lineheight unless explicitly overridden
- `plot.subtitle`: `1.1`
- `plot.caption`: `1.1`

## Text Alignment

- Title: left-aligned
- Subtitle: left-aligned
- Caption: left-aligned

## Margins

- Default plot margin: `margin(6, 36, 6, 16)`
- Top: `6`
- Right: `36`
- Bottom: `6`
- Left: `16`

## Colors

- Primary text: `grey20`
- Secondary text: `grey30`
- Axis line: `grey60`

## Line Breaking

Wrap to a fixed visual width anchored on the title width.

- Title wrap width: `38`
- Subtitle wrap width: `60`
- Caption wrap width: `83`

These widths correspond to the shared helpers in `scripts/utils.R`:

- `wrap_title_house()`
- `wrap_subtitle_house()`
- `wrap_caption_house()`

Rules:

- Use the house wrap helpers by default.
- Do not choose ad hoc wrap widths.
- Do not insert manual `\n` line breaks unless the user explicitly requests them.
- Subtitle and caption widths are derived from the title reference width and the house font sizes.

## Captions

Caption content order must be:

1. `Fuente:`
2. `Elaboración:`
3. `Nota:`

Rules:

- Pass the full raw caption as a single string.
- Wrap the full caption once using the house caption helper.
- Do not split captions into separately wrapped paragraphs.
- Keep caption size at `6.5 pt`.

## Labels

- Default label size: `3`
- Applies to both `geom_text()` and `annotate("text")` unless the user decides otherwise

## Logo

- Default logo placement uses `add_logo()` from `scripts/utils.R`
- Fixed x-position: `x = 0.88`
- Fixed size: `width = 0.09`, `height = 0.09`
- Default y-position in the helper: `y = 0.07`
- Only `y` may vary by chart, and only when the user decides

## Enforcement

- Use the shared utilities in `scripts/utils.R`.
- Keep all charts on the house typography, margins, wrap widths, and logo defaults.
- Do not introduce one-off typography changes, margin changes, or wrapping changes autonomously.
- If a chart appears to need an exception, stop and ask the user.
