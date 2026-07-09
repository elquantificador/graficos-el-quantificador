# AGENTS.md — El Quantificador chart repo

This file describes the repo's conventions for AI coding agents.

## Purpose

This repo contains reproducible data visualizations published by [El Quantificador](https://elquantificador.org). Each chart is a pair of R scripts: one that cleans data and one that renders the figure.

## Repository layout

```
graficos-el-quantificador/
├── data/
│   ├── raw/            # Raw datasets, organized by source (versioned)
│   ├── processed/      # Derived .rds files from clean_*.R (gitignored)
│   └── sources/        # Dataset inventory and methodological notes
├── outputs/
│   ├── figures/        # Published PNG files (versioned, numbered NN_slug-ecuador.png)
│   ├── tables/         # Generated Excel/HTML (not versioned)
│   └── chart_catalog/  # chart_catalog.csv (manual catalog consumed by the website)
├── scripts/
│   ├── packages.R      # ensure_packages() helper — auto-installs from CRAN
│   ├── utils.R         # Shared theme, formatters, add_logo()
│   ├── data-cleaning/  # clean_*.R scripts
│   └── plots/          # plot_*.R scripts
└── quantificador.png   # Logo used by add_logo()
```

## Naming conventions

| Artifact | Pattern | Example |
|----------|---------|---------|
| Raw data folder | `data/raw/[source_abbreviation]/` | `data/raw/enighur/` |
| Processed file | `data/processed/[source]_[topic].rds` | `data/processed/enighur_ingreso_gasto.rds` |
| Clean script | `scripts/data-cleaning/clean_[source]_[topic].R` | `clean_enighur_ingreso_gasto.R` |
| Plot script | `scripts/plots/plot_[source]_[topic].R` | `plot_enighur_ingreso_gasto.R` |
| Output PNG (portrait) | `outputs/figures/NN_[slug]-ecuador.png` | `20_descomposicion-ingreso-hogar-ecuador.png` |

`NN` is the chart's sequential number in the catalog (zero-padded to 2 digits).

## Script conventions

### Header block (required on every script)

```r
# ============================================================
# clean_foo_bar.R  (or plot_foo_bar.R)
# One-line description of what this script does.
# Requiere: data/raw/source/file.rds
# Guarda:   data/processed/foo_bar.rds   (or outputs/figures/NN_slug.png)
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/data-cleaning/clean_foo_bar.R
# ============================================================
```

### Package management

Always use the shared helper — never `install.packages()` or bare `library()` at the top level:

```r
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))
```

### Paths

All paths are **relative to the repo root**. Scripts must be run from the root:

```r
# Good
readRDS("data/raw/enighur/cuadro_2_1_1_ingresos.rds")
saveRDS(result, "data/processed/enighur_ingreso_gasto.rds")

# Bad — never hardcode absolute paths or use path-detection hacks
base_dir <- normalizePath(file.path(getwd(), "some_folder"), mustWork = FALSE)
```

### Clean scripts (`clean_*.R`)

1. Source `scripts/packages.R` and call `ensure_packages()`
2. Read from `data/raw/[source]/`
3. Process and save a single `.rds` to `data/processed/`
4. End with `message("Guardado: ", out_path)`

### Plot scripts (`plot_*.R`)

1. Source `scripts/utils.R` **and** `scripts/packages.R`, call `ensure_packages()`
2. Read from `data/processed/`
3. Build plot with `ggplot2`
4. Apply logo: `p_final <- add_logo(p_base)`
5. Save with `ragg::agg_png` at 300 dpi to `outputs/figures/`
6. End with `message("Guardado: ", out_path)`

### Output sizing

The standard canvas is **4 × 5 inches at 300 dpi**. Never use other sizes without a specific reason.

```r
# Standard (default)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300, device = ragg::agg_png)

```

### Typography

House typography is fixed for the standard canvas. Do not vary sizes chart by chart unless there is a specific layout reason.

| Element | 4 × 5 standard |
|---------|------------------|
| `plot.title` | 12–12.5 pt, bold |
| `plot.subtitle` | 9 pt |
| `plot.caption` | 5.5–6.5 pt |
| `geom_text` / `annotate` labels | 2.6–3.5 (ggplot units) |
| `theme_classic(base_size)` | default |

### Line breaking

Line breaks should target a **fixed visual width** anchored on the house title width, not arbitrary per-chart character counts.

- Use `wrap_title_house()` for titles.
- Use `wrap_subtitle_house()` for subtitles.
- Use `wrap_caption_house()` for captions.
- These helpers are calibrated so title, subtitle, and caption lines occupy roughly the same horizontal space on the 4 × 5 canvas, despite different font sizes.
- Do not use ad hoc `str_wrap()` widths unless a specific chart needs an optical override after rendering.
- Do not insert manual `\n` line breaks by default. Only use manual breaks when the automatic house wrap still produces visibly unbalanced lines.

Wrap the entire raw caption as a single string — never split into separate wrap calls per paragraph:

```r
# Good — one call, full raw text using house helper
caption_txt <- wrap_caption_house(
  "Fuente: ... Elaborado por ... Nota: ... Otros incluye ...",
)

# Bad — separate wrap calls per paragraph joined with paste()
caption_txt <- paste(
  wrap_caption_house("Fuente: ..."),
  wrap_caption_house("Nota: ..."),
  sep = "\n"
)
```

### Logo sizing

House style is fixed: `x = 0.88, width = 0.09, height = 0.09` on every canvas. Only `y` varies per chart — position it just above the caption, without overlapping it. The `utils.R` defaults already encode this house style, so a plain `add_logo(p_base)` call is correct unless a specific chart needs a non-default `y`.

```r
# Typical call — only override y
p_final <- add_logo(p_base, x = 0.88, y = 0.18)
```

If you change the default in `utils.R`, re-render **all** charts that rely on the default.

### Minimal plot script skeleton

```r
source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "scales", "ragg"))

out_path <- "outputs/figures/NN_slug-ecuador.png"
df <- readRDS("data/processed/foo_bar.rds")

p_base <- ggplot(df, aes(...)) + ... + theme_quantificador()
p_final <- add_logo(p_base)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave(out_path, plot = p_final, width = 4, height = 5, dpi = 300, device = ragg::agg_png)
message("Guardado: ", out_path)
```

## Shared utilities (`scripts/utils.R`)

| Function | Purpose |
|----------|---------|
| `theme_quantificador()` | Standard ggplot2 theme (classic, grey text, no grid) |
| `theme_women()` | Variant for gender/women charts (theme_bw, purple palette) |
| `label_number_intl(...)` | International number format (. thousands, , decimal) |
| `label_percent_intl(...)` | International percent format |
| `label_dollar_intl(...)` | International dollar format |
| `percent_intl(x, ...)` | Inline percent text formatter |
| `wrap_title_house(text, ...)` | Wraps titles to the house visual width |
| `wrap_subtitle_house(text, ...)` | Wraps subtitles to the house visual width |
| `wrap_caption_house(text, ...)` | Wraps captions to the house visual width |
| `add_logo(plot, ...)` | Overlays the El Quantificador logo using cowplot |

## How to add a new chart (checklist)

1. Place raw data in `data/raw/[source]/`
2. Write `scripts/data-cleaning/clean_[source]_[topic].R`
3. Write `scripts/plots/plot_[source]_[topic].R`
4. Run both from repo root and verify output at `outputs/figures/NN_slug-ecuador.png`
5. Add a row to `outputs/chart_catalog/chart_catalog.csv`
6. Add entry to the chart list in `README.md`
7. Add a row to `data/sources/README.md`

## Chart catalog schema

The website sync consumes `outputs/chart_catalog/chart_catalog.csv`.

- `chart_catalog.csv` is the single manually maintained source of truth for the website sync.
- `Image Filename` must be preserved for backward compatibility.
- `Image Path` must be filled manually and point to the true published PNG under `outputs/figures/`.
- `Author` must be a plain display name only. Do not add slugs or URLs.

## Running scripts

Always from the **repo root**:

```bash
Rscript scripts/data-cleaning/clean_enighur_ingreso_gasto.R
Rscript scripts/plots/plot_enighur_ingreso_gasto.R
```

## What not to do

- Do not create self-contained chart subfolders (e.g. `grafico1/`) — all scripts and data live in the centralized layout above.
- Do not use bare `library()` or `install.packages()` — use `ensure_packages()`.
- Do not hardcode absolute paths.
- Do not export SVG to `outputs/figures/` — PNG only.
- Do not commit files under `data/processed/` (gitignored).
- Do not add per-chart README files — the central `README.md` and `data/sources/README.md` cover documentation.
