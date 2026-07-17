# R review: final title and logo placement

## Final status

**Ready for use.** The revised title is accurate and editorially stronger, the subtitle is synchronized with the catalog, and the logo uses the house x-position and size with a chart-specific `y = 0.14` placement immediately above the caption. The regenerated 4 x 5 inch chart is clean and has no visible clipping or overlap.

## Summary counts by severity

| Severity | Count |
|---|---:|
| Critical | 0 |
| High | 0 |
| Medium | 0 |
| Low | 0 |

No remaining findings.

## Focused verification

- `scripts/plots/plot_jobs_habilidades_mensuales.R:24-26` sets the exact title to `Excel sobresale: aparece en el 69% de las ofertas`. The underlying processed result contains 262 Excel mentions among 377 eligible postings (`262 / 377 = 69.496%`), which the chart's whole-percentage formatter displays as 69%. The claim is therefore supported by the plotted data.
- `scripts/plots/plot_jobs_habilidades_mensuales.R:27-29` sets the exact subtitle to `Porcentaje de ofertas que menciona cada herramienta técnica; cinco principales, Ecuador, junio-julio de 2026`.
- The title wraps after `de` into two visually balanced lines. Its bold 12.5 pt house styling establishes a clear hierarchy over the 9 pt subtitle, while the subtitle provides the measure, scope, geography, and period without repeating the headline claim.
- `scripts/plots/plot_jobs_habilidades_mensuales.R:70` calls `house_apply_logo(p_base, "portrait", y = 0.14)`. Through `add_logo()`, the omitted arguments retain the house defaults `x = 0.88`, `width = 0.09`, and `height = 0.09`; only the vertical position is overridden. Visual inspection confirms that the logo sits just above and to the right of the caption without covering the x-axis title, tick labels, or caption text.
- The five lollipop marks remain correctly ranked and labelled: Excel 69%, Power BI 24%, SQL 14%, SAP 11%, and Python 6%. The 3.4-size markers, stems, percentage labels, category labels, and scale are unobstructed.
- The x- and y-axis lines remain the same `grey60` colour, y-axis tick marks remain hidden, and the zero baseline and 0%-80% ticks render cleanly.
- The caption remains fully visible and states the three scraped sites, 745 collected postings, June-July 2026 period, technical-skill scope, multiple-response caveat, and authorship. The logo does not overlap it.
- The script reran successfully from the repository root with R 4.5.2 and regenerated `outputs/figures/32_habilidades-demandadas-ecuador.png`. Only non-fatal package build-version warnings were emitted.
- The regenerated PNG is 1200 x 1500 pixels at approximately 300 dpi, matching the standard 4 x 5 inch portrait canvas. Visual inspection found no clipping, collision, or unintended hierarchy change.
- The catalog row for Chart ID 32 exactly matches the script title and subtitle. `python scripts/validate_chart_catalog.py` completed successfully with `Catalog OK: 34 rows validated.`

## Checklist summary

| Category | Result | Notes |
|---|---|---|
| Header and package discipline | Pass | Uses the repository-required header, shared utilities, and `ensure_packages()` workflow. |
| Relative paths and output creation | Pass | Input and output paths are relative; the output directory is created before saving. |
| Data and title correctness | Pass | The 69% title claim agrees with 262 of 377 eligible postings after display rounding. |
| Title and subtitle hierarchy | Pass | Exact requested copy, balanced wrapping, and clear title/subtitle distinction. |
| Logo placement | Pass | House defaults `x = 0.88`, `width = 0.09`, `height = 0.09`; `y = 0.14` is cleanly placed above the caption. |
| Axes, marks, and caption | Pass | Existing styling and content remain legible and unaffected. |
| Output specification | Pass | Explicit 4 x 5 inch, 300 dpi PNG output is preserved through `house_spec("portrait")`. |
| Plot execution | Pass | Script completed successfully and emitted the expected single `Guardado:` message. |
| Catalog synchronization | Pass | Chart ID 32 title/subtitle match the plot; all 34 catalog rows validate. |
| Visual QA | Pass | No clipping, overlap, or crowding in the final render. |
| Downstream risk | Pass | Review found no source, catalog, or processed-data defect requiring a change. |
