# Fuente de datos: Inglés por función laboral (EF EPI)

## Dataset
**EF English Proficiency Index (EF EPI)**
Edición 2025 — hoja de datos Ecuador

## Proveedor
Education First (EF)

## Acceso
Sitio oficial: https://www.ef.com/wwen/epi/

Archivo PDF fuente: `data/sources/ef-epi-fact-sheet-ecuador-english.pdf`

Archivo utilizado: `data/raw/ef_epi/ef_epi_ecuador_extracted.xlsx`, hoja `Job_Functions_Exact`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `job_function` | Función o rol laboral |
| `score` | Puntaje EF EPI (escala 0–800) |
| `proficiency_band` | Banda de nivel: Very low / Low / Moderate / High |

## Bandas de nivel

| Banda | Rango de puntaje |
|---|---|
| Very high | ≥ 600 |
| High | 550–599 |
| Moderate | 500–549 |
| Low | 450–499 |
| Very low | < 450 |

## Valores de referencia

- Puntaje Ecuador (general): 466 (Low)
- Promedio global: 488 (Low)
- Ranking global Ecuador: 83/116

## Notas
- Los valores de `Job_Functions_Exact` son extraídos directamente de texto visible en el PDF (método: `exact_text`), no estimados visualmente.
- Script de limpieza: `scripts/data-cleaning/clean_ef_epi_job.R`
- Script de gráfico: `scripts/plots/plot_ef_epi_job.R`
