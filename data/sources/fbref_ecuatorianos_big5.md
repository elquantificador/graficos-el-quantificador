# Ecuatorianos en las grandes ligas de Europa

## Fuente

- Archivo crudo: `data/raw/fbref/ecuatorianos_big5_minutos_2019_2026.csv`
- Archivo procesado: `data/processed/fbref_ecuatorianos_big5.rds`
- Script de limpieza: `scripts/data-cleaning/clean_fbref_ecuatorianos_big5.R`
- Script de gráfico: `scripts/plots/plot_fbref_ecuatorianos_big5.R`

## Construcción

- Fuente base: minutos por temporada extraídos de FBref para futbolistas ecuatorianos con participación en la Premier League, La Liga, Serie A, Bundesliga o Ligue 1.
- Unidad de análisis: jugador-temporada.
- Cobertura temporal: temporadas 2019/20 a 2025/26.
- El archivo crudo resume la tabulación manual usada para el gráfico; el script de limpieza valida el orden de temporadas y genera la tabla procesada para visualización.
