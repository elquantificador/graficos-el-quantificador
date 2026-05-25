# Ecuatorianos más altos ganan más

## Fuente

- Archivo crudo: `data/raw/ensanut/1_BDD_ENS2018_f1_personas.dta.zip`
- Archivo procesado: `data/processed/ecuatorianos_altos_ensanut_2018.rds`
- Script de limpieza: `scripts/data-cleaning/clean_ecuatorianos_altos.R`
- Script de gráfico: `scripts/plots/plot_ecuatorianos_altos.R`

## Construcción

- Población: personas mestizas entre 40 y 50 años.
- Estatura: promedio de las mediciones `f1_s7_6_1`, `f1_s7_6_2` y `f1_s7_6_3`.
- Ingreso: ingreso laboral mensual reportado en `f1_s3_18`, transformado con `log()`.
