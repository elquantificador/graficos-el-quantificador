# Fuente de datos: Personal público de salud (RAS)

## Dataset

**Registro de Actividades y Recursos de Salud (RAS), serie nacional del MSP**

## Archivos usados en el repositorio

- Archivo crudo: `data/raw/ras/msp_serie_nac.rds`
- Archivo procesado: `data/processed/ras_personal_salud_nacional.rds`
- Figura generada: `outputs/figures/18_personal-salud-publica-ecuador.png`

## Variables derivadas usadas

El script `clean_ras_personal_salud.R` reorganiza la serie nacional a formato largo y conserva:

| Variable | Descripción |
|---|---|
| `anio` | Año de la observación |
| `ocupacion` | Categoría ocupacional (`Medicos`, `Enfermeros`, `Obstetrices`, `TAPS`) |
| `total` | Total anual registrado para cada categoría |

## Notas metodológicas

- La limpieza toma la serie nacional ya consolidada en `msp_serie_nac.rds`.
- El gráfico usa cuatro categorías de personal del MSP: médicos, enfermeros, obstetrices y TAPS.
- Los valores iguales a `0` se recodifican a `NA` antes de graficar para evitar falsas interrupciones de serie.

## Script asociado

- Limpieza: `scripts/data-cleaning/clean_ras_personal_salud.R`
- Visualización: `scripts/plots/plot_ras_personal_salud.R`
