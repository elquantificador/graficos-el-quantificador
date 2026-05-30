# Registro de Actividades y Recursos de Salud (RAS) - personal del MSP

## Archivo usado

- `data/raw/ras/msp_serie_nac.rds`

## Uso en el repositorio

- Script de limpieza: `scripts/data-cleaning/clean_ras_personal_salud.R`
- Script de visualización: `scripts/plots/plot_ras_personal_salud.R`
- Figura generada: `figures/18_personal-salud-publica-ecuador.png`

## Descripción

Serie nacional del Registro de Actividades y Recursos de Salud (RAS) usada para graficar la evolución del personal del Ministerio de Salud Pública en Ecuador entre 2006 y 2021.

El script de limpieza reorganiza las series de médicos, enfermeros, obstetrices y TAPS en formato largo y guarda el resultado como `data/processed/ras_personal_salud_nacional.rds`.
