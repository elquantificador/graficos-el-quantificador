# Fuentes de datos: distribución urbano-rural del personal del MSP

## Dataset

**Registro de Actividades y Recursos de Salud (RAS), serie del Ministerio de Salud Pública.**

## Archivos usados

El gráfico usa la serie por área del RAS entre 2013 y 2021. Antes de graficar, el
script valida que las sumas de obstetrices y TAPS coincidan con la serie nacional
y con las series agregadas por provincia, cantón, parroquia y área:

- `data/raw/ras/msp_serie_nac.rds`
- `data/raw/ras/msp_serie_prov.rds`
- `data/raw/ras/msp_serie_cant.rds`
- `data/raw/ras/msp_serie_parr.rds`
- `data/raw/ras/msp_serie_area.rds`

## Variables graficadas

| Variable | Descripción |
|---|---|
| `tobst` | Obstetrices |
| `ttaps` | Técnicos de Atención Primaria en Salud (TAPS) |
| `area` | Área urbana o rural, según las etiquetas del RAS |

## Notas metodológicas

- El período graficado es 2013-2021; la serie de TAPS aparece con valores positivos
  desde 2013.
- El área se interpreta con las etiquetas incorporadas en el RAS: `Urbano` y
  `Rural`.
- La participación rural de cada ocupación es el personal ubicado en áreas rurales
  dividido para el total de esa ocupación en cada año.
- La figura muestra la evolución de esa participación para obstetrices y TAPS.

## Scripts y figura

- Visualización: `scripts/plots/plot_ras_personal_salud_provincial.R`
- Figura: `outputs/figures/38_personal-salud_provincia-ecuador.png`
