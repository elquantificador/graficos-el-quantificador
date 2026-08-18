# Fuentes de datos: personal del MSP y población provincial

## Dataset

1. **Registro de Actividades y Recursos de Salud (RAS), serie del Ministerio de Salud Pública.**
2. **Estimaciones y Proyecciones de Población de Ecuador, Revisión 2024, INEC.**

La búsqueda de población se realizó con el MCP de datos de Ecuador. El catálogo
ANDA del INEC confirmó la disponibilidad de estadísticas poblacionales oficiales.
Para mantener el mismo año del RAS, se usó el tabulado provincial de la Revisión
2024, que contiene estimaciones para 1990-2022 y proyecciones posteriores.

## Archivos usados

El gráfico usa la serie provincial del RAS y una tabla pequeña con la estimación
provincial de población para 2021. Antes de graficar, el script valida que las
sumas de las cuatro categorías de personal coincidan con la serie nacional y con
las series agregadas por cantón, parroquia y área:

- `data/raw/ras/msp_serie_nac.rds`
- `data/raw/ras/msp_serie_prov.rds`
- `data/raw/ras/msp_serie_cant.rds`
- `data/raw/ras/msp_serie_parr.rds`
- `data/raw/ras/msp_serie_area.rds`
- `data/raw/inec/estimaciones_poblacion_provincial_2021.csv`

Archivo original del INEC:

- `Provincial.zip`: https://www.ecuadorencifras.gob.ec/documentos/web-inec/Poblacion_y_Demografia/Proyecciones_Poblacionales/censo_2022/revision_2024_areas/Provincial.zip
- Libro usado: `Tabulado_provincial_edad_quinquenal_1990-2035.xlsx`
- Hojas usadas: las 24 hojas provinciales con sufijo `_n` (población total).
- Celda usada en cada hoja: total provincial del año 2021.

## Variables graficadas

| Variable | Descripción |
|---|---|
| `tmedicos` | Médicos |
| `tenf` | Enfermeros |
| `tobst` | Obstetrices |
| `ttaps` | Técnicos de Atención Primaria en Salud (TAPS) |
| `poblacion_2021` | Población provincial estimada al 30 de junio de 2021 |

## Notas metodológicas

- El año común de las dos fuentes es 2021.
- El indicador principal suma médicos, enfermeros, obstetrices y TAPS y lo divide
  para la población provincial estimada; se expresa por cada 10.000 habitantes.
- Las provincias se ordenan de mayor a menor disponibilidad de personal.
- La figura muestra un único ranking de barras horizontales, ordenado de mayor a
  menor disponibilidad de personal.
- Las estimaciones de población de la Revisión 2024 están referidas al 30 de junio
  de cada año.

## Scripts y figura

- Visualización: `scripts/plots/plot_ras_personal_salud_provincial.R`
- Figura: `outputs/figures/38_personal-salud_provincia-ecuador.png`
