# ENIGHUR 2024-2025 — ingresos y gastos del hogar

## Gráfico asociado

- `outputs/figures/20_descomposicion-ingreso-hogar-ecuador.png`
- Script de limpieza: `scripts/data-cleaning/clean_enighur_ingreso_gasto.R`
- Script de visualización: `scripts/plots/plot_enighur_ingreso_gasto.R`

## Archivos fuente utilizados

- `data/raw/enighur/cuadro_2_1_1_ingresos.rds`
- `data/raw/enighur/cuadro_2_1_3_gastos.rds`
- `data/raw/enighur/cuadro_2_2_1_promedios.rds`
- `data/raw/enighur/mapeo_categorias_gasto.rds`

## Descripción

Estos insumos contienen tabulados de la Encuesta Nacional de Ingresos y Gastos de los Hogares Urbanos y Rurales (ENIGHUR) 2024-2025 del INEC. Se usan para estimar el ingreso monetario mensual promedio del hogar, el gasto corriente, el gasto de no consumo y el ahorro implícito, además de desagregar el gasto corriente en categorías analíticas propias de El Quantificador.

## Transformaciones principales

- Se toma el total nacional de ingreso corriente total del hogar y de ingreso corriente monetario del hogar.
- Se toma el total nacional de gasto corriente de consumo y de gasto de no consumo.
- Se usa el promedio nacional del cuadro 2.2.1 para convertir los totales tabulados en montos promedio mensuales por hogar.
- Se calcula el ahorro como la diferencia entre ingreso monetario, gasto corriente y gasto de no consumo.
- Se agrupan rubros de gasto ENIGHUR usando `mapeo_categorias_gasto.rds` para construir las categorías finales del Sankey.

## Fuente institucional

- INEC Ecuador, Encuesta Nacional de Ingresos y Gastos de los Hogares Urbanos y Rurales (ENIGHUR) 2024-2025.
- Artículo asociado: https://elquantificador.blog/post/economia/2026-06-01-enighur-ingresos-gastos-ecuador/
