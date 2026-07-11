# ENIGHUR 2011-2012 y 2024-2025 - gasolina y transporte por quintil de ingreso

## Graficos asociados

- outputs/figures/31_a_gasolina-vs-transporte-publico_quintil-ingreso-ecuador.png
- outputs/figures/31_b_gasolina-share_quintil-ingreso-2012-2025.png
- Script de limpieza 31_a: scripts/data-cleaning/clean_enighur_gasolina_transporte_quintiles.R
- Script de visualizacion 31_a: scripts/plots/plot_enighur_gasolina_transporte_quintiles.R
- Script de limpieza 31_b: scripts/data-cleaning/clean_enighur_gasolina_share_quintiles_years.R
- Script de visualizacion 31_b: scripts/plots/plot_enighur_gasolina_share_quintiles_years.R

## Archivos fuente utilizados

- data/raw/enighur/enighur_gasolina_transporte_publico_quintiles_2025.csv
- data/raw/enighur/enighur_gasolina_share_quintiles_2012_2025.csv

## Descripcion

Estos insumos resumen dos comparaciones sobre la gasolina en los hogares ecuatorianos. La primera contrasta el gasto promedio en gasolina y transporte publico por quintil de ingreso en la ENIGHUR 2024-2025. La segunda compara el peso de la gasolina dentro del gasto monetario total del hogar entre la ENIGHUR 2011-2012 y la ENIGHUR 2024-2025, tambien por quintil.

## Transformaciones principales

- Se usan tablas resumen por quintil construidas a partir de la ENIGHUR.
- Para 31_a se comparan dos rubros: gasolina y servicios de transporte de pasajeros.
- Para 31_b se compara la participacion de la gasolina dentro del gasto monetario total del hogar entre dos rondas de la encuesta.
- En ambos casos, los quintiles ordenan a los hogares en cinco grupos de 20% segun su ingreso monetario.

## Fuente institucional

- INEC Ecuador, Encuesta Nacional de Ingresos y Gastos de los Hogares Urbanos y Rurales (ENIGHUR) 2011-2012.
- INEC Ecuador, Encuesta Nacional de Ingresos y Gastos de los Hogares Urbanos y Rurales (ENIGHUR) 2024-2025.
- Articulo asociado: https://elquantificador.blog/post/economia/2026-06-01-enighur-ingresos-gastos-ecuador/
