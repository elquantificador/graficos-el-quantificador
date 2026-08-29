# IPC por ciudad, enero de 2021–junio de 2026

## Fuente

Instituto Nacional de Estadística y Censos (INEC), Índice de Precios al Consumidor, tabulados de las series IPC nacional, regional y por ciudad, con corte en junio de 2026. La ficha oficial fue localizada con EcuDataMCP en el catálogo ANDA del INEC: `IDD-ECU-INEC-CGTPE-DECON-IPC-2026-v2.2`.

Los archivos fueron entregados por Leonor Molina Zapata como parte de su postulación al concurso Ecuador Quantificado 2026. El paquete original incluye los CSV descargados de los tabulados del INEC y una tabla de contenido de esos archivos.

## Pregunta y población

¿En qué ciudades aumentó más el nivel general de precios entre enero de 2021 y junio de 2026? La comparación incluye Guayaquil, Esmeraldas, Machala, Manta, Santo Domingo, Quito, Loja, Cuenca y Ambato.

## Construcción

El script `scripts/data-cleaning/clean_ipc_ciudades_leonor.R` selecciona la fila `Nivel = General` y `Descripción CCIF = GENERAL` en cada archivo de ciudad. Después transforma los meses a formato largo y conserva las 66 observaciones mensuales entre enero de 2021 y junio de 2026.

La variación acumulada se calcula como:

`(IPC del periodo / IPC de enero de 2021 - 1) × 100`

El ranking usa el primer y último dato de cada ciudad. El heatmap muestra cortes de diciembre de cada año y junio de 2026, siempre como variación respecto de enero de 2021.

## Reproducción

- Limpieza: `scripts/data-cleaning/clean_ipc_ciudades_leonor.R`
- Heatmap: `scripts/plots/plot_ipc_ciudades_heatmap_leonor.R`
- Ranking: `scripts/plots/plot_ipc_ciudades_ranking_leonor.R`
- Salidas: `outputs/figures/41_a_costo-vida-ciudades-heatmap.png` y `outputs/figures/41_b_costo-vida-ciudades-ranking.png`

## Limitaciones

Las visualizaciones comparan índices de precios, no niveles de gasto ni el costo absoluto de una canasta específica. La diferencia entre ciudades describe la evolución acumulada del índice general durante el periodo observado.
