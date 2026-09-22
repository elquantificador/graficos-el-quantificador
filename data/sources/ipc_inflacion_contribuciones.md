# Contribuciones a la inflación anual

## Fuente

Instituto Nacional de Estadística y Censos (INEC), Índice de Precios al Consumidor (IPC), corte de agosto de 2026.

Los insumos son `data/raw/ipc_inec_2026_06/Series Incidencias.zip`, con las incidencias oficiales hasta junio de 2026, y los tabulados de julio y agosto junto con las series empalmadas de `data/raw/ipc_inec_2026_08/`.

La fuente oficial es <https://www.ecuadorencifras.gob.ec/indice-de-precios-al-consumidor-2026/>.

## Método

El gráfico cubre enero de 2022 a agosto de 2026. Se muestran las divisiones 01, 04, 07, 11 y 12, y se agrupan las demás en `Otras divisiones`.

Hasta junio de 2026, transporte se muestra como división completa. `Combustibles y lubricantes` corresponde a la clase 0722 del INEC y se desagrega únicamente desde julio de 2026, cuando entra la nueva canasta.

Desde julio de 2026 el INEC usa una nueva base, canasta y clasificación. El script combina las series empalmadas con los tabulados de agosto. Las contribuciones posteriores se calculan a partir de los índices y ponderaciones vigentes y se normalizan al total de inflación anual publicado. Son estimaciones reconstruidas, no incidencias anuales publicadas directamente por el INEC.

## Reproducción

- Limpieza: `scripts/data-cleaning/clean_inec_inflacion_contribuciones.R`
- Visualización: `scripts/plots/plot_inec_inflacion_contribuciones.R`
- Salida: `outputs/figures/49_contribuciones-inflacion-ecuador.png`
