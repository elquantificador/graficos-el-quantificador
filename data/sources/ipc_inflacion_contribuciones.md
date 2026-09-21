# Contribuciones a la inflación anual

## Fuente

Instituto Nacional de Estadística y Censos (INEC), Índice de Precios al Consumidor (IPC), corte de agosto de 2026.

Los insumos están en `data/raw/ipc_inec_2026_08/`: los tabulados de agosto y las series empalmadas publicadas con el cambio de base de julio de 2026.

La fuente oficial es <https://www.ecuadorencifras.gob.ec/indice-de-precios-al-consumidor-2026/>.

## Método

El gráfico cubre enero de 2022 a agosto de 2026. Se muestran las divisiones 01, 04, 07, 11 y 12, y se agrupan las demás en `Otras divisiones`.

`Combustibles y lubricantes` corresponde a la clase 0722 del INEC. `Resto del transporte` es la contribución de transporte menos esa clase.

Desde julio de 2026 el INEC usa una nueva base, canasta y clasificación. El script combina las series empalmadas con los tabulados de agosto. Las contribuciones se calculan a partir de los índices y ponderaciones vigentes y se normalizan al total de inflación anual publicado, de modo que las barras reconcilien con la línea sin agregar una categoría residual.

## Reproducción

- Limpieza: `scripts/data-cleaning/clean_inec_inflacion_contribuciones.R`
- Visualización: `scripts/plots/plot_inec_inflacion_contribuciones.R`
- Salida: `outputs/figures/49_contribuciones-inflacion-ecuador.png`
