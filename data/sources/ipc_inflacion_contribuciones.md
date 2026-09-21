# Contribuciones a la inflación anual de Ecuador

## Fuente

Instituto Nacional de Estadística y Censos (INEC), Índice de Precios al Consumidor (IPC), con información hasta agosto de 2026.

Los archivos de origen se conservan en `data/raw/ipc_inec_2026_06/` y `data/raw/ipc_inec_2026_08/`. Para enero de 2022 a junio de 2026 se usa `Series Incidencias.zip`, que contiene `ipc_incid_nac_div_06_2026.zip`, y `Tabulados_y_series_historicas_CSV_2026_06.zip`. Para julio y agosto de 2026 se usa `Tabulados_y_series_historicas_CSV_2026_08.zip`, junto con la serie empalmada oficial de INEC en `Series_empalmadas_2026_07.zip`.

La página oficial del IPC es:

<https://www.ecuadorencifras.gob.ec/indice-de-precios-al-consumidor-2026/>

## Método

El script de limpieza usa la incidencia anual oficial hasta junio de 2026 y calcula la incidencia anual con índices y ponderaciones de la nueva base para julio y agosto de 2026. La incidencia se agrupa en siete componentes:

- alimentos y bebidas: división 01;
- vivienda y servicios básicos: división 04;
- gasolina: gasolina ecológica, gasolina de bajo octanaje y gasolina de alto octanaje, códigos 07221248, 07221249 y 07221250 hasta junio de 2026;
- resto del transporte: incidencia oficial de la división 07 menos la contribución calculada para gasolina;
- bienes y servicios diversos: división 12;
- restaurantes y hoteles: división 11;
- otras divisiones: suma de las divisiones 02, 03, 05, 06, 08, 09 y 10.

La contribución de gasolina se calcula con la ponderación oficial de cada producto y su variación anual de índice: `ponderación × (índice_t / índice_t-12 - 1) × 100`. Desde julio de 2026 la nueva base identifica los productos 0722201 y 0722202, pero la serie empalmada publicada por INEC no incluye índices históricos a nivel de producto. Por eso, para julio y agosto se distribuye la contribución anual de la clase 0722 según el peso de esos dos productos de gasolina en la nueva canasta. Esta aproximación queda explicada en el pie del gráfico.

La fila general se calcula con la serie empalmada y los índices de la nueva base. La nueva canasta tiene 13 divisiones, por lo que la agrupación `Otras divisiones` incluye también la división 13 desde julio de 2026. En la nueva base, la suma de las incidencias calculadas por división puede diferir ligeramente de la inflación general por la agregación de índices y ponderaciones. El gráfico incorpora esa diferencia residual en `Otras divisiones` para que las barras cierren con la línea.

El gráfico presenta enero de 2022 a agosto de 2026. Una línea vertical marca el cambio de base, canasta y clasificación del IPC en julio de 2026. El gráfico muestra la información nueva, pero conserva la advertencia metodológica sobre la aproximación de gasolina.

## Reproducción

- Limpieza: `scripts/data-cleaning/clean_inec_inflacion_contribuciones.R`
- Visualización: `scripts/plots/plot_inec_inflacion_contribuciones.R`
- Salida: `outputs/figures/49_contribuciones-inflacion-ecuador.png`
