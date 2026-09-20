# Contribuciones a la inflación anual de Ecuador

## Fuente

Instituto Nacional de Estadística y Censos (INEC), Índice de Precios al Consumidor (IPC), serie oficial de incidencias nacionales con corte en junio de 2026.

Los archivos de origen se conservan en `data/raw/ipc_inec_2026_06/`. El ZIP `Series Incidencias.zip` contiene `ipc_incid_nac_div_06_2026.zip`, con las series de incidencia mensual, anual y acumulada por división de consumo. El ZIP `Tabulados_y_series_historicas_CSV_2026_06.zip` contiene la serie nacional detallada del IPC, usada para identificar los productos de gasolina.

La página oficial del IPC es:

<https://www.ecuadorencifras.gob.ec/indice-de-precios-al-consumidor-2026/>

## Método

El script de limpieza usa `2.INCID. ANUAL.csv`. La incidencia oficial se agrupa en siete componentes:

- alimentos y bebidas: división 01;
- vivienda y servicios básicos: división 04;
- gasolina: gasolina ecológica, gasolina de bajo octanaje y gasolina de alto octanaje, códigos 07221248, 07221249 y 07221250;
- resto del transporte: incidencia oficial de la división 07 menos la contribución calculada para gasolina;
- bienes y servicios diversos: división 12;
- restaurantes y hoteles: división 11;
- otras divisiones: suma de las divisiones 02, 03, 05, 06, 08, 09 y 10.

La contribución de gasolina se calcula con la ponderación oficial de cada producto y su variación anual de índice: `ponderación × (índice_t / índice_t-12 - 1) × 100`. La fila `Variación Anual Nacional` se conserva como la serie total que se muestra con una línea en el gráfico. La definición del resto del transporte hace que los siete componentes reconcilien con la incidencia oficial de la división 07 y con la inflación nacional, salvo los redondeos publicados por el INEC.

El gráfico presenta enero de 2022 a junio de 2026. Este corte pertenece al IPC con base 2014 = 100. El INEC cambió la base, la canasta y la clasificación del IPC desde julio de 2026, por lo que el gráfico no mezcla esa nueva serie con la anterior.

## Reproducción

- Limpieza: `scripts/data-cleaning/clean_inec_inflacion_contribuciones.R`
- Visualización: `scripts/plots/plot_inec_inflacion_contribuciones.R`
- Salida: `outputs/figures/49_contribuciones-inflacion-ecuador.png`
