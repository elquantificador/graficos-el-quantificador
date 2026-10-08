# Comercio mensual con Colombia

Insumos para la fila 47 del [Pipeline de gráficos El Quantificador](https://docs.google.com/spreadsheets/d/1IOItcPLqwQPnJEDEns2jIh2QUVGZgzUNEwV5FXOrYS4/edit#gid=1696996476): «¿Cómo afectó la guerra comercial a las importaciones desde Colombia?».

## Fuente y cobertura

Banco Central del Ecuador (BCE), estadísticas de comercio exterior de bienes. Consulta y descarga mediante EcuDataMCP el 1 de octubre de 2026, hora de Edmonton. Las respuestas JSON registran la consulta del 2 de octubre en UTC.

La serie descargada cubre enero de 2024 a julio de 2026, con 31 observaciones mensuales. Julio de 2026 es el último mes devuelto por BCEData al consultar hasta octubre de 2026. Los datos de junio que esperaba la ficha del pipeline ya están disponibles.

Los valores están en millones de dólares corrientes:

- FOB: valor de la mercancía, sin el transporte internacional ni el seguro.
- CIF: valor de la mercancía que incluye el transporte internacional y el seguro.

Por ejemplo, en junio de 2026 las importaciones desde Colombia sumaron USD 101,526868117 millones FOB y USD 105,141673488 millones CIF. Se conserva la precisión publicada por el BCE.

## Archivos

Todos están en `data/raw/importaciones_colombia/`.

| Archivo | Contenido |
|---|---|
| `bce_colombia_importaciones_mensuales_2024_2026.csv` | Serie de Colombia, un mes por fila, con valores FOB y CIF. |
| `bcedata_importaciones_fob_2024_2026.json` | Respuesta completa de BCEData, grupo 87, con Colombia y las demás series de países y agregados. |
| `bcedata_importaciones_cif_2024_2026.json` | Respuesta completa de BCEData, grupo 78, con Colombia y las demás series de países y agregados. |
| `IEM-318-e.xlsx` | Archivo oficial de importaciones FOB por país, boletín IEM 2094, agosto de 2026. Contiene enero de 2025 a julio de 2026. |
| `IEM-319-e.xlsx` | Archivo oficial de importaciones CIF por país del mismo boletín y período. |

El CSV contiene `periodo` en formato `YYYY-MM`, `pais_origen`, `fob_millones_usd` y `cif_millones_usd`. Usa coma como separador de columnas y punto decimal. Se extrajo la serie «Colombia» de ambos JSON, sin redondear, acumular ni calcular variaciones.

Los 19 valores mensuales de Colombia de enero de 2025 a julio de 2026 coinciden entre BCEData y los Excel oficiales para ambas medidas. El BCE indica que las tablas incluyen cifras de reexportaciones.

## Consulta de la fuente

En EcuDataMCP, `search_indicadores_bce(query="Colombia")` identifica los grupos 87 (FOB) y 78 (CIF). Para cada grupo se consultó `get_indicador_bce` con `desde="2024-01"`, `hasta="2026-10"`, `frecuencia="Mensual"` y `format="json"`.

`search_bce_iem(query="país de origen")` identifica las tablas `iem-318-e` e `iem-319-e` del [boletín IEM 2094](https://contenido.bce.fin.ec/documentos/PublicacionesNotas/Catalogo/IEMensual/Indices/m2094082026.html). Archivos originales: [FOB](https://contenido.bce.fin.ec/documentos/PublicacionesNotas/Catalogo/IEMensual/m2094/IEM-318-e.xlsx) y [CIF](https://contenido.bce.fin.ec/documentos/PublicacionesNotas/Catalogo/IEMensual/m2094/IEM-319-e.xlsx).

Para comparar años, usa los mismos meses en ambos períodos y mantén una sola medida, FOB o CIF, en cada serie del gráfico. Estos montos permiten describir la evolución de las importaciones; atribuir cambios a la guerra comercial requiere considerar también otros factores.

## Contexto de la medida aduanera

El [boletín oficial de SENAE](https://www.aduana.gob.ec/gaceta-boletin/aplicacion-de-la-resolucion-nro-senae-senae-2026-0006-re-acerca-de-tasa-por-servicio-aduanero-por-concepto-de-control-aduanero-a-las-mercancias-que-ingresen-desde-colombia/) informó que la tasa por servicio de control aduanero a las mercancías que ingresaran desde Colombia entró en vigor el 1 de febrero de 2026, bajo la Resolución SENAE-SENAE-2026-0006-RE. La página oficial actualmente indica que la resolución no está vigente. La coincidencia de fechas no permite atribuir a esta medida todo el cambio observado en importaciones.

## Scripts del gráfico 51

- Limpieza: `scripts/data-cleaning/clean_bce_comercio_colombia.R`.
- Gráfico: `scripts/plots/plot_51_bce_importaciones_colombia.R`.
- Salida: `outputs/figures/51_importaciones-colombia-ecuador.png`.

## Exportaciones hacia Colombia

La serie de exportaciones FOB procede del grupo 80 de BCEData, «Exportaciones FOB Mensuales por Continente y País Destino». EcuDataMCP consultó `get_indicador_bce(id_grupo=80, desde="2024-01", hasta="2026-10", frecuencia="Mensual", format="json")` el 2 de octubre de 2026. La respuesta contiene enero de 2024 a julio de 2026, con 31 meses.

En `data/raw/importaciones_colombia/` se conservan la respuesta completa `bcedata_exportaciones_fob_2024_2026.json`, la serie «Colombia» en `bce_colombia_exportaciones_mensuales_2024_2026.csv` y el [Excel oficial IEM-314-e del boletín 2094](https://contenido.bce.fin.ec/documentos/PublicacionesNotas/Catalogo/IEMensual/m2094/IEM-314-e.xlsx). El CSV usa `periodo` en formato `YYYY-MM`, `pais_destino` y `fob_millones_usd`.

El Excel contiene enero de 2025 a julio de 2026. De sus 19 valores de Colombia, 18 coinciden con BCEData. En marzo de 2026, el Excel contiene USD 53,5 millones y BCEData USD 53,50992748 millones. El gráfico usa BCEData para todos los meses, conservando la precisión de esa fuente.

Para comparar los años se suman enero a julio en cada uno y se calcula:

`Variación porcentual = (total enero-julio de 2026 / total enero-julio de 2025 - 1) × 100`.

Aquí, cada total suma los siete montos mensuales FOB de importaciones o de exportaciones, por separado. Las exportaciones sumaron USD 512,265198627 millones en 2025 y USD 460,908174554 millones en 2026: `(460,908174554 / 512,265198627 - 1) × 100 = -10,0254759%`, que se muestra como una caída de 10,0%. Las importaciones sumaron USD 1.076,349080355 y USD 733,087957020 millones, una caída de 31,9%. Se mantiene el mismo período y la misma medida FOB para ambos flujos.

## Recaudación de la tasa a Colombia

El 2 de octubre de 2026, EcuDataMCP localizó el [Excel de estadísticas de recaudación del SRI, agosto de 2026](https://www.sri.gob.ec/o/sri-portlet-biblioteca-alfresco-internet/descargar/93ba1615-410a-4067-bd8a-004b087fa28a/Estad%c3%adsticas%20de%20Recaudaci%c3%b3n_agosto2026.xlsx), mediante `search_archivos(fuente="sri_recaudacion", query="2026", format="json")`. Se conserva en `data/raw/recaudacion_colombia/sri_estadisticas_recaudacion_agosto2026.xlsx`.

La hoja «Recaudación abierta» contiene la fila 62, «Tasa Servicio Aduanero (9)». La nota 9, en A85, identifica la tasa sobre las mercancías importadas originarias de Colombia. El encabezado A4 indica miles de dólares; C6:J6 identifica enero a agosto de 2026. C62:J62 contiene los ocho montos mensuales y B62 el total acumulado.

El extracto `data/raw/recaudacion_colombia/sri_tasa_servicio_aduanero_colombia_2026_01_08.csv` conserva esos valores, las celdas originales y una conversión a millones de dólares, obtenida dividiendo entre 1.000. Se excluyeron septiembre a diciembre porque quedan fuera de la cobertura del archivo, aunque sus celdas contienen ceros.

Los ocho valores coinciden con G88 de las hojas mensuales correspondientes. Su suma coincide con B62 y con G88 de «Acum»: USD 150,19755561 millones. Febrero a mayo suma USD 149,41605951 millones. Junio, julio y agosto también registran cobros; el archivo no explica esos pagos, por lo que no permiten establecer por sí solos la vigencia de la tasa en esos meses. La fecha de recaudación tampoco identifica necesariamente el mes de la importación.

La nota del SRI menciona la tarifa inicial del 30%; no sirve como cronología de los cambios posteriores. Para documentar las tarifas y fechas, deben consultarse las resoluciones de SENAE.

La búsqueda de archivos de SENAE devolvió el catálogo histórico de 2012 a 2021. Las búsquedas en el portal nacional de datos abiertos fallaron con HTTP 403. La serie específica disponible para esta revisión se obtuvo del SRI.

## Gráfico definitivo en dólares nominales

El gráfico muestra importaciones desde Colombia y exportaciones hacia Colombia, de enero de 2024 a julio de 2026. Cada punto corresponde a un mes. Los valores se expresan en millones de dólares nominales FOB, sin ajuste por inflación.

`Monto mensual del gráfico = monto FOB publicado por BCEData, en millones de USD`.

Aquí, FOB mide el valor de la mercancía sin transporte internacional ni seguro. Por ejemplo, el punto de exportaciones de julio de 2026 representa USD 82,107720903 millones; el punto de importaciones del mismo mes representa USD 129,761418213 millones.

Al comparar los totales de enero a julio de 2026 con los mismos meses de 2025, las caídas son de 31,9% en importaciones y 10,0% en exportaciones, según la fórmula explicada en «Exportaciones hacia Colombia». Las dos líneas muestran los montos mensuales de los 31 meses disponibles.

El limpiador `scripts/data-cleaning/clean_bce_comercio_colombia.R` lee los CSV de importaciones y exportaciones. Guarda `data/processed/bce_comercio_colombia.rds`, con las columnas `fecha`, `serie` y `valor_millones_usd`: 31 meses por serie y 62 filas en total.

El gráfico `scripts/plots/plot_51_bce_importaciones_colombia.R` lee ese archivo y guarda `outputs/figures/51_importaciones-colombia-ecuador.png`. El eje vertical comienza en USD 25 millones. Cada serie se identifica con una etiqueta de su color en un espacio libre dentro del área de trazado. No hay leyenda ni líneas de cuadrícula. La línea punteada marca el 1 de febrero de 2026. Los datos de recaudación se conservan como material complementario.

Ejecuta desde la raíz del repositorio:

```powershell
Rscript scripts/data-cleaning/clean_bce_comercio_colombia.R
Rscript scripts/plots/plot_51_bce_importaciones_colombia.R
```
