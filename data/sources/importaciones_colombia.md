# Importaciones mensuales desde Colombia

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

## Scripts del gráfico 50

- Limpieza: `scripts/data-cleaning/clean_bce_importaciones_colombia.R`.
- Gráfico: `scripts/plots/plot_50_bce_importaciones_colombia.R`.
- Salida: `outputs/figures/50_importaciones-colombia-ecuador.png`.
