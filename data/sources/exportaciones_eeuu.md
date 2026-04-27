# Exportaciones de Ecuador a Estados Unidos por producto

## Fuente principal

- Banco Central del Ecuador (BCE), Estadísticas de Comercio Exterior de Bienes.

## Archivo usado

- `data/raw/exportaciones/05. Export. por Producto Principal y País.xlsx` (`Columnas`)

## Nota metodológica

- El script `clean_exportaciones_eeuu.R` lee la hoja `Columnas` a partir de la fila de encabezados reales.
- Se filtra `País Destino == "ESTADOS UNIDOS"` y los años `2024` y `2025`.
- El archivo descargado reporta `FOB` en miles de USD.
- Para el gráfico, los valores se convierten a millones de USD dividiendo por `1000`.
- Se agrupan `PETRÓLEO CRUDO` y `DERIVADOS DE PETRÓLEO` como `PETRÓLEO CRUDO Y DERIVADOS`.
- Se agrupan `PESCADO` y `ENLATADOS DE PESCADO` como `PESCADOS Y ENLATADOS`.
- El ranking se construye con el valor FOB acumulado de `2024 + 2025`.
