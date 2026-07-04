# Conocimiento de la orientación sexual o identidad de género

## Fuente

- Archivo crudo: `data/raw/lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx`
- Archivo procesado: `data/processed/lgbti_conocen_orientacion_identidad_2025.rds`
- Script de limpieza: `scripts/data-cleaning/clean_lgbti_conocen_orientacion_identidad.R`
- Script de gráfico: `scripts/plots/plot_lgbti_conocen_orientacion_identidad.R`

## Construcción

- Encuesta base: Encuesta Nacional de Condiciones de Vida de la Población LGBTI+ 2025.
- Población de referencia: personas LGBTI+ encuestadas en la ENCV LGBTI+ 2025.
- Base usada en este repo: `6.657` observaciones en el archivo crudo versionado.
- Variables usadas: `s08_p01_1`, `s08_p01_2`, `s08_p01_5`, `s08_p01_7` y `s08_p01_8`.
- Grupos mostrados: madre, padre, hermanas/os, amigas/os y compañeras/os de estudio o trabajo.
- Concepto medido: si la persona cercana conoce la orientación sexual o identidad de género de la persona encuestada, según tipo de relación.
- Estimación: proporciones ponderadas con `fexp` usando `srvyr`. Respuestas `No aplica` excluidas.

## Verificación del título

- La variable `s08_p01_2` (`Padre`) registra el menor porcentaje de conocimiento (`59%`), por debajo de `Madre` (`74%`), `Hermanas/os`, `Amigas/os` (`85%`) y `Compañeras/os de estudio o trabajo`.
- El título `Los padres son quienes menos saben sobre la identidad LGBTI+ de sus hijos` resume ese hallazgo para la población encuestada.
- Precisión metodológica: las categorías `Madre` y `Padre` se refieren a la relación de la persona LGBTI+ encuestada; el conocimiento se infiere de la respuesta `sí` a la pregunta correspondiente del bloque `s08`.
