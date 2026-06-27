# Aceptación de la orientación sexual o identidad de género

## Fuente

- Archivo crudo: `data/raw/lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx`
- Archivo procesado: `data/processed/lgbti_aceptacion_orientacion_identidad_2025.rds`
- Script de limpieza: `scripts/data-cleaning/clean_lgbti_aceptacion_orientacion_identidad.R`
- Script de gráfico: `scripts/plots/plot_lgbti_aceptacion_orientacion_identidad.R`

## Construcción

- Encuesta base: Encuesta Nacional de Condiciones de Vida de la Población LGBTI+ 2025.
- Población de referencia: personas LGBTI+ encuestadas en la ENCV LGBTI+ 2025.
- Base usada en este repo: `6.657` observaciones en el archivo crudo versionado.
- Variables usadas: `s08_p01_1_1a`, `s08_p01_2_1a`, `s08_p01_5_1a`, `s08_p01_7_1a` y `s08_p01_8_1a`.
- Grupos mostrados: madre, padre, hermanas/os, amigas/os y compañeras/os de estudio o trabajo.
- Concepto medido: aceptación de la identidad de género u orientación sexual de la persona encuestada, según tipo de relación.
- Estimación: proporciones ponderadas con `fexp` usando `srvyr`.
- Exclusiones: respuestas `No aplica` y `No sabe`.

## Verificación del título

- La sintaxis fuente de INEC para estos tabulados identifica `s08_p01_1_1a` como `Madre` y `s08_p01_2_1a` como `Padre`, dentro del bloque `Aceptación de la orientación sexual o identidad de género`.
- En la tabulación reproducida para este repo, `Padre` registra la menor aceptación total (`54,3%`) y `Madre` la segunda menor (`61,2%`), por debajo de hermanas/os, amigas/os y compañeras/os de estudio o trabajo.
- El título `Los padres y madres son quienes menos aceptan la orientación sexual de sus hijos LGBTI+` resume ese hallazgo para la población encuestada.
- Precisión metodológica: la variable medida en la encuesta es `identidad de género u orientación sexual`; la frase `de sus hijos LGBTI+` es una inferencia editorial basada en que la encuesta se aplica a personas LGBTI+ y las categorías `Madre` y `Padre` se refieren a la relación de esas personas encuestadas.
