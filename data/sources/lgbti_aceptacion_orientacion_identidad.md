# Aceptación de la orientación sexual o identidad de género

## Fuente

- Archivo crudo: `data/raw/lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx`
- Archivo procesado: `data/processed/lgbti_aceptacion_orientacion_identidad_2025.rds`
- Script de limpieza: `scripts/data-cleaning/clean_lgbti_aceptacion_orientacion_identidad.R`
- Script de gráfico: `scripts/plots/plot_lgbti_aceptacion_orientacion_identidad.R`

## Construcción

- Encuesta base: Encuesta Nacional de Condiciones de Vida de la Población LGBTI+ 2025.
- Variables usadas: `s08_p01_1_1a`, `s08_p01_2_1a`, `s08_p01_5_1a`, `s08_p01_7_1a` y `s08_p01_8_1a`.
- Grupos mostrados: madre, padre, hermanas/os, amigas/os y compañeras/os de estudio o trabajo.
- Estimación: proporciones ponderadas con `fexp` usando `srvyr`.
- Exclusiones: respuestas `No aplica` y `No sabe`.
