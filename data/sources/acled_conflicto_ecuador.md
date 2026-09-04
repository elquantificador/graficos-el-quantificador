# Conflicto en Ecuador, 2018–2025

## Fuente

ACLED, Armed Conflict Location & Event Data Project. El CSV maestro fue entregado por Mayari Tapia por correo el 3 de septiembre de 2026:

- Archivo recibido: `acled_ecuador_maestro_20260903.csv`
- Ruta local: `data/raw/acled_conflicto_ecuador/acled_ecuador_maestro_20260903.csv`
- Script original de descarga y limpieza: [2.limpieza y descarga_acled_ecuador.R](https://huggingface.co/spaces/Seth77/Conflicto/blob/main/datos/2.limpieza%20y%20descarga_acled_ecuador.R)
- Diccionario de variables: [App del proyecto](https://huggingface.co/spaces/Seth77/Conflicto)
- Codebook: [ACLED Codebook](https://acleddata.com/methodology/acled-codebook)

El archivo se conserva localmente y no se versiona en Git por su tamaño.

## Cobertura y variables

El archivo contiene eventos de Ecuador con `event_id_cnty`, `event_date`, `year_month`, `sub_event_type`, `latitude` y `longitude`, entre otras variables. La base entregada contiene 13.226 eventos únicos con coordenadas, desde el 2 de enero de 2018 hasta el 8 de junio de 2025.

La pieza conserva el periodo declarado por la autora, 2018–2025, y documenta que el archivo recibido no contiene eventos posteriores al 8 de junio de 2025.

## Transformaciones

1. Se eliminan duplicados por `event_id_cnty`.
2. Se conservan eventos con coordenadas, subtipo y `year_month` dentro de 2018–2025.
3. Se recodifican los subtipos a las ocho categorías del concurso según `Metodología_Conflicto_Ecuador--1-.pdf`.
4. Para la visualización final se excluyen las protestas pacíficas. Los eventos restantes se agrupan por latitud, longitud y categoría. El tamaño del punto se limita a 15 para conservar legibilidad.
5. El mapa utiliza los segmentos de límites provinciales del Marco Geoestadístico 2022 del INEC, documentados en `inec_geoestadistico_2022.md`.

## Reproducción

```powershell
Rscript scripts/data-cleaning/clean_acled_conflicto_ecuador.R
Rscript scripts/plots/plot_acled_conflicto_ecuador.R
```

La salida es `outputs/figures/44_conflicto-tacticas-visual-pass-ecuador.png`.
