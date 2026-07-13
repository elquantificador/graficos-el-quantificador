# Chart catalog

`chart_catalog.csv` es la fuente de verdad para la sincronizacion del sitio.

## Columnas base

- `Chart Name`: titulo publico del grafico.
- `Subtitle`: subtitulo publico.
- `Date`: fecha de publicacion o fecha prevista en formato `YYYY-MM-DD`.
- `LinkedIn Link`: enlace a la publicacion original.
- `Image Filename`: nombre del PNG publicado.
- `Image Path`: ruta relativa al repo bajo `outputs/figures/`.
- `Author`: nombre visible del autor o autores.
- `Description`: copy editorial limpio, sin markdown de imagenes ni enlaces.
- `Script Link`: URL al script de ploteo en GitHub.

## Columnas de gestion

- `Chart ID`: identificador canonico del grafico. Ejemplos: `31`, `27_a`, `31_b`.
- `Status`: uno de `published`, `draft`, `supplementary`, `archived`, `hold`.
- `Series`: etiqueta corta opcional para series relacionadas. Ejemplo: `enighur-gasolina`.
- `Notes`: contexto editorial o notas operativas breves.

## Reglas

- `Image Filename` debe coincidir con el PNG realmente publicado.
- `Image Path` debe apuntar al archivo correcto dentro de `outputs/figures/`.
- `Description` no debe incluir markdown de imagenes, enlaces, hashtags o residuos de copy para redes.
- Los cambios fuertes de titulo pueden cambiar el slug downstream, asi que `Chart ID` debe ser estable aunque cambie el copy.
- Corre `python scripts/validate_chart_catalog.py` antes de commit o push.
