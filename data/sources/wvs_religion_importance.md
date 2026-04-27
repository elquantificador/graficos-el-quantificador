# Fuente de datos: Importancia de la religión

## Dataset
**World Values Survey**
Tabulación de la pregunta sobre importancia de la religión en la vida

## Proveedor
World Values Survey Association

## Archivo utilizado

- `data/raw/wvs/wvs_importance_of_religion_in_life.xls`

## Variables construidas

| Variable | Descripción |
|---|---|
| `geography` | Total o país/región reportada en la tabla |
| `response` | Categoría de respuesta |
| `share` | Participación de la respuesta |
| `sample_size` | Tamaño muestral reportado en la tabla |
| `estimated_n` | Conteo estimado a partir de la participación |

## Notas

- Script asociado: `scripts/data-cleaning/clean_wvs_religion_importance.R`.
- La tabla se extrae desde una hoja Excel ya estructurada, no desde microdatos individuales.
- Para `Total`, el script separa porcentaje y `N` desde una misma celda.
