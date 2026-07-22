# Fuente de datos: horas promedio trabajadas por sexo y sector

## Dataset

**Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)**

Serie 2018-2026 para personas de 15 años o más.

## Proveedor

Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso

Portal del INEC: https://www.ecuadorencifras.gob.ec/empleo-encuesta-nacional-de-empleo-desempleo-y-subempleo-enemdu/

Archivos utilizados:

- `data/raw/enemdu/ENEMDU_PERSONAS_2018_12_hom.sav`
- `data/raw/enemdu/enemdu_persona_201912.sav`
- `data/raw/enemdu/enemdu_persona_2020_12.sav`
- `data/raw/enemdu/enemdu_persona_2021_12.sav`
- `data/raw/enemdu/enemdu_persona_2022_12.sav`
- `data/raw/enemdu/enemdu_persona_2023_12.sav`
- `data/raw/enemdu/enemdu_persona_2024_12.sav`
- `data/raw/enemdu/enemdu_persona_2025_12.sav`
- `data/raw/enemdu/enemdu_persona_2026_l_trimestre.sav`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `p24` | Horas efectivas trabajadas en la semana |
| `p02` | Sexo |
| `p03` | Edad en años cumplidos |
| `secemp` | Sector de empleo |
| `p05a`, `p05b` | Variables de seguridad social usadas para imputar `secemp` faltante |

## Notas

- La serie 2018-2025 usa microdatos de diciembre de cada año.
- Para 2026, el valor anual se calcula con el archivo de personas del I trimestre de 2026.
- El universo se restringe a personas de 15 años o más.
- Las observaciones con `secemp` faltante se imputan usando la tenencia de seguridad social, siguiendo la lógica del script original de Eddie Tomalá.
