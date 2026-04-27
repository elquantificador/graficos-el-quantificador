# Fuente de datos: Uso del tiempo y cocina

## Dataset
**Encuesta Específica de Uso del Tiempo 2019**

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Archivos utilizados

- `data/raw/uso_tiempo/201912_multibdd_uso_del_tiempo.sav.csv`
- `data/raw/uso_tiempo/201912_multibdd_personas.sav.csv`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `id_per` | Identificador de persona |
| `s1p2` | Sexo |
| `s1p3` | Edad |
| `s51p2` | Realiza actividades de preparación de alimentos |
| `s51p2a`, `s51p2b`, `s51p2c`, `s51p2d` | Horas y minutos dedicados a cocinar |
| `ciudad` | Código geográfico |
| `upm` | Unidad primaria de muestreo |
| `estrato` | Estrato |
| `fexp` | Factor de expansión |

## Notas

- Script asociado: `scripts/data-cleaning/clean_uso_tiempo.R`.
- El script une la base de personas con la base específica de uso del tiempo por `id_per`.
- La variable `t_horas_cocina` se construye sumando horas y minutos reportados.
