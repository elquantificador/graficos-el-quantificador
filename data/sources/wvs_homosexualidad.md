# Fuente de datos: Actitudes hacia la homosexualidad

## Dataset
**World Values Survey (WVS), Ecuador**
Microdatos de la ronda aplicada en Ecuador para 2013 y 2018.

## Proveedor
World Values Survey Association

## Archivo utilizado

- `data/raw/wvs/WVSEcuador.dta`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `S020` | Año o ronda de la encuesta |
| `X001` | Sexo de la persona encuestada |
| `S017` | Peso muestral |
| `A124_09` | Preferiría no tener a un homosexual como vecino |
| `D081` | Opinión sobre si las parejas homosexuales son tan buenos padres como otras parejas |

## Archivos construidos

- `data/processed/wvs_homosexualidad_vecinos.rds`
- `data/processed/wvs_homosexualidad_padres.rds`

## Notas

- Scripts asociados:
  `scripts/data-cleaning/clean_wvs_homosexualidad_vecinos.R`,
  `scripts/data-cleaning/clean_wvs_homosexualidad_padres.R`.
- Ambos scripts usan `S017` (`Weight`) como peso muestral para calcular proporciones ponderadas.
- Los cálculos parten del archivo `WVSEcuador.dta`, manteniendo las definiciones de variables del repositorio histórico `WVSEcuador`.
