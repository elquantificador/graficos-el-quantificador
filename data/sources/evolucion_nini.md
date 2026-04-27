# Fuente de datos: Evolución NINI

## Dataset
**Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)**
Series trimestrales, 2021 Q1 – 2025 Q4

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Portal del INEC: https://www.ecuadorencifras.gob.ec/empleo-encuesta-nacional-de-empleo-desempleo-y-subempleo-enemdu/

Archivos utilizados: `data/raw/enemdu/evolucion/enemdu_persona_YYYY_*_trimestre.csv`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `p02` | Sexo (1 = Hombre, 2 = Mujer) |
| `p03` | Edad en años cumplidos |
| `p07` | Asiste actualmente a un establecimiento educativo |
| `p09` | Tipo de establecimiento educativo |
| `p20` | Trabajó la semana pasada |
| `p21` | Razón por la que no trabajó |
| `p22` | Disponibilidad para trabajar |
| `periodo` | Periodo en formato YYYYMM |
| `upm` | Unidad primaria de muestreo |
| `estrato` | Estrato de muestreo |
| `fexp` | Factor de expansión |

## Definición NINI
Una persona se clasifica como NINI si cumple todas las condiciones:
- No trabaja: `p20 == 2` y `p21 == 12` y `p22 == 2`
- No estudia: `p07 == 2` y no está en nivelación SENESCYT: `p09 != 6`

## Notas
- Se filtra la población de 15 a 34 años.
- El gráfico muestra únicamente datos desde enero 2024.
