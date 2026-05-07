# Fuente de datos: Principal cuidador/a del niño entre semana (ENDI)

## Dataset
**Encuesta Nacional de Desnutrición Infantil — Segunda Ronda (ENDI R2)**
Corte 2023-2024

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Portal del INEC: https://www.ecuadorencifras.gob.ec/encuesta-nacional-sobre-desnutricion-infantil/

Archivo utilizado: `data/raw/endi_r2/BDD_ENDI_R2_f2_salud_ninez.rds`

## Variable utilizada

| Variable | Descripción |
|---|---|
| `f2_s5_512` | ¿Con quién permanece el niño o niña la mayor parte del tiempo de lunes a viernes? |
| `id_upm` | Unidad primaria de muestreo |
| `estrato` | Estrato de muestreo |
| `fexp` | Factor de expansión |

## Diseño muestral
Encuesta con diseño complejo: estratificado, por conglomerados y con pesos muestrales. Implementado con `survey::svydesign()` usando `id_upm` como UPM, `estrato` como estrato y `fexp` como factor de expansión.

## Indicador graficado
Distribución ponderada del principal cuidador o espacio de permanencia del niño o niña entre lunes y viernes.

## Universo
Niños y niñas menores de 5 años incluidos en el módulo `f2_salud_niñez`, con respuesta válida a la pregunta `f2_s5_512`.

## Notas

- Script de limpieza: `scripts/data-cleaning/clean_endi_cuidador_principal.R`
- Script de gráfico: `scripts/plots/plot_endi_cuidador_principal.R`
- La categoría `Padre o abuelos` agrupa `Padre` y `Abuelo, abuela`.
- La categoría `Otros` agrupa `Tíos/tías`, `Miembros del hogar de 10 años y más`, `Miembros del hogar menores de 10 años`, `Otros familiares, vecinos/as o amigos/as`, `Empleada o niñera` y `Se queda solo`.
- `Centro de Desarrollo Infantil (CDI)` se mantiene como categoría separada.
