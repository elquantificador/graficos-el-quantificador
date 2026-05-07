# Fuente de datos: Anemia infantil por quintil (ENDI)

## Dataset
**Encuesta Nacional de Desnutrición Infantil — Segunda Ronda (ENDI R2)**
Corte 2023-2024

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Portal del INEC: https://www.ecuadorencifras.gob.ec/encuesta-nacional-sobre-desnutricion-infantil/

Archivo utilizado: `data/raw/endi_r2/BDD_ENDI_R2_f1_personas.rds`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `ane6_23_new` | Prevalencia de anemia en niñas/os de 6 a 23 meses de edad |
| `quintil` | Quintil de bienestar del hogar |
| `id_upm` | Unidad primaria de muestreo |
| `estrato` | Estrato de muestreo |
| `fexp` | Factor de expansión |

## Diseño muestral
Encuesta con diseño complejo: estratificado, por conglomerados y con pesos muestrales. Implementado con `survey::svydesign()` usando `id_upm` como UPM, `estrato` como estrato y `fexp` como factor de expansión.

## Indicador graficado
Prevalencia ponderada de anemia para la población de 6 a 23 meses, desagregada por quintiles de bienestar, con intervalos de confianza al 95%.

El script también calcula un estimado nacional agregado y lo guarda por separado en `data/processed/endi_r2_prev_anemia_overall.rds`.

## Notas

- Script de limpieza: `scripts/data-cleaning/clean_endi_anemia_quintil.R`
- Script de gráfico: `scripts/plots/plot_endi_anemia_quintil.R`
- El indicador `ane6_23_new` solo está disponible para la subpoblación relevante; el resto de observaciones se excluye mediante `NA`.
- El archivo por quintil contiene la prevalencia, su intervalo de confianza y un conteo simple `n` por grupo. Ese `n` no reemplaza la estimación ponderada.
