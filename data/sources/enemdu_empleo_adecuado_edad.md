# Empleo adecuado por edad, ENEMDU mensual

El gráfico 13 muestra la variación interanual del número de personas con empleo adecuado por grupo de edad, entre marzo de 2025 y marzo de 2026.

## Fuente y archivo

Instituto Nacional de Estadística y Censos (INEC), Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), tabulados de marzo de 2026. El portal de resultados es <https://www.ecuadorencifras.gob.ec/enemdu-2026/>.

El archivo utilizado es `data/raw/enemdu/202603_Tabulados_Mercado_Laboral_EXCEL (2).xlsx`. El script lee el total de empleo adecuado de la hoja `1. Poblaciones` y las participaciones por edad de `3.2 Caracterización Adec_pleno`.

## Cálculo

Para cada mes y edad, el nivel se obtiene multiplicando el total de personas con empleo adecuado por la participación del grupo de edad. Para 25 a 44 años se suman las participaciones de dos grupos de la tabla. La variación interanual es `100 × (nivel del mes / nivel del mismo mes del año anterior − 1)`. Por ejemplo, un nivel de 900 frente a 1.000 un año antes da −10 %.

Se leen los meses de enero de 2024 a marzo de 2026 para poder comparar con el año anterior. El gráfico presenta de marzo de 2025 a marzo de 2026. La serie procede de los tabulados publicados y no es una nueva estimación de microdatos.

## Reproducción

`scripts/data-cleaning/clean_enemdu_empleo_adecuado_edad.R` guarda `data/processed/enemdu_empleo_adecuado_edad_yoy.rds`. `scripts/plots/plot_13_enemdu_empleo_adecuado_edad_nivel_yoy_mensual_raw.R` produce el PNG del gráfico 13.
