# Ingreso mensual de los hogares, ENEMDU marzo 2026

El gráfico 16 muestra la distribución ponderada del ingreso mensual del hogar y el porcentaje de hogares con ingreso de USD 1.500 o más.

## Fuente y archivo

Instituto Nacional de Estadística y Censos (INEC), Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), microdatos de marzo de 2026. El archivo utilizado es `data/raw/enemdu/enemdu_persona_2026_03.sav`. El [portal de ENEMDU](https://www.ecuadorencifras.gob.ec/empleo-encuesta-nacional-de-empleo-desempleo-y-subempleo-enemdu/) proporciona acceso a la encuesta.

## Cálculo

El script agrupa a las personas por `id_hogar` y suma, para cada componente, los valores observados de sus integrantes. El ingreso total suma ingresos laborales, de capital, transferencias, pensiones y bonos. Se excluye un hogar si alguno de los componentes tiene el código de ingreso no informado `-1` o `999999`, o si no tiene ningún componente observado. Se conservan los ceros observados; los componentes vacíos se omiten al sumar.

El porcentaje con ingresos inferiores a USD 1.500 es `100 × suma de fexp de esos hogares / suma de fexp de todos los hogares seleccionados`. El porcentaje de USD 1.500 o más es su complemento. Por ejemplo, si los pesos de los hogares inferiores suman 80 y el total es 100, el porcentaje inferior es 80 % y el complementario es 20 %. El percentil 60 y los porcentajes se calculan antes de limitar el histograma al percentil 99. Las barras agrupan ingresos en intervalos de USD 100 y usan `fexp` como ponderador.

## Reproducción

`scripts/data-cleaning/clean_enemdu_ingreso_hogar.R` guarda `data/processed/enemdu_ingreso_hogar_2026_03.rds` y tablas resumen en `outputs/tables/`. `scripts/plots/plot_16_enemdu_ingreso_hogar_distribution.R` genera el PNG.
