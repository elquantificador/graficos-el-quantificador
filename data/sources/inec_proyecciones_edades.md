# Composición por edad, estimaciones y proyecciones del INEC

El gráfico 34 presenta la distribución de la población nacional entre cinco grupos de edad de 1950 a 2050, a partir de las estimaciones y proyecciones de población del INEC.

## Fuente y archivo

Instituto Nacional de Estadística y Censos (INEC), [Estimaciones y Proyecciones de Población, revisión 2024](https://www.ecuadorencifras.gob.ec/estimaciones-y-proyecciones-de-poblacion-revision-2024/). El archivo utilizado es `data/raw/inec/tabul_nac_edad_sim_1950-2050.xlsx`, hoja `población_ambos_sexos`, rango `B18:CY118`.

## Cálculo

Para cada año, se suman los valores por edad en los grupos 0 a 14, 15 a 24, 25 a 54, 55 a 64 y 65 años o más. La proporción de cada grupo es su población dividida por la suma de los cinco grupos ese año. Por ejemplo, si un grupo representa 20 personas de un total de 100, su proporción es 20 %. Las cifras futuras son proyecciones basadas en supuestos demográficos, no conteos observados.

## Reproducción

`scripts/data-cleaning/clean_inec_proyecciones_edades.R` guarda `data/processed/inec_proyecciones_edades.rds`. `scripts/plots/plot_34_inec_proyecciones_edades.R` genera el PNG.
