# Personas nacidas en Ecuador en Norteamérica

El gráfico 22 muestra tres cifras incorporadas directamente en `scripts/plots/plot_22_norteamerica_mapa_placeholder.R`: 16.320 para Canadá en 2021, 621.915 para Estados Unidos en 2024 y 4.615 para México en 2020. La base cartográfica se obtiene de Natural Earth mediante `rnaturalearth`.

## Fuentes indicadas en el gráfico

- Canadá: Statistics Canada, Censo de Población 2021, [tabla 98-10-0349-01](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810034901). La cifra 16.320 corresponde a inmigrantes nacidos en Ecuador que viven en hogares particulares; la tabla usa datos de la muestra censal del 25 %. La [tabla 98-10-0307-01](https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810030701) reporta 18.995 personas nacidas en Ecuador en hogares particulares: 16.320 inmigrantes, 650 no inmigrantes y 2.025 residentes no permanentes. La cifra del gráfico es, por tanto, más limitada que el total de personas nacidas en Ecuador.
- Estados Unidos: U.S. Census Bureau, American Community Survey 2024, tabla B05006, lugar de nacimiento de la población nacida en el extranjero. El identificador de Ecuador es `B05006_170E`; las definiciones están disponibles en los catálogos de la encuesta [de un año](https://api.census.gov/data/2024/acs/acs1/groups/B05006.html) y [de cinco años](https://api.census.gov/data/2024/acs/acs5/groups/B05006.html).
- México: INEGI, [Censo de Población y Vivienda 2020](https://www.inegi.org.mx/programas/ccpv/2020/), lugar de nacimiento.

## Alcance y reproducción

El script guarda las tres cifras como constantes y usa diferentes años para cada país. No hay en el repositorio un extracto de las tablas ni los filtros completos usados para obtenerlas; en particular, el script no indica si la cifra de Estados Unidos proviene de la estimación ACS de un año o de cinco años. Por ello, la figura puede regenerarse desde el código, pero las tres cifras requieren una verificación independiente con las fuentes y sus universos antes de volver a utilizarlas.
