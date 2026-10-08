# Ecuador no cuenta con cifras actualizadas del mercado laboral desde mayo de 2026

## Pieza

- `outputs/figures/50_mercado-laboral-pobreza-ecuador.png`
- Limpieza: `scripts/data-cleaning/clean_inec_mercado_laboral_pobreza_2026.R`
- Visualización: `scripts/plots/plot_50_inec_mercado_laboral_pobreza_2026.R`

## Fuente y procedencia

La pieza usa los valores del primer gráfico del artículo **Apagón estadístico:
¿qué está pasando en el INEC en 2026?**. Los indicadores de mercado laboral
corresponden a mayo de 2026. Los indicadores de pobreza por ingresos
corresponden a diciembre de 2025.

La fuente oficial de la ENEMDU es el [INEC](https://www.ecuadorencifras.gob.ec/enemdu-2026/).
El archivo CSV incluido en `data/raw/enemdu/` es un extracto pequeño y explícito
de los ocho valores usados en la pieza. No contiene microdatos ni reestima los
indicadores.

## Indicadores

Mercado laboral: desempleo (3,1 %), subempleo (18,3 %), empleo adecuado
(36,6 %) y ocupación en el sector informal (52,8 %).

Pobreza por ingresos: pobreza extrema (8,3 %), pobreza por ingresos (21,4 %),
pobreza urbana (13,8 %) y pobreza rural (37,6 %).

La fecha prevista de publicación de los datos de junio a octubre de 2026 es el
22 de diciembre de 2026, según el calendario del INEC. La visualización tiene
fecha prevista de publicación el 9 de octubre de 2026 y el catálogo la mantiene
como `draft` hasta que exista una decisión editorial y un enlace de publicación.
