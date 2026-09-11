# Ingreso de hogares con dos adultos y dos hijos y canasta básica, Ecuador, 2025

## Pieza

- `outputs/figures/46_canasta-basica-ingreso-ecuador.png`
- Limpieza: `scripts/data-cleaning/clean_inec_canasta_ingreso.R`
- Visualización: `scripts/plots/plot_inec_canasta_ingreso.R`

## Fuente y procedencia

El gráfico usa la distribución de ingreso de la ENIGHUR 2024-2025 y el valor de
la Canasta Familiar Básica publicado por el INEC para diciembre de 2025. El archivo
compacto de hogares se conserva en
`data/raw/enighur/enighur_2025_hogares_2adultos_2hijos.csv`; los umbrales de SBU
y canasta se conservan en `data/raw/inec_canasta_ingreso/canasta_vs_ingreso_karel.csv`.

El insumo original de canasta fue entregado por Karel Lázaro González Ruíz como
parte de su participación en el concurso Ecuador Quantificado 2026. La entrega
original y su código reproducible están disponibles en:

https://github.com/karelgonzalezruiz/Concurso-Ecuador-Quantificado-2026-Participacion

El extracto de ENIGHUR se construyó desde las bases de trabajo de ENIGHUR 2025
del repositorio `enighur-quantificador`, usando la base de hogares agregados y
la base de personas para reconstruir la composición del hogar.

## Variables y definición

El gráfico muestra la distribución del ingreso corriente monetario mensual
(`ing_mon_cor`) de hogares de cuatro personas con dos adultos de 18 años o más,
dos hijos menores de 18 años, un representante del hogar y un cónyuge o
conviviente. Los hogares se ponderan con `Fexp`.

El porcentaje principal se calcula como la suma de los factores de expansión de
los hogares cuyo ingreso es menor que el costo de la canasta, dividida para la
suma de los factores de expansión de todos los hogares seleccionados. La línea
azul representa el ingreso familiar oficial de 1,6 perceptores, que incorpora
las partes proporcionales de los décimos; la línea naranja marca el costo de la
canasta y es el umbral central del título.

La ENIGHUR 2024-2025 recolecta información entre diciembre de 2024 y noviembre
de 2025. El extracto contiene 2.704 hogares muestrales antes de aplicar `Fexp`.

## Construcción

El script de limpieza valida el extracto de hogares, incorpora los umbrales de
2025 y calcula la proporción de hogares por debajo y por encima de la canasta.
El script de visualización conserva el estilo del histograma de distribución de
ingresos de ENIGHUR y adapta la pieza al lienzo vertical de El Quantificador.
