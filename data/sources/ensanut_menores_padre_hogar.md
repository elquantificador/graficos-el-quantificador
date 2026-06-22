# Menores cuyo padre vive en el hogar

## Fuente

- Archivo crudo: `data/raw/ensanut/1_BDD_ENS2018_f1_personas.dta.zip`
- Archivo procesado: `data/processed/ensanut_menores_padre_hogar.rds`
- Script de limpieza: `scripts/data-cleaning/clean_ensanut_menores_padre_hogar.R`
- Script de gráfico: `scripts/plots/plot_ensanut_menores_padre_hogar.R`

## Construcción

- Población: niñas, niños y adolescentes de 1 a 18 años.
- Variable base: `f1_s2_14`, que indica si el padre de la persona vive en el hogar.
- Agrupación etaria: `1-5`, `6-10`, `11-15` y `16-18`.
- Estimación: proporciones ponderadas con `fexp`.
- Nota de procedencia: la lógica de construcción se adaptó del repositorio `aquijanoruiz/ENSANUT`.
