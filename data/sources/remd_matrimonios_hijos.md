# REMD: divorcios registrados según hijos reconocidos

## Fuente

Registro Estadístico de Matrimonios y Divorcios (REMD), Instituto Nacional de Estadística y Censos (INEC), años 2020 a 2025.

## Insumo utilizado

`data/raw/remd_matrimonios/cohort_2020_survival_input.csv` es una versión reducida, sin identificadores, de la cohorte validada en el repositorio `matrimonios-divorcios-salas`, commit `cde4c13`. Conserva únicamente la elegibilidad del enlace, el grupo de hijos reconocidos, el indicador de divorcio registrado y el tiempo de seguimiento.

## Transformación

`clean_remd_matrimonios_hijos.R` conserva los matrimonios con enlace primario elegible, una categoría válida de hijos reconocidos y hasta cinco años de seguimiento. El gráfico estima `1 - S(t)` con Kaplan-Meier para mostrar el porcentaje acumulado de matrimonios con un divorcio registrado.

Los grupos contienen 22.246 matrimonios con 0 hijos, 5.879 con 1 hijo, 3.349 con 2 hijos, 1.365 con 3 hijos y 738 con 4 o más hijos. En el gráfico, esta última categoría se abrevia como `4+ hijos`. Se excluyen 1.413 matrimonios sin categoría válida de hijos reconocidos.
