# Fuente de datos: Padres e hijos (censo)

## Dataset

Censo de Población y Vivienda 2010 y 2022 (tabulados REDATAM)

## Proveedor

Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso

Portal REDATAM del INEC: <https://redatam.ec/>

Archivos utilizados en este repositorio:

- `data/censo_padres_hijos_2010.xlsx` (tabulación censo 2010)
- `data/censo_padres_hijos_2022.xlsx` (tabulación censo 2022)

## Estructura usada de los tabulados

- Hoja: `Output`
- Grupos de edad: fila 12 (columnas desde C)
- Datos: desde fila 13
- Variable de relación: columna B (`relationship`)

## Grupos de edad usados en el gráfico

- 10 - 19 años
- 20 - 29 años
- 30 - 39 años
- 40 - 49 años

## Definición de la proporción graficada

Se calcula, por año y grupo de edad:

- **Numerador**: personas reportadas como hijo/a (o hijastro/a en 2022) y nieto/a del representante o jefe del hogar.
- **Denominador**: total de personas del grupo de edad en la tabulación.
- **Indicador**: `share = numerator / denominator`.

Categorías del numerador por año:

- 2010: `Hijo o hija`, `Nieto o nieta`
- 2022: `Hija o hijo`, `Hijastra o hijastro`, `Nieta o nieto`

## Notas

- Se excluyen filas vacías, `Total` y valores no numéricos (`-`).
- La diferencia de etiquetas entre 2010 y 2022 se armoniza en el script de limpieza.
- Script asociado: `scripts/data-cleaning/clean_padres_hijos_censo.R`.
