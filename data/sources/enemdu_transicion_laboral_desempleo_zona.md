# Fuente de datos: Transición laboral desde el desempleo por zona

## Dataset
**Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)**
Tabulados de Matriz de Transición Laboral, trimestre IV 2022 a trimestre IV 2023

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Archivo utilizado: `data/raw/enemdu/Trimestre_IV_2022_2023_tabulados_matriz.xlsx`

Hojas utilizadas:

- `1.1. MTL - Nacional`
- `1.2. MTL - Urbano`
- `1.3. MTL - Rural`

## Variables utilizadas

Se usa la fila `Desempleado` de cada hoja para extraer:

- transición a `Empleado` en 2023
- transición a `Desempleado` en 2023
- transición a `Población Económicamente inactiva` en 2023
- total de personas desempleadas en 2022

## Notas

- El gráfico se restringe a personas que estaban desempleadas en el trimestre IV de 2022.
- La desagregación por zona de residencia se toma directamente de las hojas `Urbano` y `Rural`.
- La población que sale de la fuerza laboral corresponde a personas que no trabajan y no están disponibles para trabajar por cualquier motivo, siguiendo la definición del glosario del archivo fuente.
