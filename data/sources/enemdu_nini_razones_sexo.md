# Fuente de datos: razones de inactividad juvenil por sexo

## Gráficos

- `37_a_ninis-razones-estudio-ecuador.png`: razones para no estudiar.
- `37_b_ninis-razones-trabajo-ecuador.png`: razones para no trabajar.

## Fuente oficial

- Institución: Instituto Nacional de Estadística y Censos (INEC)
- Operación: Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)
- Periodo: anual 2025
- Archivo conservado: `data/raw/enemdu/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip`
- Archivo utilizado dentro del ZIP: `BDDenemdu_personas_2025_anual.csv`
- URL: `https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2025/anual/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip`
- SHA-256 del ZIP: `A2DC06B14B84AC2908F59313A70676F9B5C1B697B4780E744452585E60DBCBE8`
- Licencia declarada por el INEC: CC BY 4.0

El ZIP también contiene los diccionarios de personas y vivienda y los archivos de metadatos correspondientes.

## Entrega original

La visualización y el script originales fueron presentados por Valeria Lizeth Marcayata Ojeda al concurso Ecuador Quantificado 2026. El paquete recibido incluía el gráfico final en PNG y un script de R, pero no la base cruda. La adaptación conserva la comparación entre hombres y mujeres y la desagregación por nivel educativo. La salida se separa en dos piezas para que cada pregunta tenga espacio suficiente en el lienzo vertical.

## Universo analítico

Personas de 15 a 29 años que cumplen simultáneamente estas condiciones:

- no tienen empleo, según `empleo`
- no asisten a clases, según `p07`
- cuentan con un factor de expansión válido
- tienen un nivel educativo clasificado entre ninguno, alfabetización, educación básica, bachillerato y superior

Con estas reglas, la población ponderada es de 865.199 jóvenes. El 77,3% corresponde a mujeres.

## Variables utilizadas

- `p02`: sexo
- `p03`: edad
- `p07`: asistencia a clases
- `p09`: razón por la que no asiste a clases
- `p34`: razón por la que no buscó trabajo
- `nnivins`: nivel de instrucción
- `empleo`: población con empleo
- `fexp`: factor de expansión

## Verificación y corrección de códigos

Las categorías se comprobaron contra las etiquetas de la base SPSS oficial de la ENEMDU anual 2025. Esta revisión detectó una inversión en el script entregado:

- `p09 = 16` significa falta de recursos tecnológicos
- `p09 = 17` significa otra razón

La adaptación corrige esa asignación. En la figura, recursos tecnológicos se integra en `Otra razón` porque representa menos de 0,1% en ambos lados y no puede distinguirse con claridad en el lienzo vertical.

## Reproducción

Desde la raíz del repositorio:

```r
Rscript scripts/data-cleaning/clean_enemdu_nini_razones_sexo.R
Rscript scripts/plots/plot_enemdu_nini_razones_sexo.R
```
