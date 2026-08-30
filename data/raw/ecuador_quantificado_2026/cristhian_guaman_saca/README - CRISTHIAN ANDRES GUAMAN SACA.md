
# Juventud en pausa: la brecha del empleo adecuado en Ecuador, 2025

## Descripción del proyecto

Este proyecto analiza el acceso al empleo adecuado de los jóvenes en Ecuador durante 2025, con énfasis en las diferencias por sexo y provincia.

La pregunta central del análisis es:

¿Qué tan desigual es el acceso al empleo adecuado para los jóvenes ecuatorianos según género y territorio?

El análisis se basa en microdatos de la Encuesta Nacional de Empleo, Desempleo y Subempleo ENEMDU Anual 2025 del Instituto Nacional de Estadística y Censos INEC.

## Fuente de datos

Fuente principal: INEC, ENEMDU Anual 2025.

Base utilizada:
BDDenemdu_personas_2025_anual.csv

Diccionario utilizado:
Diccionario de Datos_persona_anual_2025.xlsx

Documentos metodológicos consultados:
- Metodología ENEMDU.
- Metodología para la clasificación de la población con empleo por condición de actividad.
- Metodología para la clasificación de la población con empleo por sector informal.
- Diseño muestral ENEMDU.

## Población analizada

Se consideró como población joven a las personas de 18 a 29 años.

## Variables utilizadas

p02: Sexo.
p03: Edad.
prov: Provincia.
condact: Condición de actividad.
secemp: Sector de empleo.
fexp: Factor de expansión.

## Indicadores calculados

Los indicadores fueron calculados usando el factor de expansión fexp.

Empleo adecuado juvenil:
jóvenes con condición de actividad igual a empleo adecuado, calculado sobre la PEA joven.

Desempleo juvenil:
jóvenes en desempleo abierto u oculto, calculado sobre la PEA joven.

Informalidad juvenil:
jóvenes ocupados clasificados dentro del sector informal, calculado sobre jóvenes ocupados.

## Principales hallazgos

En 2025, los hombres jóvenes registraron un 36,7% de empleo adecuado, mientras que las mujeres jóvenes alcanzaron un 26,7%. Esto representa una brecha aproximada de 10 puntos porcentuales.

Además, el desempleo de las mujeres jóvenes fue de 11,5%, frente a 6,3% en los hombres jóvenes. Es decir, las mujeres jóvenes tuvieron 5,2 puntos porcentuales más de desempleo que los hombres jóvenes.

A nivel provincial, el acceso al empleo adecuado juvenil muestra diferencias marcadas. Pichincha, Galápagos y Azuay presentan los mayores porcentajes de empleo adecuado juvenil, mientras que Morona Santiago, Napo y Chimborazo se ubican entre los menores.

## Archivos del proyecto

Carpeta principal:
Ecuador_Quantificado_2026

Subcarpetas:
- datos_originales: contiene las bases originales descargadas del INEC.
- metodologia: contiene los documentos metodológicos en PDF.
- codigo: contiene el notebook de Google Colab.
- datos_limpios: contiene los resultados procesados.
- graficos: contiene la visualización final en PNG y PDF.

Archivos principales:
- empleo_juvenil_ecuador.ipynb
- resumen_empleo_juvenil_2025.csv
- visualizacion_final_juventud_empleo_ecuador_2025_v3.png
- visualizacion_final_juventud_empleo_ecuador_2025_v3.pdf

## Reproducibilidad

Para reproducir el análisis:

1. Descargar los microdatos de la ENEMDU Anual 2025 desde el portal del INEC.
2. Colocar los archivos en la carpeta datos_originales.
3. Abrir el notebook empleo_juvenil_ecuador.ipynb en Google Colab.
4. Ejecutar las celdas en orden.
5. El notebook genera los indicadores, el archivo limpio y la visualización final en PNG y PDF.

## Herramientas utilizadas

- Python.
- Google Colab.
- pandas.
- numpy.
- matplotlib.

## Autoría

Elaboración propia a partir de datos oficiales del INEC.
