# Fuente de datos: Actividad física insuficiente por área y grupo etario

## Dataset
**Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), módulo de actividad física de diciembre 2024**

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Archivo utilizado:

- `data/raw/actividad_fisica_joan/2024_12/2_BDD_DATOS_ABIERTOS_ACTIVIDAD_FISICA_2024_12_CSV.csv`

## Variables utilizadas

- `p03`: edad
- `area`: área de residencia
- `fexp`: factor de expansión
- `af101`: días con actividad física en la última semana para población de 8 a 17 años
- `af201cod`, `af201d`, `af201h`, `af201m`: frecuencia y duración de actividad vigorosa
- `af202cod`, `af202d`, `af202h`, `af202m`: frecuencia y duración de actividad moderada
- `af203cod`, `af203d`, `af203h`, `af203m`: frecuencia y duración de caminata

## Notas

- El gráfico publicado se restringe a diciembre de 2024.
- El gráfico final muestra únicamente población de 18 a 69 años.
- La población adulta se divide en tres grupos etarios: 18-29, 30-44 y 45-69 años.
- Se considera actividad física insuficiente cuando el total semanal es menor a 150 minutos moderados equivalentes.
- Minutos moderados equivalentes = caminata + actividad moderada + 2 × actividad vigorosa. Cada componente es días por semana × (horas diarias × 60 + minutos diarios).
- Se incluyen únicamente respuestas 1 o 2 en `af201cod`, `af202cod` y `af203cod`. Los códigos 88 y 99 se excluyen del numerador y del denominador; no se convierten en cero minutos. Las respuestas 2 sí aportan cero minutos al componente correspondiente.
- Se usa `fexp` para calcular las proporciones dentro de cada grupo de edad y área de residencia.

## Corrección del 19 de octubre de 2026

El cálculo anterior clasificaba los códigos 88 y 99 como cero minutos de actividad, lo que elevaba las prevalencias. De 17.730 registros de personas de 18 a 69 años, se excluyen 1.386 con esos códigos y se conservan 16.344 respuestas válidas. Todos los registros con respuesta 1 tienen días, horas y minutos informados.

| Edad | Urbana anterior | Urbana corregida | Rural anterior | Rural corregida |
|---|---:|---:|---:|---:|
| 18-29 años | 22,8% | 11,7% | 19,4% | 5,3% |
| 30-44 años | 18,7% | 9,9% | 15,1% | 7,6% |
| 45-69 años | 23,1% | 15,4% | 18,6% | 10,7% |

Como contraste externo, el cálculo corregido reproduce, redondeadas a una decimal, las cifras del INEC para diciembre de 2024: nacional 11,3%; urbana 12,6%; rural 8,3%; por edad 9,9%, 9,3% y 14,0%. Las seis cifras cruzadas por área y edad son cálculos propios a partir de los microdatos.

Fuentes oficiales:

- [Resultados de diciembre de 2024, INEC](https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Sociales/Actividad_fisica/2024/202412_Actividad_Fisica.pdf)
- [Nota técnica, INEC](https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Sociales/Actividad_fisica/2024/Nota_tecnica_Actividad_Fisica.pdf)
- [Actividad física y sedentarismo: acceso a datos](https://www.ecuadorencifras.gob.ec/actividad-fisica-y-sedentarismo/)
