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
- Para la población de 8 a 17 años, el gráfico usa como aproximación de actividad física insuficiente reportar menos de 7 días de actividad en la última semana.
- Para la población de 18 a 69 años, el gráfico usa minutos moderados equivalentes semanales: actividad moderada + caminata + 2 x actividad vigorosa.
- Se considera actividad física insuficiente cuando el total semanal es menor a 150 minutos moderados equivalentes.
