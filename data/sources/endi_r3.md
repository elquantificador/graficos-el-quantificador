# Fuente de datos: ENDI ronda 3, desnutrición crónica y sobrepeso

Insumo de los gráficos 53_a (`53_a_desnutricion-cronica-edad-ecuador.png`) y 53_b (`53_b_desnutricion-sobrepeso-area-ecuador.png`). Consulta: 7 de noviembre de 2026.

## `endi_r3/endi_r3_boletin_indicadores.csv`

**Proveedor:** INEC, Encuesta Nacional sobre Desnutrición Infantil (ENDI).

**Documentos:**

- Boletín técnico ENDI R3 (octubre 2026), figuras 1 a 4: <https://www.ecuadorencifras.gob.ec/documentos/web-inec/ENDI/R3/Boletin_tecnico_ENDI_R3.pdf>
- Principales resultados ENDI R3, lámina 16 (intervalos de confianza de la R3 en menores de 5 años): <https://www.ecuadorencifras.gob.ec/documentos/web-inec/ENDI/R3/Principales_Resultados_ENDI_R3.pdf>

**Rondas:** R1 julio 2022 a julio 2023; R2 julio 2023 a agosto 2024; R3 agosto 2025 a septiembre 2026. Las fechas de `inicio` y `fin` aproximan esos meses; el gráfico ubica cada ronda en su punto medio.

**Contenido:** prevalencias publicadas, en porcentaje, para Ecuador continental:

- Desnutrición crónica (talla para la edad menor a -2 DE, patrón OMS) en menores de 2 años, de 2 a menores de 5 años y menores de 5 años; nacional, urbano y rural; R1 a R3.
- Sobrepeso y obesidad en menores de 5 años; nacional, urbano y rural; R1 a R3.

`sig_r2_r3` vale 1 cuando el INEC reporta que la diferencia entre R2 y R3 es estadísticamente significativa al 95 % (prueba z entre dos estimaciones independientes, |z| > 1,96, con los errores estándar de los tabulados). Solo se registra en la fila de R3.

**Limitaciones:** las cifras se transcribieron del texto del boletín, no de los tabulados ni de los microdatos, y no traen errores estándar salvo los intervalos de la lámina 16. Para la versión publicable conviene verificarlas contra `Tabulados_ENDI_R3.xlsx` (<https://www.ecuadorencifras.gob.ec/documentos/web-inec/ENDI/R3/Tabulados_ENDI_R3.xlsx>) o reproducirlas con `BDD_ENDI_R3_rds.zip`, como se hizo con la R2 en `clean_endi_desnutricion.R`.
