# Fuente de datos: ENDI, rondas 1 a 3

Los gráficos 53_a y 53_b utilizan los tabulados oficiales de INEC, descargados el 7 de noviembre de 2026 y guardados en `data/raw/endi_r3/`:

- [Ronda 1, 2022-2023](https://www.ecuadorencifras.gob.ec/documentos/web-inec/ENDI/Tabulados_ENDI_R1.xlsx).
- [Ronda 2, 2023-2024](https://www.ecuadorencifras.gob.ec/documentos/web-inec/ENDI/R2/Tabulados_ENDI_R2.xlsx).
- [Ronda 3, 2025-2026](https://www.ecuadorencifras.gob.ec/documentos/web-inec/ENDI/R3/Tabulados_ENDI_R3.xlsx).

El script de limpieza extrae las filas 9 a 11 (nacional, urbano y rural) de las hojas `T2_i1` (desnutrición crónica en menores de 2 años), `T2_i7` (de 2 a menores de 5 años), `T2_i4` (menores de 5 años) y `T2_i12` (sobrepeso y obesidad en menores de 5 años). Usa los valores y errores estándar completos de las columnas D y E; la cobertura nacional excluye Galápagos.

Las diferencias R3 menos R2 se calculan antes de redondear. Para la significancia se usa la prueba de dos estimaciones independientes indicada por el [boletín técnico R3](https://www.ecuadorencifras.gob.ec/documentos/web-inec/ENDI/R3/Boletin_tecnico_ENDI_R3.pdf): `z = (R3 - R2) / sqrt(EE_R2² + EE_R3²)`, con significancia al 95 % cuando `|z| > 1,96`. Los resultados de significancia coinciden con el boletín. Algunas diferencias redondeadas difieren del boletín porque este resta las prevalencias ya redondeadas.

Las fechas de inicio y fin aproximan los meses indicados por INEC; el gráfico de series sitúa cada ronda en su punto medio. La ronda 1 corresponde a julio 2022 a julio 2023; la ronda 2, a julio 2023 a agosto 2024; y la ronda 3, a agosto 2025 a septiembre 2026.
