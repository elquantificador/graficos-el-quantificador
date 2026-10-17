# Aviso sobre datos

Las carpetas `data/raw/` y `data/sources/` reúnen datos publicados por terceros
(INEC, ARCONEL, BCE, Fiscalía, LAPOP, WVS, ACLED, FBref y otras fuentes
documentadas en `data/sources/`). LIDE no es titular de esos datos y no los
licencia. Cada fuente mantiene sus propios términos de uso, citación y
redistribución, que pueden ser más estrictos que los de este repositorio.

Antes de reutilizar un archivo de `data/raw/`:

1. Lee su ficha en `data/sources/`.
2. Consulta los términos de la institución que lo publica.
3. Cita la fuente original, no este repositorio.

Excepción: los cortes de ofertas laborales de `data/raw/jobs_scrape/` (tomados de
Multitrabajos, Computrabajo y Mipleo) no se publican aquí, porque contienen
texto, empresas y enlaces de terceros. Los scripts `clean_jobs_*` los necesitan
para replicarse; escribe a elquantificador@laboratoriolide.org para evaluar el
acceso.

Lo que LIDE sí produce, como `data/processed/` derivado de datos abiertos y
`outputs/tables/`, se distribuye bajo CC BY-NC 4.0 según `LICENSE-images`, sin
perjuicio de los términos de la fuente original.

Si eres titular de alguno de estos datos y quieres que lo retiremos, escríbenos
a elquantificador@laboratoriolide.org.
