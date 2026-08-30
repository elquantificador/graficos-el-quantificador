# Revisión R: empleo juvenil ENEMDU 2025

Fecha: 2026-08-29
Scripts revisados:

- `scripts/data-cleaning/clean_enemdu_juventud_empleo_2025.R`
- `scripts/plots/plot_enemdu_juventud_empleo_2025.R`

## Resultado

La revisión no encontró problemas críticos, altos, medios ni bajos que impidan usar los gráficos 1 y 2.

## Comprobaciones

- La limpieza lee el archivo anual oficial dentro del ZIP esperado y verifica que estén presentes las variables requeridas.
- El universo está definido como jóvenes de 18 a 29 años en la PEA, con filtro explícito de sexo, provincia y factor de expansión válido.
- `condact = 1` se usa para empleo adecuado y `desempleo = 1` para desempleo, de acuerdo con la documentación revisada.
- Se validan los rangos de códigos y se detiene la ejecución ante códigos no documentados o agregaciones incompletas.
- Los controles reproducen los valores entregados: hombres 36,7% de empleo adecuado y 6,3% de desempleo; mujeres 26,7% y 11,5%; además de los valores provinciales de referencia.
- Los dos scripts usan rutas relativas desde la raíz, guardan un RDS intermedio reproducible y separan limpieza de visualización.
- El script de gráficos usa `theme_quantificador()`, los helpers de wrapping y el dispositivo `ragg` a 4 x 5 pulgadas y 300 dpi.
- El gráfico provincial corrige la codificación de caracteres antes de ordenar y rotular las provincias.
- La posición del logo se ajusta únicamente para evitar la colisión con el pie de fuente en estos dos gráficos densos.
- El tercer panel de la entrega original no se reproduce, tal como se solicitó, porque duplica un gráfico existente.

## Verificación externa

EcuDataMCP se consultó para localizar la ENEMDU y su documentación. El catálogo disponible expuso ediciones 2022 y 2023, pero no la edición anual 2025; el acceso CKAN tampoco estuvo disponible desde este entorno. La limitación queda registrada en `data/sources/enemdu_juventud_empleo_2025.md` y en la metadata del RDS. La reproducción usa el ZIP oficial 2025 ya conservado en el repositorio.

## Verificación de artefactos

- `42_a_empleo-adecuado-juvenil_sexo-ecuador.png`: 1200 x 1500 px, RGB, 300 dpi.
- `42_b_empleo-adecuado-juvenil_provincia-ecuador.png`: 1200 x 1500 px, RGB, 300 dpi.
- `python scripts/validate_chart_catalog.py`: `Catalog OK: 51 rows validated.`
- `git diff --check`: sin errores de whitespace.
