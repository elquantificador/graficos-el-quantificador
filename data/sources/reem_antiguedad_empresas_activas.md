# Fuente de datos: antigüedad de empresas activas

## Gráfico

- `44_antiguedad-empresas-activas-ecuador.png`: distribución de antigüedad y mediana de las empresas activas en Ecuador.

## Fuente oficial

- Institución: Instituto Nacional de Estadística y Censos (INEC).
- Operación: Registro Estadístico de Empresas (REEM).
- Periodo: 2025 provisional.
- URL: `https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Economicas/Registro_Empresas_Establecimientos/2025/Semestre_I/DATOS_ABIERTOS_REEM_2025.zip`.
- Archivo interno utilizado: `EMPRESAS_periodo_2025.csv`.

## Insumo versionado y BDD local

El archivo versionado es únicamente `data/raw/reem/reem_2025_antiguedad_empresas_activas.csv`, un tabulado generado por `clean_reem_antiguedad_empresas_activas.R`: conserva una fila por cada edad de 0 a 59 años y agrupa en una última fila las empresas de 60 años o más.

La BDD oficial `DATOS_ABIERTOS_REEM_2025.zip` se usa solo de manera local y está explícitamente ignorada por Git. No se incorpora al repositorio. El script la descomprime en una carpeta temporal, lee exclusivamente `fecha_inicio_actividad`, escribe el tabulado y elimina los archivos temporales al terminar.

## Universo y cálculo

El REEM 2025 provisional identifica como activas a las empresas que registran actividad económica según los registros administrativos integrados por INEC. La antigüedad se calcula como `2025 - año(fecha_inicio_actividad)`.

Se incluyen 1.204.165 empresas con fecha de inicio válida entre 1900 y 2025, de 1.204.276 registros de empresas activas. La mediana es 11 años y el promedio es 12,89 años.

## Reproducción

Desde la raíz del repositorio, tras descargar localmente el ZIP oficial en la ruta ignorada indicada arriba:

```powershell
Rscript scripts/data-cleaning/clean_reem_antiguedad_empresas_activas.R
Rscript scripts/plots/plot_reem_antiguedad_empresas_activas.R
```
