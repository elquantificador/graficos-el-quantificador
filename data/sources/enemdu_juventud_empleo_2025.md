# Fuente de datos: empleo adecuado juvenil por sexo y provincia

## Gráficos

- `42_a_empleo-adecuado-juvenil_sexo-ecuador.png`: empleo adecuado y desempleo juvenil por sexo.
- `42_b_empleo-adecuado-juvenil_provincia-ecuador.png`: empleo adecuado juvenil por provincia.

El tercer panel de la entrega original, sobre informalidad, queda fuera porque duplica una pieza ya producida en el repositorio.

## Fuente oficial

- Institución: Instituto Nacional de Estadística y Censos (INEC).
- Operación: Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU).
- Periodo: anual 2025.
- Archivo: `data/raw/enemdu/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip`.
- Archivo utilizado dentro del ZIP: `BDDenemdu_personas_2025_anual.csv`.
- URL: `https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2025/anual/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip`.

## Procedencia y uso de EcuDataMCP

Se consultó EcuDataMCP para verificar la disponibilidad de la encuesta y su documentación. El catálogo ANDA disponible en EcuDataMCP devuelve ENEMDU 2023 y 2022, pero todavía no expone la edición anual 2025. El portal CKAN tampoco respondió desde este entorno. Por eso, la reproducción usa el ZIP oficial 2025 ya conservado en `data/raw/enemdu/` y deja esta limitación registrada.

## Universo y variables

Se analizan personas de 18 a 29 años que forman parte de la población económicamente activa: personas ocupadas o desempleadas. Los porcentajes usan el factor de expansión `fexp`.

- `p02`: sexo.
- `p03`: edad.
- `prov`: provincia.
- `condact`: condición de actividad; `1` identifica empleo adecuado.
- `empleo`: indicador de población con empleo.
- `desempleo`: indicador de población desempleada.
- `fexp`: factor de expansión.

El empleo adecuado y el desempleo se calculan sobre la PEA joven del sexo o provincia correspondiente. Los controles del script reproducen los valores visibles en la entrega: 36,7% de empleo adecuado y 6,3% de desempleo entre hombres; 26,7% y 11,5% entre mujeres; y los valores provinciales publicados en el panel original.

## Reproducción

Desde la raíz del repositorio:

```powershell
Rscript scripts/data-cleaning/clean_enemdu_juventud_empleo_2025.R
Rscript scripts/plots/plot_enemdu_juventud_empleo_2025.R
```
