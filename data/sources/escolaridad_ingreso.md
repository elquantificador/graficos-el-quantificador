# Fuente de datos: Ingresos por nivel de escolaridad

## Dataset
**Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)**
Corte mensual — enero 2026

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Portal del INEC: https://www.ecuadorencifras.gob.ec/empleo-encuesta-nacional-de-empleo-desempleo-y-subempleo-enemdu/

Archivo utilizado: `data/raw/enemdu/enemdu_persona_2026_01.sav` (formato SPSS)

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `ingrl` | Ingreso laboral mensual (USD) |
| `p10a` | Nivel de instrucción más alto aprobado |
| `p03` | Edad en años cumplidos |
| `upm` | Unidad primaria de muestreo |
| `estrato` | Estrato de muestreo |
| `fexp` | Factor de expansión (pesos muestrales) |

## Diseño muestral
Encuesta con diseño complejo: estratificado, por conglomerados, con pesos muestrales. Implementado con el paquete `survey` de R.

## Notas
- Se filtran personas de 15 años o más.
- Se excluyen observaciones con `ingrl = 999999` o `ingrl = -1` (códigos de no respuesta).
- Los percentiles reportados son ponderados: p10, p25, p50, p75, p90.
