# Fuente de datos: Ingreso laboral por sector formal e informal

## Dataset
**Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)**
Corte mensual — marzo 2026

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Portal del INEC: https://www.ecuadorencifras.gob.ec/empleo-encuesta-nacional-de-empleo-desempleo-y-subempleo-enemdu/

Archivo utilizado: `data/raw/enemdu/enemdu_persona_2026_03.sav` (formato SPSS)

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `ingrl` | Ingreso laboral mensual de la ocupación principal |
| `secemp` | Sector de empleo (formal / informal / otros) |
| `p05a`, `p05b` | Variables de afiliación y acceso a seguridad social usadas para imputar `secemp` faltante |
| `p03` | Edad en años cumplidos |
| `empleo` | Condición de ocupación |
| `fexp` | Factor de expansión (pesos muestrales) |

## Diseño muestral
Encuesta con diseño complejo y ponderación muestral. En este gráfico se usan pesos (`fexp`) para calcular percentiles ponderados del ingreso.

## Notas
- Se restringe el universo a personas ocupadas de 15 años o más con ingreso laboral positivo.
- Se excluyen observaciones con `ingrl = 999999` o `ingrl = -1` por corresponder a no respuesta o valores inválidos.
- Las observaciones con `secemp` faltante se imputan con base en la tenencia de seguridad social, siguiendo la lógica del script original.
- Si una observación no tiene clasificación sectorial y tampoco tiene información suficiente en `p05a` y `p05b`, permanece como faltante y se excluye del análisis final.
- Para la visualización, el eje se recorta en el percentil 90 ponderado del ingreso total observado.
