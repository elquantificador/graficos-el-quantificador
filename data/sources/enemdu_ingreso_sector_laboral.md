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
| `p03` | Edad en años cumplidos |
| `empleo` | Condición de ocupación |
| `fexp` | Factor de expansión (pesos muestrales) |

## Diseño muestral
Encuesta con diseño complejo y ponderación muestral. En este gráfico se usan pesos (`fexp`) para calcular percentiles ponderados del ingreso.

## Notas
- Se restringe el universo a personas ocupadas de 15 años o más con ingreso laboral positivo.
- Se excluyen observaciones con `ingrl = 999999` o `ingrl = -1` por corresponder a no respuesta o valores inválidos.
- Se incluyen los códigos observados 1 y 2 de `secemp`, correspondientes a los sectores formal e informal. Las observaciones sin esa clasificación se excluyen; no se imputa el sector.
- Las medianas y los percentiles 10, 25, 75 y 90 se calculan por sector sobre la muestra completa seleccionada. Las cajas muestran los percentiles 25 y 75, los bigotes los percentiles 10 y 90, y el eje visible se extiende hasta USD 1.350.
