# Fuente de datos: Desnutrición crónica por etnia (ENDI)

## Dataset
**Encuesta Nacional de Desnutrición Infantil — Segunda Ronda (ENDI R2)**
Corte 2023–2024

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Portal del INEC: https://www.ecuadorencifras.gob.ec/encuesta-nacional-sobre-desnutricion-infantil/

Archivo utilizado: `data/raw/endi_r2/BDD_ENDI_R2_f1_personas.rds` (formato RDS, preprocesado desde la base oficial)

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `dcronica_2` | Desnutrición crónica (1 = sí, 0 = no) |
| `etnia` | Autoidentificación étnica |
| `upm` | Unidad primaria de muestreo |
| `estrato` | Estrato de muestreo |
| `fexp` | Factor de expansión (pesos muestrales) |

## Diseño muestral
Encuesta con diseño complejo: estratificado por conglomerados, con pesos muestrales. Implementado con el paquete `srvyr` de R.

## Indicador graficado
Prevalencia de desnutrición crónica (retardo en talla) por grupo étnico, con intervalos de confianza al 95%.

## Notas
- Script de limpieza: `scripts/data-cleaning/clean_endi_desnutricion.R`
- Script de gráfico: `scripts/plots/plot_endi_desnutricion.R`
