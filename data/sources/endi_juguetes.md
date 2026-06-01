# Fuente de datos: ¿Con qué juegan los niños y niñas en Ecuador? (ENDI)

## Dataset
**Encuesta Nacional de Desnutrición Infantil — Segunda Ronda (ENDI R2)**
Corte 2023-2024

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Portal del INEC: https://www.ecuadorencifras.gob.ec/encuesta-nacional-sobre-desnutricion-infantil/

Archivo utilizado: `data/raw/endi_r2/BDD_ENDI_R2_f3_desarrollo_inf.rds`

Diccionario de apoyo: `data/sources/Diccionario_ENDI_di.xlsx`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `f3_s1_100_a` | Juega con muñecas de trapo, lana, carritos u otros juguetes |
| `f3_s1_100_b` | Juega con ollas, palos, piedras, conchas u hojas |
| `f3_s1_100_c` | Juega con juguetes comprados en un almacén o mercado |
| `f3_s1_100_d` | Juega con juguetes para armar o construir, como legos o rompecabezas |
| `f3_s1_100_e` | Juega con elementos para aprender texturas, formas o colores |
| `f3_s1_100_f` | Juega con muñecos y objetos de roles o fantasía |
| `f3_s1_100_g` | Juega con artículos electrónicos como tablets, celulares o consolas |
| `id_upm` | Unidad primaria de muestreo |
| `estrato` | Estrato de muestreo |
| `fexp_di` | Factor de expansión del módulo de desarrollo infantil |

## Diseño muestral
Encuesta con diseño complejo: estratificado, por conglomerados y con pesos muestrales. Implementado con `survey::svydesign()` usando `id_upm` como UPM, `estrato` como estrato y `fexp_di` como factor de expansión.

## Indicador graficado
Porcentaje ponderado de niños menores de 5 años que juegan con cada tipo de juguete. Las categorías no son excluyentes: un mismo niñ@ puede aparecer en varias de ellas.

## Universo
Niños y niñas incluidos en el módulo `f3_desarrollo_inf` con respuesta válida en cada ítem de la batería `f3_s1_100`.

## Notas

- Script de limpieza: `scripts/data-cleaning/clean_endi_juguetes.R`
- Script de gráfico: `scripts/plots/plot_endi_juguetes.R`
- El módulo `f3` contiene 9,836 registros en esta copia del dataset.
- La visualización publicada omite la barra de `f3_s1_100_c` y destaca ese resultado en el texto de apoyo: `97,2%` juega con juguetes comprados en un almacén o mercado.
- La pregunta se interpreta como batería de selección múltiple, por lo que las proporciones no suman 100%.
