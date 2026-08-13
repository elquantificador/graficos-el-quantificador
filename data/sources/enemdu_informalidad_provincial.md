# Fuente de datos: informalidad y empleo no remunerado por provincia

## Dataset

**Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU), anual 2025**

El insumo local contiene las tasas provinciales de informalidad laboral y empleo no remunerado para 2024 y 2025. Juan Diego Sotomayor Jiménez transcribió y verificó las cifras contra las tablas 8 y 9 del boletín oficial.

## Proveedor

Instituto Nacional de Estadística y Censos (INEC), Ecuador.

## Acceso

- Boletín Técnico Nro. 03-2026-ENEMDU: https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2025/anual/Boletin_tecnico_anual_enero-diciembre_2025.pdf
- Portal de estadísticas laborales: https://www.ecuadorencifras.gob.ec/estadisticas-laborales-enemdu/
- Proyecto reproducible original: https://github.com/jdsotomayorjimenez/elquantificador_informalidad_ecu
- Fecha de descarga documentada por el autor: 23 de junio de 2026.

## Archivos utilizados

- `data/raw/enemdu/enemdu_anual_2025_provincial.csv`
- `data/processed/enemdu_informalidad_provincial.rds`, generado por el script de limpieza.

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `provincia` | Provincia del Ecuador. |
| `region_natural` | Región natural añadida por el autor; no proviene del boletín. |
| `informalidad_2024`, `informalidad_2025` | Personas con empleo en el sector informal como porcentaje del empleo total. |
| `no_remunerado_2024`, `no_remunerado_2025` | Empleo no remunerado como porcentaje de la población económicamente activa. |

## Validaciones

El script de limpieza comprueba:

- presencia de las seis columnas esperadas;
- 24 provincias sin duplicados;
- tasas numéricas entre 0 y 100;
- valores de control para Morona Santiago, Galápagos, Guayas y Azuay;
- correlaciones provinciales cercanas a 0,741 en 2025 y 0,717 en 2024.

## Notas metodológicas

- La tasa nacional de informalidad de 51,5% es un promedio ponderado publicado por el INEC. No equivale al promedio simple de las 24 provincias.
- La asociación presentada es una correlación entre agregados provinciales. No permite establecer causalidad ni inferir relaciones individuales.
- El empleo en el sector informal sigue la definición administrativa del INEC para personas ocupadas en unidades productivas de menos de 100 trabajadores sin RUC.
- El proyecto original fue el primer lugar de Ecuador Quantificado 2026.
