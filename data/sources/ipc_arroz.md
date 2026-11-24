# IPC del arroz, octubre de 2026

Insumos del carrusel 57: `57_a` (piladoras, SIPA), `57_b` (incidencias de octubre) y `57_c` (índice del arroz).

## `ipc_arroz/ipc_incidencias_productos_octubre_2026.csv`

- Fuente: INEC, Boletín Técnico N° 10-2026-IPC, tablas 2, 3 y 4 (productos con mayor incidencia en la variación mensual de las tres divisiones que más aportaron en octubre de 2026). <https://www.ecuadorencifras.gob.ec/documentos/web-inec/Inflacion/2026/octubre/BOLETIN_TECNICO_IPC_2026_10.pdf>
- Valores transcritos del boletín: ponderación, incidencia mensual (puntos porcentuales) y variación mensual (%). No son reestimaciones.
- Base del IPC: julio 2025 a junio 2026 = 100. Inflación mensual nacional de octubre de 2026: 0,33%.
- El boletín publica solo los productos de mayor incidencia dentro de las tres divisiones que más aportaron (alimentos y bebidas no alcohólicas, transporte, servicios de restaurantes y alojamiento). Las diez divisiones restantes sumaron una incidencia de -0,0255 puntos. El gráfico 57_b muestra el top 10 de esa lista, no de toda la canasta.

## `ipc_arroz/ipc_arroz_general_nacional_2015_2026_06.csv`

- Fuente: INEC, tabulados del IPC de junio de 2026, archivo `1. NACIONAL.csv` dentro de `ipc_ind_nac_reg_ciud_06_2026` (ver `data/raw/ipc_inec_2026_06/`, no versionado). Se extrajeron solo la subclase `01111 Arroz` y el índice general.
- Base 2014 = 100, enero de 2015 a junio de 2026.
- En julio de 2026 el INEC cambió el año base del IPC (julio 2025 a junio 2026 = 100). Los índices de julio a septiembre de 2026 en la nueva base no se incluyen; la variación de octubre (14,95%) no se incluye en el gráfico 57_c.

## Lectura

- La incidencia es el aporte de cada producto a la variación mensual del IPC; la suma de las incidencias de todos los productos es igual a la variación mensual.
- Las explicaciones del alza (reapertura de exportaciones a Colombia, lluvias asociadas a El Niño) vienen de prensa y gremios; estos datos no las identifican causalmente.

## `sipa_piladoras/sipa_precios_piladoras_2012_2026_10.csv`

- Fuente: Ministerio de Desarrollo Económico y Productivo, Sistema de Información Pública Agropecuaria (SIPA), archivo `precios-agroindustria.xlsx`, hoja `Histórico Piladoras 12-25` («Precios mensuales piladoras», información 2012 a 2026, actualización de noviembre de 2026). <https://sipa.agricultura.gob.ec/descargas/base-estadistica/modulo_economico/precios-agroindustria.xlsx>
- Descargado el 23 de noviembre de 2026. Se exportó la hoja completa a CSV sin cambiar valores; solo se recortaron espacios al final de nombres de cantón, producto y mes, y se convirtió el mes a número.
- Columnas: año, mes, provincia, cantón, producto, presentación, tipo (compra o venta de la piladora) y precio promedio en dólares por unidad de presentación.
- El gráfico 57_a usa la compra de arroz en cáscara grano largo en saca de 200 libras (solo Guayas) y la venta de arroz pilado natural grano largo en quintal de 100 libras, promediadas sin ponderar entre los cantones con reporte en cada mes. El número de cantones cambia entre meses.

## Franjas de eventos (57_a y 57_c)

- El Niño: NOAA, avisos de El Niño de junio de 2023 a mayo de 2024 y desde el 11 de junio de 2026; El Niño 2015-16, marzo de 2015 a mayo de 2016.
- Cierre del mercado colombiano al arroz ecuatoriano: 24 de febrero de 2026 hasta la reanudación de exportaciones el 25 de julio de 2026 (Primicias, octubre de 2026).
- Inundaciones en la Costa a inicios de 2023: fechas aproximadas (enero a mayo de 2023), según prensa y el boletín situacional de arroz 2023 del MAG.
