# IPC del arroz, octubre de 2026

Insumos del carrusel 57 (`57_a` y `57_b`).

## `ipc_arroz/ipc_incidencias_productos_octubre_2026.csv`

- Fuente: INEC, Boletín Técnico N° 10-2026-IPC, tablas 2, 3 y 4 (productos con mayor incidencia en la variación mensual de las tres divisiones que más aportaron en octubre de 2026). <https://www.ecuadorencifras.gob.ec/documentos/web-inec/Inflacion/2026/octubre/BOLETIN_TECNICO_IPC_2026_10.pdf>
- Valores transcritos del boletín: ponderación, incidencia mensual (puntos porcentuales) y variación mensual (%). No son reestimaciones.
- Base del IPC: julio 2025 a junio 2026 = 100. Inflación mensual nacional de octubre de 2026: 0,33%.
- El boletín publica solo los productos de mayor incidencia dentro de las tres divisiones que más aportaron (alimentos y bebidas no alcohólicas, transporte, servicios de restaurantes y alojamiento). Las diez divisiones restantes sumaron una incidencia de -0,0255 puntos. El gráfico 57_a muestra el top 10 de esa lista, no de toda la canasta.

## `ipc_arroz/ipc_arroz_general_nacional_2015_2026_06.csv`

- Fuente: INEC, tabulados del IPC de junio de 2026, archivo `1. NACIONAL.csv` dentro de `ipc_ind_nac_reg_ciud_06_2026` (ver `data/raw/ipc_inec_2026_06/`, no versionado). Se extrajeron solo la subclase `01111 Arroz` y el índice general.
- Base 2014 = 100, enero de 2015 a junio de 2026.
- En julio de 2026 el INEC cambió el año base del IPC (julio 2025 a junio 2026 = 100). Los índices de julio a septiembre de 2026 en la nueva base no se incluyen; la variación de octubre (14,95%) se cita en la nota del gráfico 57_b.

## Lectura

- La incidencia es el aporte de cada producto a la variación mensual del IPC; la suma de las incidencias de todos los productos es igual a la variación mensual.
- Las explicaciones del alza (reapertura de exportaciones a Colombia, lluvias asociadas a El Niño) vienen de prensa y gremios; estos datos no las identifican causalmente.
