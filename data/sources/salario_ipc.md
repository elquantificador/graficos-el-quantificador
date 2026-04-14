# Salario promedio e IPC nacional

## Fuente principal

- Instituto Nacional de Estadística y Censos (INEC), Índice de Precios al Consumidor (IPC) nacional mensual.
- Registro Estadístico de Empleo en la Seguridad Social (REESS), serie mensual de sueldo promedio del empleo registrado.

## Archivos usados

- `data/REESS Indicadores Laborales_Empleo_01_2026.xlsx` (`4_2_3`)
- `data/ipc_ind_nac_reg_ciud_03_2026.xlsx` (`1. NACIONAL`)

## Nota metodológica

- El script `clean_salario_ipc.R` transforma ambas series a formato largo mensual.
- Para sueldo promedio, toma la fila `Total sueldo corriente medio` de la hoja `4_2_3`.
- Para el gráfico de sueldo real, el sueldo nominal se deflacta usando el IPC general mensual.
- Para el gráfico de costo de vida, se calcula la variación interanual de:
- IPC general
- alimentos y bebidas no alcohólicas
- alquileres efectivos del alojamiento
- transporte
- combustibles y lubricantes para equipo de transporte personal
- sueldo promedio nominal
