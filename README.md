# Repositorio de Investigacion

Estructura tipica de un repositorio para investigacion con codigo.

## Estructura
- data/: datos crudos y procesados (mantener archivos grandes fuera de git)
- scripts/: scripts de analisis y utilidades
- figures/: graficos y visualizaciones generadas

## Inicio rapido
1. Coloca los datos en data/.
2. Ejecuta los scripts desde scripts/.
3. Guarda los resultados en figures/.

## Script standalone NINI
- Script: `scripts/evolucion_nini.R`
- Datos de entrada: `data/evolucion/*.csv`
- Salida: `figures/evolucion_nini.png`