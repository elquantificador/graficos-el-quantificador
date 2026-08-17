# Fuente de datos: Personal público de salud por provincia (RAS)

## Dataset

**Registro de Actividades y Recursos de Salud (RAS), serie del Ministerio de Salud Pública**

## Archivos usados

El gráfico provincial usa la serie `msp_serie_prov.rds` en el último año disponible. Antes de graficar, el script valida que las sumas de las cuatro categorías de personal coincidan con la serie nacional y con las series agregadas por cantón, parroquia y área:

- `data/raw/ras/msp_serie_nac.rds`
- `data/raw/ras/msp_serie_prov.rds`
- `data/raw/ras/msp_serie_cant.rds`
- `data/raw/ras/msp_serie_parr.rds`
- `data/raw/ras/msp_serie_area.rds`

## Variables graficadas

| Variable | Descripción |
|---|---|
| `tmedicos` | Médicos |
| `tenf` | Enfermeros |
| `tobst` | Obstetrices |
| `ttaps` | Técnicos de Atención Primaria en Salud (TAPS) |

## Notas metodológicas

- El año graficado es el último año disponible en la serie nacional, 2021.
- Las provincias se ordenan por el total de las cuatro categorías de personal.
- El gráfico muestra la composición provincial mediante barras horizontales apiladas.
- Las cifras de referencia se muestran redondeadas al entero más cercano.

## Scripts y figura

- Visualización: `scripts/plots/plot_ras_personal_salud_provincial.R`
- Figura: `outputs/figures/38_personal-salud_provincia-ecuador.png`
