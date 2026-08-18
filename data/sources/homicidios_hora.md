# Hora de los asesinatos

## Fuente

- Repositorio: [LIDE-Grafico_Horas](https://github.com/angel-cloud976/LIDE-Grafico_Horas)
- Versión usada: commit `cb2b375`
- Archivo: `Homicidios/mdi_homicidiosintencionales_pm_2014_2026.xlsx`
- Copia local: `data/raw/homicidios/mdi_homicidiosintencionales_pm_2014_2026.xlsx`

## Pregunta y cobertura

La pieza responde a qué hora del día se registra la mayor cantidad de asesinatos en Ecuador. El gráfico usa los años completos 2017–2025. El archivo de origen llega hasta mayo de 2026, pero ese año parcial no se incluye en la distribución.

## Tratamiento

- Se conservan únicamente los registros con `tipo_muerte == "ASESINATO"`.
- `hora_infraccion` se convierte a la hora entera de 0 a 23; los valores vacíos o `SIN_DATO` se excluyen.
- Se cuentan los registros por hora y se completan las 24 horas, incluidas aquellas sin observaciones.
- La distribución final contiene 32.853 asesinatos con hora válida. El pico se ubica a las 21:00, con 2.439 registros (7,4%).

## Reproducción

```powershell
Rscript scripts/data-cleaning/clean_homicidios_hora.R
Rscript scripts/plots/plot_homicidios_hora.R
```
