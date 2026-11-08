# Homicidios de menores de edad

## Fuente

- Ministerio del Interior, registros de muertes violentas intencionales (`mdi_homicidiosintencionales`), 2014 a septiembre de 2026.
- Entregado por Odalis Clemente (Laboratorio LIDE) en Google Drive, carpeta `Daniel/homicidios_intencionales`, archivo `mdi_homicidiosintencionalse_2014_2026.xlsx`.
- Copia local: `data/raw/homicidios/mdi_homicidiosintencionales_2014_sep2026.xlsx` (6,2 MB, 46.107 registros, fecha máxima 30 de septiembre de 2026).
- Reemplaza en este análisis a `mdi_homicidiosintencionales_pm_2014_2026.xlsx`, que llega solo hasta mayo de 2026.

## Pregunta y cobertura

La pieza mide cuántos menores de 18 años fueron víctimas de muertes violentas intencionales (asesinato, homicidio, femicidio y sicariato) y dónde. Para comparar 2026 con los años anteriores se usa siempre el mismo período, enero a septiembre de cada año.

## Tratamiento

- Edad en años a partir de `edad` y `medida_edad` (años, meses o días); la coma decimal se convierte a punto. Menor de edad: 0 a menos de 18 años.
- Se excluyen de los conteos de menores los registros sin edad válida: 2,9% de los casos en 2025 y en 2026 (3,5% en 2023).
- Gráfico por año: menores por año, 2014 a 2026, con variación 2026 frente a 2025.
- Gráfico por provincia: menores acumulados de 2020 a 2026 por `provincia`, con la participación sobre el total nacional con provincia registrada. `STO DGO DE LOS TSÁCHILAS` se muestra como Santo Domingo.
- Cifras clave: 485 menores en 2026 contra 433 en 2025 (+12,0%); Guayas 847 de 1.721 (49,2%).

## Reproducción

```bash
Rscript scripts/data-cleaning/clean_homicidios_menores.R
Rscript scripts/plots/plot_homicidios_menores_b_evolucion.R
Rscript scripts/plots/plot_homicidios_menores_a_provincias.R
```

Ejecutar con una configuración regional UTF-8 (por ejemplo `LC_ALL=C.UTF-8`) para que los acentos se dibujen bien.

## Origen del código

Los gráficos parten del script `codigo_completo.R` de Odalis Clemente (Gráfico 01 y Gráfico 10), reescrito con el estilo de la casa, rutas relativas y el formato de captions del repositorio.
