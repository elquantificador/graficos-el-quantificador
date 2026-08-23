# Muertes intencionales y desapariciones

## Fuentes

- Repositorio: [LIDE-Grafico_Crimen](https://github.com/angel-cloud976/LIDE-Grafico_Crimen)
- Versión usada: commit `6ce297fce7153dbb591c861bb79a839f3cd3d2dd`
- Muertes intencionales: `mdi_homicidiosintencionales_pm_2014_2026.xlsx`
- Desapariciones: `mdi_personasdesaparecidas_pm_2017_2026.xlsx`
- Presupuesto policial: `Gasto_proforma.xlsx`
- Copias locales: `data/raw/crimen/`

## Pregunta y cobertura

La pieza compara la evolución anual de las muertes intencionales, las desapariciones registradas y el presupuesto liquidado de la Policía Nacional. Se usan años completos de 2017 a 2025; los registros parciales de 2026 quedan fuera.

## Tratamiento

- Se consideran como muertes intencionales los registros tipificados como `ASESINATO`, `HOMICIDIO`, `FEMICIDIO` y `SICARIATO`.
- Las desapariciones se cuentan por año de `fecha_desaparicion`.
- El presupuesto corresponde a la entidad `052 - POLICÍA NACIONAL` y usa `pre_liquidado`.
- El presupuesto se muestra en el eje derecho; las dos series de casos usan el eje izquierdo.
- La serie final contiene 36.502 muertes intencionales y 75.699 desapariciones entre 2017 y 2025.

## Copy de publicación

En Ecuador, las muertes intencionales pasaron de 970 casos en 2017 a 9.282 en 2025. En el mismo periodo, las desapariciones registradas bajaron de 10.479 a 7.505, mientras el presupuesto liquidado de la Policía Nacional subió de USD 1,20 mil millones a USD 1,61 mil millones.

La serie de muertes intencionales reúne asesinatos, homicidios, femicidios y sicariatos. Las desapariciones incluyen todos los casos registrados, sin distinguir su desenlace.

Elaboración: Angel Alava González para El Quantificador.

#Ecuador #Seguridad #ElQuantificador

## Reproducción

```powershell
Rscript scripts/data-cleaning/clean_crimen_desapariciones.R
Rscript scripts/plots/plot_crimen_desapariciones.R
```
