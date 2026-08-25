# Fuente de datos: remesas recibidas y comparación regional

## Objetivo

Verificar de forma independiente los datos necesarios para el gráfico
comparativo de remesas de Ecuador, Colombia y Perú.

## Fuentes primarias

- Banco Mundial, indicador `BX.TRF.PWKR.CD.DT`, *Personal remittances,
  received (current US$)*. La serie se descarga mediante el paquete R `WDI`,
  que consulta la API oficial para Ecuador, Colombia y Perú.
- Banco Mundial, indicador `NY.GDP.MKTP.CD`, *GDP (current US$)*. La serie se
  descarga mediante el paquete R `WDI` para calcular las remesas de Ecuador
  como porcentaje del PIB.
- Banco Central del Ecuador, *Boletín Analítico de la Evolución Anual de
  Remesas, Año 2025*.
- Banco Central del Ecuador, base histórica `RemesasIntegradoWEB_PUB.xlsx`.
  La composición por país de procedencia de 2025 proviene de la Figura 3 y
  del texto del boletín: Estados Unidos USD 6.010,1 millones, España USD
  1.088,0 millones, Italia USD 152,6 millones y resto del mundo USD 478,8
  millones.

## Reproducción

```powershell
Rscript scripts/data-cleaning/clean_remesas_regional.R
```

El script descarga la serie del Banco Mundial mediante `WDI`, el boletín PDF
del BCE y su base histórica. También genera:

- `data/raw/remesas_regional/world_bank_remesas_wdi.csv`
- `data/raw/remesas_regional/world_bank_ecuador_gdp_wdi.csv`
- `outputs/tables/remesas_ecuador_bce_world_bank.csv`
- `outputs/tables/remesas_regional_growth_world_bank.csv`
- `outputs/tables/remesas_origen_bce_2025.csv`
- `data/processed/remesas_regional_world_bank_bce.rds`

## Nota metodológica

El indicador del Banco Mundial se basa en datos de balanza de pagos del FMI y
estimaciones del Banco Mundial. El BCE publica la medición oficial ecuatoriana
de remesas recibidas. Ambas fuentes miden el flujo en dólares corrientes, pero
pueden diferir por revisiones, calendarios de publicación o cobertura de la
compilación. Para la comparación regional se usa una sola fuente, el Banco
Mundial. Para Ecuador se conserva la comparación BCE-Banco Mundial como prueba
de consistencia, no como una mezcla silenciosa de series.

## Verificación ejecutada el 25 de agosto de 2026

- La API del Banco Mundial devolvió 26 observaciones por país, de 2000 a 2025,
  sin faltantes, años duplicados ni valores negativos.
- Para Ecuador, Banco Mundial y BCE difieren entre 0,06% y 0,39% del valor
  publicado por el BCE entre 2016 y 2025. La diferencia máxima es de USD 10,1
  millones en 2016 y cae a USD 4,6 millones en 2025.
- La base histórica XLSX del BCE expresa los totales en miles de USD. Sus
  observaciones 2016–2018 coinciden con el boletín anual del BCE después de
  convertir unidades y redondear a una decimal.
- Para el gráfico regional, la comparación consistente es Banco Mundial para
  los tres países: Ecuador 95,7%, Colombia 71,5% y Perú 69,7% entre 2020 y
  2024.
- Para el gráfico 40_b, la participación de Estados Unidos proviene del BCE y
  se verifica contra el total anual: USD 6.010,1 millones / USD 7.729,5
  millones = 77,8% después de redondear a una decimal.
- Para el gráfico 40_c, la producción usa exclusivamente las series del
  Banco Mundial de 2000 a 2025: las remesas se dividen por el PIB nominal de
  Ecuador para obtener el porcentaje del PIB, mientras el eje secundario
  conserva el monto absoluto en dólares corrientes.
