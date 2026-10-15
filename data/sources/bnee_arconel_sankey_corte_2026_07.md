# Origen y destino de la energía eléctrica, corte a julio de 2026

## Fuente

- Agencia de Regulación y Control de Electricidad (ARCONEL), *Balance Nacional de Energía Eléctrica (BNEE)*, corte a julio de 2026.
- Página que enlaza la descarga: https://arconel.gob.ec/balance-nacional-de-energia-electrica/
- Archivo original: `data/raw/BNEE_julio_2026.xls`.
- Limpieza: `scripts/data-cleaning/clean_bnee_arconel_sankey_corte_2026_07.R`.
- El BNEE cubre el año móvil de agosto de 2025 a julio de 2026.

## Origen

El total publicado de `Producción e Importaciones` es 41.183,410506902 GWh. Los flujos de origen suman ese total: hidráulica, 28.823,828174203 GWh; no renovable, 11.251,164509126 GWh; importaciones, 456,554302562 GWh; y otras renovables, 651,863521011 GWh. Otras renovables agrupa eólica, fotovoltaica, biomasa y biogás. La cifra de otras renovables se calcula como residuo para que los cuatro flujos cierren con el total publicado; la diferencia frente a la suma directa de las cuatro tecnologías es menor a una millonésima de GWh.

## Destinos

El BNEE registra 36.557,145120126 GWh como `Energía Entregada para Servicio Eléctrico`. El resto hasta `Producción e Importaciones`, 4.626,265386776 GWh, corresponde al residual que ARCONEL describe como energía no disponible para servicio público, usada internamente en procesos productivos y de explotación, principalmente por empresas petroleras. La definición se documenta en la [Estadística Anual y Multianual del Sector Eléctrico 2023](https://arconel.gob.ec/wp-content/uploads/downloads/2024/07/Estadistica-Anual-y-Multianual-del-Sector-Electrico-Ecuatoriano-2023.pdf).

Los demás destinos se reconstruyen con las relaciones contables del BNEE:

- `Pérdidas en Transmisión`: 1.727,721502195 GWh.
- `Energía Disponible para Servicio Eléctrico`: 34.829,423617931 GWh, después de las pérdidas de transmisión.
- `Exportación`: 88,080289353 GWh.
- `Disponible Sistemas de Distribución`: 34.741,343328578 GWh, después de exportaciones.
- `Consumo Total Energía Eléctrica`: 29.014,446717833 GWh.
- `Pérdidas en Distribución`: 5.726,896610745 GWh.

El BNEE desglosa las pérdidas de distribución en 2.061,969943022 GWh técnicas y 3.664,926667723 GWh no técnicas. Suman el total publicado. Equivalen a 5,0% y 8,9% de la producción e importaciones, respectivamente; juntas, 13,9%. Sobre la energía disponible para los sistemas de distribución, representan 5,94% y 10,55%.

Los seis destinos del flujo suman el total publicado de producción e importaciones. Las diferencias residuales inferiores a 0,000001 GWh se deben al redondeo binario de las cifras del archivo.

## Lectura y límites

ARCONEL describe las pérdidas técnicas como pérdidas físicas en redes y equipos, y las no técnicas como energía suministrada que no se mide o registra comercialmente; entre sus ejemplos están los medidores dañados o alterados, errores de lectura o facturación y conexiones ilegales. Fuente: [Estadística del Sector Eléctrico 2025, sección 1.4 y glosario](https://arconel.gob.ec/wp-content/uploads/downloads/2026/03/Estadistica2025.pdf). El BNEE usado aquí no desglosa las pérdidas de transmisión por causa ni las pérdidas de distribución por empresa, provincia, fuente de generación o momento. Tampoco asigna una fuente particular de generación a un destino. `Consumo total` es el valor que ARCONEL utiliza para calcular pérdidas y difiere de la energía facturada; su nota indica que se ajusta al número de días calendario de cada mes. Este balance no identifica las causas ni los horarios de interrupciones en los hogares.
