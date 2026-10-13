# Ingreso rural y recaudación eléctrica

## Qué mide el borrador

El gráfico compara dos series agregadas de Ecuador entre 2007 y 2025:

- el ingreso laboral mensual promedio de las personas con empleo en el área rural;
- la recaudación anual del servicio eléctrico reportada por ARCONEL.

Ambas series se convierten a dólares de diciembre de 2025 con el IPC nacional del INEC y se presentan como índice, con 2007 = 100. Así se comparan cambios relativos y no niveles que tienen unidades distintas.

La recaudación de ARCONEL es nacional. No mide cuánto pagan los hogares rurales y no permite afirmar que un movimiento cause el otro.

## Fuentes oficiales

- [ENEMDU anual, INEC](https://www.ecuadorencifras.gob.ec/enemdu-anual/)
- [ENEMDU diciembre 2017, microdatos SPSS](https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2017/Diciembre/BASES_DATOS-ENEMDU-SPSS.zip)
- [Compendio Estadístico 2016, tabla de ingreso laboral rural 2007-2016](https://www.ecuadorencifras.gob.ec/documentos/web-inec/Bibliotecas/Compendio/Compendio-2016/Compendio%202016%20DIGITAL.pdf)
- [ENEMDU anual 2021](https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2021/Anual-2021/Anual_2021_Mercado%20Laboral%20y%20Pobreza%20v1.pdf)
- [ENEMDU anual 2022](https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2022/Anual/Principales%20resultados%20de%20Mercado%20Laboral%20y%20Pobreza_Anual%20-%202022.pdf)
- [ENEMDU anual 2023](https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2023/anual/Principales_resultados_de_Mercado_Laboral_y_Pobreza_Anual_2023.pdf)
- [ENEMDU anual 2024](https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2024/anual/Principales-resultados_de_Mercado_Laboral_y_Pobreza_Anual_2024_final.pdf)
- [ENEMDU anual 2025](https://www.ecuadorencifras.gob.ec/documentos/web-inec/EMPLEO/2025/anual/Principales_resultados_de_Mercado_Laboral_y_Pobreza_Anual_2025.pdf)
- [IPC, tabulados y series históricas, INEC](https://www.ecuadorencifras.gob.ec/indice-de-precios-al-consumidor-ipc/)
- [Base oficial de reportes de ARCONEL](https://reportes.arconel.gob.ec/)
- [Publicaciones estadísticas del sector eléctrico, ARCONEL](https://arconel.gob.ec/publicaciones-estadistica-del-sector-electrico-2/)

## Cobertura y comparabilidad

- La serie común empieza en 2007 porque la fuente rural usada no entrega una serie anual comparable para 2000-2006.
- Para 2007-2016 se usa la serie histórica de ingreso laboral rural del INEC, publicada como valores de diciembre.
- Para 2017-2020 se calcula un promedio ponderado con microdatos de ENEMDU de diciembre. Es una medida de diciembre, no un promedio anual. En 2020 no se presenta un agregado anual comparable por las limitaciones de la encuesta durante la pandemia.
- Para 2021-2025 se usan los promedios publicados en los resultados anuales de ENEMDU.
- Para cada año de 2007-2025 se descargó el reporte `Facturación Total` de ARCONEL. La recaudación es la suma del campo `Recaudación Servicio Eléctrico (USD)` en todas las filas del libro, que desagregan empresa, mes, grupo de consumo y tipo de cliente.
- La serie de ARCONEL es un total del servicio eléctrico. Puede incluir clientes regulados y no regulados y no está desagregada por área rural.
- Las dos series describen movimientos agregados. Su coincidencia temporal no prueba causalidad.

## Reproducción

```text
Rscript scripts/data-cleaning/clean_rural_income_electricity.R
Rscript scripts/plots/plot_rural_income_electricity.R
```

El gráfico queda en `outputs/figures/draft_rural_income_electricity-ecuador.png`. La tabla queda en `outputs/tables/draft_rural_income_electricity_annual.csv`. Es un borrador y no debe sincronizarse al sitio público mientras la definición editorial de la comparación no esté cerrada.
