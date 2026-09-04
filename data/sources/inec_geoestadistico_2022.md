# Marco Geoestadístico 2022 del INEC

## Fuente

- Servicio oficial: https://idgn.ecuadorencifras.gob.ec/server/rest/services/Hosted/Marco_Geoestadistico_2022/FeatureServer
- Capas descargadas: límite provincial, límite cantonal y límite parroquial. El mapa usa el límite provincial.
- Consulta realizada: 2 de septiembre de 2026.

## Copia local

Las capas se guardan en `data/raw/inec_geoestadistico_2022/shapefile/`:

- `provincias/provincias.shp`
- `cantones/cantones.shp`
- `parroquias/parroquias.shp`

Son segmentos `LINESTRING` en WGS 84. Se descargaron por lotes porque el
servicio limita cada consulta a 2.000 registros. La copia completa contiene
3.844 segmentos provinciales, 6.442 cantonales y 10.098 parroquiales.
La copia se conserva localmente y no se versiona en Git por su tamaño.

## Uso en la réplica

Estas geometrías corresponden al año 2022 y funcionan como referencia
territorial. El gráfico de conflicto debe filtrar y acumular los eventos ACLED
del período 2018–2025. La geometría no debe interpretarse como un filtro de
fechas.
