# Fuente de datos: habilidades demandadas en ofertas de empleo

## Dataset

**Ofertas de empleo relacionadas con datos y tecnología en Ecuador**

## Proveedor

Datos públicos recopilados por el proyecto vecino `data-jobs-scrape` desde Multitrabajos, Computrabajo y Mipleo.

## Acceso

Instantáneas utilizadas:

- `data/raw/jobs_scrape/jobs_20260617_043911.csv`
- `data/raw/jobs_scrape/jobs_20260716_013707.csv`

Las instantáneas corresponden al 17 de junio y al 16 de julio de 2026.

## Variables utilizadas

- `source`: portal de empleo de origen.
- `job_id`: identificador de la oferta dentro del portal.
- `in_universe`: indicador de pertenencia al universo de empleos de datos, empleos adyacentes a datos y empleos intensivos en tecnología.
- `skills`: herramientas y conocimientos detectados en la oferta.
- `scraped_at`: fecha y hora de recopilación.

## Notas

- Las ofertas repetidas entre instantáneas se deduplican por `source` y `job_id`, conservando el registro más reciente.
- El ranking se restringe a herramientas técnicas concretas: software, lenguajes, productos de bases de datos, plataformas de nube, bibliotecas, sistemas operativos, APIs y tecnologías de infraestructura.
- Se excluyen conocimientos de dominio o conceptos transversales como KPIs, análisis de datos, estadística, dashboards, inteligencia de negocios y ERP genérico.
- La base del porcentaje son las ofertas clasificadas dentro del universo que declaran al menos una habilidad.
- Una oferta puede mencionar más de una herramienta, por lo que los porcentajes no suman 100%.
