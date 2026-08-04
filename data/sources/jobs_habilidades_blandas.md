# Fuente de datos: habilidades blandas en ofertas de empleo

## Dataset

**Ofertas de empleo relacionadas con datos y tecnología en Ecuador**

## Proveedor

Datos públicos recopilados por el proyecto vecino `data-jobs-scrape` desde Multitrabajos, Computrabajo y Mipleo.

## Acceso

Instantáneas utilizadas:

- `data/raw/jobs_scrape/jobs_20260617_043911.csv`
- `data/raw/jobs_scrape/jobs_20260716_013707.csv`

Las instantáneas corresponden al 17 de junio y al 16 de julio de 2026 (las mismas usadas en el gráfico de habilidades técnicas, `jobs_habilidades_demandadas.md`).

## Variables utilizadas

- `source`: portal de empleo de origen.
- `job_id`: identificador de la oferta dentro del portal.
- `in_universe`: indicador de pertenencia al universo de empleos de datos, empleos adyacentes a datos y empleos intensivos en tecnología.
- `skills`: herramientas y conocimientos detectados en la oferta.
- `description`: texto completo de la oferta.
- `scraped_at`: fecha y hora de recopilación.

## Notas

- Las ofertas repetidas entre instantáneas se deduplican por `source` y `job_id`, conservando el registro más reciente.
- A diferencia del gráfico de habilidades técnicas, la base analítica no exige que `skills` esté declarado: se incluye toda oferta del universo con `description` disponible, ya que las competencias blandas se detectan sobre todo el texto de la oferta (`skills` + `description`).
- Las competencias se identifican mediante un diccionario de expresiones regulares (comunicación, trabajo en equipo, pensamiento analítico, organización y planificación, proactividad e iniciativa, orientación a resultados, resolución de problemas, liderazgo, adaptabilidad, negociación, creatividad e innovación, trabajo bajo presión).
- La base del porcentaje son las ofertas del universo con descripción disponible (434 ofertas en los cortes de junio-julio de 2026).
- Una oferta puede mencionar más de una competencia, por lo que los porcentajes no suman 100%.
