# Revisión R: `clean_enemdu_nini_razones_sexo.R`

## Resumen por severidad

- Crítica: 0
- Alta: 0
- Media: 0
- Baja: 0

Estado: listo para uso.

## Hallazgos

No se encontraron hallazgos que requieran cambios.

## Comprobaciones realizadas

- **Correctitud:** el universo reproduce 865.199 jóvenes NINI y 77,3% de mujeres, dentro de las tolerancias declaradas.
- **Códigos:** `p02`, `p07`, `p09`, `p34`, `nnivins` y `empleo` se verificaron contra las etiquetas oficiales de la ENEMDU anual 2025.
- **Corrección documentada:** el script asigna correctamente `p09 = 16` a falta de recursos tecnológicos y `p09 = 17` a otra razón.
- **Porcentajes:** las distribuciones por sexo suman 100% en las secciones de estudio y trabajo.
- **Reproducibilidad:** todas las rutas son relativas a la raíz y el script lee directamente el CSV contenido en el ZIP oficial sin modificar el insumo crudo.
- **Salida:** el objeto procesado se guarda en `data/processed/enemdu_nini_razones_sexo.rds`.
- **Convenciones:** usa el gestor de paquetes del repositorio, nombres `snake_case`, tubería nativa, secciones numeradas y una única salida de estado.

## Verificación ejecutada

```text
Rscript scripts/data-cleaning/clean_enemdu_nini_razones_sexo.R
Guardado: data/processed/enemdu_nini_razones_sexo.rds
```

