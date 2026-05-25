# Gráficos — El Quantificador

Repositorio para la reproducción y publicación de los gráficos de El Quantificador. Cada gráfico tiene sus datos documentados, un script de limpieza y un script de visualización separados.

## Licencias

El repositorio usa un esquema de licencias separadas:

- **Código y scripts**: [MIT](LICENSE)
- **Imágenes y gráficos del repositorio** (incluyendo `figures/` y `quantificador.png`, salvo nota en contrario): [CC BY-NC 4.0](LICENSE-images)

Los datos crudos, diccionarios, metadatos y otros insumos de terceros dentro de `data/` pueden estar sujetos a sus propias condiciones de uso y no se relicencian automáticamente bajo MIT ni bajo CC BY-NC 4.0.

## Estructura del proyecto

```
graficos-el-quantificador/
├── data/
│   ├── raw/                # Datos crudos organizados por tema/fuente
│   ├── processed/          # Datos procesados generados por los scripts de limpieza (no versionados)
│   └── sources/            # Inventario y fichas de fuentes de datos
├── figures/                # Figuras generadas versionadas en el repositorio
├── scripts/
│   ├── packages.R          # Helper para instalar y cargar paquetes automáticamente
│   ├── utils.R             # Funciones compartidas: tema, logo overlay
│   ├── data-cleaning/      # Scripts de limpieza y preparación de datos
│   └── plots/              # Scripts de visualización
└── quantificador.png       # Logo
```

## Cómo reproducir un gráfico

### Requisitos

- R ≥ 4.3

### Gestión automática de paquetes

Todos los scripts usan `source("scripts/packages.R")` y `ensure_packages(...)` al inicio.

Este helper:

1. Verifica si cada paquete requerido está instalado.
2. Si no está instalado, lo instala automáticamente desde CRAN.
3. Carga el paquete silenciosamente.

Por eso, **ya no es necesario instalar manualmente todos los paquetes antes de ejecutar los scripts**.

Si prefieres preinstalarlos de una vez, puedes usar:

```r
install.packages(c("dplyr", "survey", "ggplot2", "scales", "cowplot", "haven",
                   "data.table", "readxl", "stringr", "lubridate", "ragg", "janitor"))
```

### Convención importante

**Todos los scripts deben ejecutarse desde la raíz del repositorio** (el directorio que contiene `scripts/`, `data/`, `figures/`, etc.).

En RStudio, abre el proyecto y verifica que el directorio de trabajo sea la raíz con `getwd()`.

En línea de comandos:
```bash
cd /ruta/al/repositorio
Rscript scripts/data-cleaning/clean_evolucion_nini.R
Rscript scripts/plots/plot_evolucion_nini.R
```

### Flujo de trabajo

Cada gráfico usa dos scripts:

1. **`clean_*.R`** — Carga los datos crudos, los limpia y guarda el resultado en `data/processed/` como `.rds`.
2. **`plot_*.R`** — Carga los datos procesados, aplica el diseño muestral si corresponde, y genera la figura en `figures/`.

Ejemplo para reproducir el gráfico de ingresos por escolaridad:
```bash
Rscript scripts/data-cleaning/clean_escolaridad_ingreso.R
Rscript scripts/plots/plot_escolaridad_ingreso.R
```

## Gráficos actuales

Los PNG versionados en `figures/` ordenados según la secuencia de publicaciones son:

1. Sin imagen versionada en `figures/` para la publicación `¿Los ecuatorianos más altos ganan más?` (2026-02-13).
2. `02_san-valentin_inflacion-ecuador.png`
3. `03_inactividad-juvenil_sexo-ecuador.png`
4. `04_ingresos_nivel-educativo-ecuador.png`
5. `05_femicidios_contexto-delictivo-ecuador.png`
6. `06_jovenes_viven-con-padres-ecuador.png`
7. `07_desnutricion-cronica_etnia-ecuador.png`
8. `08_ingles_funcion-laboral-ecuador.png`
9. `09_religion_importancia_sudamerica.png`
10. `10_uso-del-tiempo_cocina-sexo-ecuador.png`
11. `11_salarios_publico-privado_inflacion-ecuador.png`
12. `12_exportaciones_eeuu_top10_2024-2025.png`
13. `13_empleo-adecuado_edad_yoy_mar-2026.png`
14. `14_anemia-infantil_quintil-ecuador.png`
15. `15_cuidador-principal_infancia-ecuador.png`
16. `16_ingreso-hogar_distribucion-ecuador.png`
17. `17_salario-industrias_formal-ecuador.png`


## Funciones compartidas (`scripts/utils.R`)

`utils.R` se carga automáticamente con `source("scripts/utils.R")` al inicio de cada script de visualización. Define:

- **`theme_quantificador()`** — Tema ggplot2 base (fondo blanco, tipografía gris, sin cuadrícula).
- **`theme_women()`** — Variante para gráficos de estadísticas de mujeres (base `theme_bw`, paleta morada).
- **`add_logo(plot, ...)`** — Superpone el logo de El Quantificador sobre un ggplot usando `cowplot`.

Para agregar un nuevo gráfico, importa las funciones con `source("scripts/utils.R")` y aplica el tema y el logo:
```r
source("scripts/utils.R")

p <- ggplot(...) + ... + theme_quantificador()
p_final <- add_logo(p)
ggsave("figures/mi_grafico.png", p_final, ...)
```

## Notas sobre los datos

Los archivos de datos crudos (`.sav`, `.xlsx`, `.csv`, `.xls`, `.rds`) viven bajo `data/raw/`, organizados por tema o fuente. El inventario y las fichas metodológicas están en `data/sources/`, empezando por `data/sources/README.md`.

Los archivos procesados (`data/processed/`) tampoco se versionan: son derivados de los datos crudos y pueden regenerarse en cualquier momento ejecutando el script `clean_*.R` correspondiente.

Las figuras PNG en `figures/` sí se versionan en este repositorio como salidas publicadas. Los PDF generados se ignoran.
