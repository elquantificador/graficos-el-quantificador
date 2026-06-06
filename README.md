# Gráficos — El Quantificador

Repositorio para la reproducción y publicación de los gráficos de El Quantificador. Cada gráfico tiene sus datos documentados, un script de limpieza y un script de visualización separados.

## Licencias

El repositorio usa un esquema de licencias separadas:

- **Código y scripts**: [MIT](LICENSE)
- **Imágenes y gráficos del repositorio** (incluyendo `outputs/figures/` y `quantificador.png`, salvo nota en contrario): [CC BY-NC 4.0](LICENSE-images)

Los datos crudos, diccionarios, metadatos y otros insumos de terceros dentro de `data/` pueden estar sujetos a sus propias condiciones de uso y no se relicencian automáticamente bajo MIT ni bajo CC BY-NC 4.0.

## Estructura del proyecto

```
graficos-el-quantificador/
├── data/
│   ├── raw/                # Datos crudos organizados por tema/fuente
│   ├── processed/          # Datos procesados generados por los scripts de limpieza (no versionados)
│   └── sources/            # Inventario y fichas de fuentes de datos
├── outputs/
│   ├── figures/            # Figuras generadas versionadas en el repositorio
│   └── tables/             # Tablas generadas (Excel/HTML)
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

**Todos los scripts deben ejecutarse desde la raíz del repositorio** (el directorio que contiene `scripts/`, `data/`, `outputs/`, etc.).

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
2. **`plot_*.R`** — Carga los datos procesados, aplica el diseño muestral si corresponde, y genera la figura en `outputs/figures/`.

Ejemplo para reproducir el gráfico de ingresos por escolaridad:
```bash
Rscript scripts/data-cleaning/clean_escolaridad_ingreso.R
Rscript scripts/plots/plot_escolaridad_ingreso.R
```

## Gráficos actuales

Los PNG versionados en `outputs/figures/` ordenados según la secuencia de publicaciones son:

1. `01_altura-ingresos_ensanut-2018.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_ecuatorianos_altos.R)
2. `02_san-valentin_inflacion-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_san_valentin.R)
3. `03_inactividad-juvenil_sexo-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_evolucion_nini.R)
4. `04_ingresos_nivel-educativo-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_escolaridad_ingreso.R)
5. `05_femicidios_contexto-delictivo-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_femicidios.R)
6. `06_jovenes_viven-con-padres-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_padres_hijos_censo.R)
7. `07_desnutricion-cronica_etnia-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_endi_desnutricion.R)
8. `08_ingles_funcion-laboral-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_ef_epi_job.R)
9. `09_religion_importancia_sudamerica.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_wvs_religion_importance.R)
10. `10_uso-del-tiempo_cocina-sexo-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_uso_tiempo.R)
11. `11_salarios_publico-privado_inflacion-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_salario_real_vs_nominal.R)
12. `12_exportaciones_eeuu_top10_2024-2025.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_exportaciones_eeuu.R)
13. `13_empleo-adecuado_edad_yoy_mar-2026.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enemdu_empleo_adecuado_edad_nivel_yoy_mensual_raw.R)
14. `14_anemia-infantil_quintil-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_endi_anemia_quintil.R)
15. `15_cuidador-principal_infancia-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_endi_cuidador_principal.R)
16. `16_ingreso-hogar_distribucion-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enemdu_ingreso_hogar_distribution.R)
17. `17_salario-industrias_formal-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_reess_salario_industrias.R)
18. `18_personal-salud-publica-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_ras_personal_salud.R)
19. `19_juguetes-infancia-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_endi_juguetes.R)
20. `20_descomposicion-ingreso-hogar-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enighur_ingreso_gasto.R)


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
ggsave("outputs/figures/mi_grafico.png", p_final, ...)
```

## Notas sobre los datos

Los archivos de datos crudos (`.sav`, `.xlsx`, `.csv`, `.xls`, `.rds`) viven bajo `data/raw/`, organizados por tema o fuente. El inventario y las fichas metodológicas están en `data/sources/`, empezando por `data/sources/README.md`.

Los archivos procesados (`data/processed/`) tampoco se versionan: son derivados de los datos crudos y pueden regenerarse en cualquier momento ejecutando el script `clean_*.R` correspondiente.

Las figuras PNG en `outputs/figures/` sí se versionan en este repositorio como salidas publicadas. Los PDF generados se ignoran.
Las tablas derivadas en Excel/HTML se escriben en `outputs/tables/`.


