# Graficos - El Quantificador

Repositorio para la reproduccion y publicacion de los graficos de El Quantificador. Cada grafico tiene sus datos documentados, un script de limpieza y un script de visualizacion separados.

## Licencias

El repositorio usa un esquema de licencias separadas:

- **Codigo y scripts**: [MIT](LICENSE)
- **Imagenes y graficos del repositorio** (incluyendo `outputs/figures/` y `quantificador.png`, salvo nota en contrario): [CC BY-NC 4.0](LICENSE-images)

## Estructura del proyecto

```
graficos-el-quantificador/
├── data/
│   ├── raw/
│   ├── processed/
│   └── sources/
├── outputs/
│   ├── figures/
│   ├── tables/
│   └── chart_catalog/
├── scripts/
│   ├── packages.R
│   ├── utils.R
│   ├── validate_chart_catalog.py
│   ├── templates/
│   ├── data-cleaning/
│   └── plots/
└── quantificador.png
```

## Catalogo de visualizaciones

El sitio de El Quantificador sincroniza `outputs/chart_catalog/chart_catalog.csv` para crear o actualizar paginas bajo `content/visualizaciones` y referenciar las imagenes publicadas.

- Las imagenes publicadas viven en `outputs/figures/`.
- `outputs/chart_catalog/chart_catalog.csv` es el unico catalogo de visualizaciones y se edita manualmente.
- Cada fila debe tener un `Chart ID` canonico y un `Status` explicito.
- La documentacion del catalogo vive en `outputs/chart_catalog/README.md`.
- Antes de commit o push, corre `python scripts/validate_chart_catalog.py`.

## Lifecycle de graficos

Estados permitidos en `Status`:

- `published`: grafico ya publicado y sincronizable al sitio.
- `draft`: grafico en preparacion, todavia no publico.
- `supplementary`: variante o pieza complementaria asociada a una principal.
- `archived`: pieza guardada para referencia, no destinada a publicacion actual.
- `hold`: idea o artefacto congelado temporalmente.

## Flujo recomendado para un grafico nuevo

1. Colocar datos crudos en `data/raw/[source]/`.
2. Crear `scripts/data-cleaning/clean_[source]_[topic].R`.
3. Crear `scripts/plots/plot_[source]_[topic].R`.
4. Generar el `.png` en `outputs/figures/`.
5. Registrar o actualizar la fila en `outputs/chart_catalog/chart_catalog.csv`.
6. Validar con `python scripts/validate_chart_catalog.py`.
7. Actualizar `README.md`, `TODO.md` y `data/sources/README.md` cuando haga falta.

## Plantillas reutilizables

Hay esqueletos base para acelerar piezas nuevas en `scripts/templates/clean_template.R` y `scripts/templates/plot_template.R`.

## Graficos actuales

1. `30_aranceles-colombia-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_aranceles_colombia.R)
2. `31_gasolina-vs-transporte-publico_quintil-ingreso-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enighur_gasolina_transporte_quintiles.R)
3. `31_b_gasolina-share_quintil-ingreso-2012-2025.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enighur_gasolina_share_quintiles_years.R)
4. `32_habilidades-demandadas-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_jobs_habilidades_mensuales.R)
5. `33_a_horas-promedio_sector-formal-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enemdu_horas_sector_formal_linea.R)
6. `33_b_horas-promedio_sector-informal-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enemdu_horas_sector_informal_linea.R)
7. `36_a_ranking-informalidad_provincia-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enemdu_informalidad_provincial.R)
8. `36_b_informalidad-empleo-no-remunerado_provincia-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enemdu_informalidad_provincial.R)
9. `37_a_ninis-razones-estudio-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enemdu_nini_razones_sexo.R)
10. `37_b_ninis-razones-trabajo-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enemdu_nini_razones_sexo.R)
11. `38_hora-asesinatos-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_homicidios_hora.R)
12. `39_a_desapariciones-sin-resolver-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_desaparecidos_fatalidad.R)
13. `39_b_crimen-presupuesto-ecuador.png` - [draft script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_crimen_desapariciones.R)

## Notas sobre los datos

Los archivos de datos crudos viven bajo `data/raw/`, organizados por tema o fuente. El inventario y las fichas metodologicas estan en `data/sources/`, empezando por `data/sources/README.md`.
