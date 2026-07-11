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
│   ├── data-cleaning/
│   └── plots/
└── quantificador.png
```

## Catalogo de visualizaciones

El sitio de El Quantificador sincroniza `outputs/chart_catalog/chart_catalog.csv` para crear o actualizar paginas bajo `content/visualizaciones` y referenciar las imagenes publicadas.

- Las imagenes publicadas viven en `outputs/figures/`.
- `outputs/chart_catalog/chart_catalog.csv` es el unico catalogo de visualizaciones y se edita manualmente.

## Graficos actuales

1. `30_aranceles-colombia-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_aranceles_colombia.R)
2. `31_gasolina-vs-transporte-publico_quintil-ingreso-ecuador.png` - [script](https://github.com/elquantificador/graficos-el-quantificador/blob/main/scripts/plots/plot_enighur_gasolina_transporte_quintiles.R)

## Notas sobre los datos

Los archivos de datos crudos viven bajo `data/raw/`, organizados por tema o fuente. El inventario y las fichas metodologicas estan en `data/sources/`, empezando por `data/sources/README.md`.


