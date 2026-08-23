# Desapariciones sin resolver

## Fuente

- Repositorio: [Github-Eddie-Tomala](https://github.com/EDDIETOMALAFIGUEROA/Github-Eddie-Tomala)
- Versión usada: commit `407ebdf041b652f0a4f17105eb483443c2cdfeb1`
- Archivo: `data/raw/mdi/mdi_personasdesaparecidas_pm_2017_2025.xlsx`
- Copia local: `data/raw/desapariciones/mdi_personasdesaparecidas_pm_2017_2025.xlsx`

## Pregunta y cobertura

La pieza muestra qué proporción de las denuncias anuales de desaparición permanece en estado `DESAPARECIDO`. Se usan años completos de 2017 a 2025.

## Tratamiento

- Los casos se agrupan por año de `fecha_desaparicion` y `situacion_actual`.
- El denominador es el total de denuncias con estado disponible en cada año.
- Solo se muestra el estado `DESAPARECIDO`; los casos `ENCONTRADO` y otros estados quedan dentro del denominador.
- En 2025, los casos que permanecen desaparecidos representan 10,5% del total anual.

## Copy de publicación

Cada vez más denuncias de desaparición permanecen sin resolver: la proporción pasó de 0,8% en 2017 a 10,5% en 2025.

Elaboración: Eddie Bryan Tomalá Figueroa para El Quantificador.

#Ecuador #Seguridad #ElQuantificador

## Reproducción

```powershell
Rscript scripts/data-cleaning/clean_desaparecidos_fatalidad.R
Rscript scripts/plots/plot_desaparecidos_fatalidad.R
```
