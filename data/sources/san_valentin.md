# Fuente de datos: IPC San Valentín

## Dataset
**Índice de Precios al Consumidor (IPC)**
Serie mensual, enero 2015 – enero 2026

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Portal del INEC: https://www.ecuadorencifras.gob.ec/indice-de-precios-al-consumidor/

Archivo utilizado: `data/raw/ipc/ipc_ind_nac_reg_ciud_01_2026.xlsx` (segunda hoja)

## Categorías CCIF utilizadas

| Código CCIF | Descripción |
|---|---|
| `11` | Restaurantes y hoteles |
| `01182094` | Chocolate |
| `09421293` | Entradas al cine |
| `09331286` | Flores |

## Metodología
La inflación acumulada se calcula como la variación porcentual del índice entre enero de 2016 y enero de 2026:

```
Inflación = (IPC_ene_2026 − IPC_ene_2016) / IPC_ene_2016
```

## Notas
- Las columnas en el Excel usan abreviaturas de meses en español con año de dos dígitos (e.g., `ene_16`, `ene_26`).
- La función `parse_month_any()` en `scripts/data-cleaning/clean_san_valentin.R` maneja este formato.
