# Fuente de datos: Ser Estudiante, puntaje global por provincia

## Dataset
**Ser Estudiante 2024-2025 — Evaluación nacional, microdatos**

## Proveedor
Instituto Nacional de Evaluación Educativa (INEVAL), Ecuador

## Archivo utilizado

- `data/raw/sest/SEST25_micro_50578_20251215_SAV.sav`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `inev` | Promedio global o nota global obtenida por el sustentante |
| `fex_inev` | Factor de expansión para inferencia del puntaje global |
| `id_prov` | Provincia |
| `grado` | Subnivel o nivel del sustentante |
| `estado_eval` | Estado de evaluación |

## Nota metodológica

- Se conserva únicamente a los sustentantes con estado `Evaluado`.
- El promedio provincial se calcula como media ponderada de `inev` usando `fex_inev`.
- Se excluyen `Zona No Delimitada` y `En el Exterior`.
- El script también guarda un corte por provincia y grado para usos posteriores.

## Scripts asociados

- Limpieza: `scripts/data-cleaning/clean_sest_puntaje_provincia.R`
