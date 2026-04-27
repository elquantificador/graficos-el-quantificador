# Inventario de fuentes de datos

Este directorio documenta los insumos crudos almacenados en `data/raw/` y las fichas metodológicas asociadas a cada gráfico o análisis.

## Estructura esperada

- `data/raw/` contiene exclusivamente datos crudos u hojas extraídas listas para limpieza.
- `data/processed/` contiene derivados generados por los scripts de `scripts/data-cleaning/`.
- `data/sources/` contiene fichas descriptivas, notas metodológicas e inventario.

## Inventario actual de datasets crudos

| Tema | Ruta en `data/raw/` | Estado | Fuente |
|---|---|---|---|
| ENEMDU mensual enero 2026 | `enemdu/enemdu_persona_2026_01.sav` | Usado en `clean_escolaridad_ingreso.R` | `escolaridad_ingreso.md` |
| ENEMDU trimestral 2021–2025 | `enemdu/evolucion/*.csv` | Usado en `clean_evolucion_nini.R` | `evolucion_nini.md` |
| ENDI R2 | `endi_r2/BDD_ENDI_R2_f1_personas.rds` | Usado en `clean_endi_desnutricion.R` | `endi_desnutricion.md` |
| Uso del tiempo 2019 | `uso_tiempo/201912_multibdd_*.sav.csv` | Usado en `clean_uso_tiempo.R` | `uso_tiempo.md` |
| Censo 2010 y 2022 | `censo/censo_padres_hijos_*.xlsx` | Usado en `clean_padres_hijos_censo.R` | `padres_hijos_censo.md` |
| IPC enero 2026 | `ipc/ipc_ind_nac_reg_ciud_01_2026.xlsx` | Usado en `clean_san_valentin.R` | `san_valentin.md` |
| IPC marzo 2026 | `ipc/ipc_ind_nac_reg_ciud_03_2026.xlsx` | Usado en `clean_salario_ipc.R` | `salario_ipc.md` |
| REESS enero 2026 | `reess/REESS Indicadores Laborales_Empleo_01_2026.xlsx` | Usado en `clean_salario_ipc.R` | `salario_ipc.md` |
| Fiscalía muertes de mujeres | `fiscalia/muertes_fem_fiscalia_2026.csv` | Usado en `clean_femicidios.R` | `femicidios.md` |
| Exportaciones BCE | `exportaciones/05. Export. por Producto Principal y País.xlsx` | Usado en `clean_exportaciones_eeuu.R` | `exportaciones_eeuu.md` |
| EF EPI Ecuador 2025 | `ef_epi/ef_epi_ecuador_extracted.xlsx` | Usado en `clean_ef_epi_job.R` | `ef_epi_job.md` |
| WVS religión | `wvs/wvs_importance_of_religion_in_life.xls` | Usado en `clean_wvs_religion_importance.R` | `wvs_religion_importance.md` |
| Ser Estudiante 2024–2025 | `sest/SEST25_micro_50578_20251215_SAV.sav` | Cargado, aún sin script de limpieza/plot | `sest25_microdata.md` |

## Cobertura de documentación

- Cada dataset usado por un script debe tener una ficha en este directorio.
- Si se agrega un archivo nuevo bajo `data/raw/`, debe registrarse aquí aunque todavía no tenga gráfico asociado.
