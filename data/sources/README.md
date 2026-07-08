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
| ENSANUT 2018 personas | `ensanut/1_BDD_ENS2018_f1_personas.dta.zip` | Usado en `clean_ecuatorianos_altos.R` | `ecuatorianos_altos.md` |
| ENEMDU trimestral 2021–2025 | `enemdu/evolucion/*.csv` | Usado en `clean_evolucion_nini.R` | `evolucion_nini.md` |
| ENDI R2 | `endi_r2/BDD_ENDI_R2_f1_personas.rds` | Usado en `clean_endi_desnutricion.R` | `endi_desnutricion.md` |
| ENDI R2 anemia por quintil | `endi_r2/BDD_ENDI_R2_f1_personas.rds` | Usado en `clean_endi_anemia_quintil.R` | `endi_anemia_quintil.md` |
| ENDI R2 juguetes y juego infantil | `endi_r2/BDD_ENDI_R2_f3_desarrollo_inf.rds` | Usado en `clean_endi_juguetes.R` | `endi_juguetes.md` |
| Uso del tiempo 2019 | `uso_tiempo/201912_multibdd_*.sav.csv` | Usado en `clean_uso_tiempo.R` | `uso_tiempo.md` |
| Censo 2010 y 2022 | `censo/censo_padres_hijos_*.xlsx` | Usado en `clean_padres_hijos_censo.R` | `padres_hijos_censo.md` |
| IPC enero 2026 | `ipc/ipc_ind_nac_reg_ciud_01_2026.xlsx` | Usado en `clean_san_valentin.R` | `san_valentin.md` |
| IPC marzo 2026 | `ipc/ipc_ind_nac_reg_ciud_03_2026.xlsx` | Usado en `clean_salario_ipc.R` | `salario_ipc.md` |
| REESS enero 2026 | `reess/REESS Indicadores Laborales_Empleo_01_2026.xlsx` | Usado en `clean_salario_ipc.R` | `salario_ipc.md` |
| REESS febrero 2026 | `reess/Indicadores Laborales_Empleo_02_2026.xlsx` | Usado en `clean_reess_salario_industrias.R` | `reess_salario_industrias.md` |
| RAS MSP nacional | `ras/msp_serie_nac.rds` | Usado en `clean_ras_personal_salud.R` | `ras_personal_salud.md` |
| Fiscalía muertes de mujeres | `fiscalia/muertes_fem_fiscalia_2026.csv` | Usado en `clean_femicidios.R` | `femicidios.md` |
| Exportaciones BCE | `exportaciones/05. Export. por Producto Principal y País.xlsx` | Usado en `clean_exportaciones_eeuu.R` | `exportaciones_eeuu.md` |
| EF EPI Ecuador 2025 | `ef_epi/ef_epi_ecuador_extracted.xlsx` | Usado en `clean_ef_epi_job.R` | `ef_epi_job.md` |
| WVS religión | `wvs/wvs_importance_of_religion_in_life.xls` | Usado en `clean_wvs_religion_importance.R` | `wvs_religion_importance.md` |
| WVS Ecuador actitudes hacia la homosexualidad | `wvs/WVSEcuador.dta` | Usado en `clean_wvs_homosexualidad_vecinos.R` y `clean_wvs_homosexualidad_padres.R` | `wvs_homosexualidad.md` |
| RAS nacional MSP | `ras/msp_serie_nac.rds` | Usado en `clean_ras_personal_salud.R` | `ras_personal_salud.md` |
| ENIGHUR tabulados de ingresos, gastos y promedios | `enighur/cuadro_2_1_1_ingresos.rds`, `cuadro_2_1_3_gastos.rds`, `cuadro_2_2_1_promedios.rds`, `mapeo_categorias_gasto.rds` | Usado en `clean_enighur_ingreso_gasto.R` | `enighur_ingreso_gasto.md` |
| ENEMDU marzo 2026 ingreso laboral por sector | `enemdu/enemdu_persona_2026_03.sav` | Usado en `clean_enemdu_ingreso_sector_laboral.R` | `enemdu_ingreso_sector_laboral.md` |
| ENEMDU matriz de transición laboral IV 2022-IV 2023 | `enemdu/Trimestre_IV_2022_2023_tabulados_matriz.xlsx` | Usado en `clean_enemdu_transicion_laboral_desempleo_zona.R` | `enemdu_transicion_laboral_desempleo_zona.md` |
| FBref minutos de ecuatorianos en Big Five 2019/20-2025/26 | `fbref/ecuatorianos_big5_minutos_2019_2026.csv` | Usado en `clean_fbref_ecuatorianos_big5.R` | `fbref_ecuatorianos_big5.md` |
| ENSANUT 2018 padre en hogar de menores | `ensanut/1_BDD_ENS2018_f1_personas.dta.zip` | Usado en `clean_ensanut_menores_padre_hogar.R` | `ensanut_menores_padre_hogar.md` |
| ENCV LGBTI+ 2025 aceptación de orientación o identidad | `lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx` | Usado en `clean_lgbti_aceptacion_orientacion_identidad.R` | `lgbti_aceptacion_orientacion_identidad.md` |
| ENEMDU módulo de actividad física diciembre 2024 | `actividad_fisica_joan/2024_12/2_BDD_DATOS_ABIERTOS_ACTIVIDAD_FISICA_2024_12_CSV.csv` | Usado en `clean_enemdu_actividad_fisica_insuficiente.R` | `enemdu_actividad_fisica_insuficiente.md` |
| Aranceles aplicados a importaciones desde Colombia | `aranceles_colombia/COL_completo_con_arancel.xlsx`, `aranceles_colombia/lista-de-ecuador.xlsx` | Usado en `clean_aranceles_colombia.R` | `aranceles_colombia.md` |

## Cobertura de documentación

- Cada dataset usado por un script debe tener una ficha en este directorio.
- Si se agrega un archivo nuevo bajo `data/raw/`, debe registrarse aquí aunque todavía no tenga gráfico asociado.
