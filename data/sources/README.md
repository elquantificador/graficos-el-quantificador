# Inventario de fuentes de datos

Este directorio documenta los insumos crudos almacenados en `data/raw/` y las fichas metodologicas asociadas a cada grafico o analisis.

## Estructura esperada

- `data/raw/` contiene exclusivamente datos crudos u hojas extraidas listas para limpieza.
- `data/processed/` contiene derivados generados por los scripts de `scripts/data-cleaning/`.
- `data/sources/` contiene fichas descriptivas, notas metodologicas e inventario.

## Inventario actual de datasets crudos

| Tema | Ruta en `data/raw/` | Estado | Fuente |
|---|---|---|---|
| ENEMDU mensual enero 2026 | `enemdu/enemdu_persona_2026_01.sav` | Usado en `clean_escolaridad_ingreso.R` | `escolaridad_ingreso.md` |
| ENEMDU horas promedio por sector 2018-2026 | `enemdu/ENEMDU_PERSONAS_2018_12_hom.sav`, `enemdu/enemdu_persona_201912.sav`, `enemdu/enemdu_persona_2020_12.sav`, `enemdu/enemdu_persona_2021_12.sav`, `enemdu/enemdu_persona_2022_12.sav`, `enemdu/enemdu_persona_2023_12.sav`, `enemdu/enemdu_persona_2024_12.sav`, `enemdu/enemdu_persona_2025_12.sav`, `enemdu/enemdu_persona_2026_l_trimestre.sav` | Usado en `clean_enemdu_horas_sector_lineas.R` | `enemdu_horas_sector_lineas.md` |
| ENSANUT 2018 personas | `ensanut/1_BDD_ENS2018_f1_personas.dta.zip` | Usado en `clean_ecuatorianos_altos.R` | `ecuatorianos_altos.md` |
| ENEMDU trimestral 2021-2025 | `enemdu/evolucion/*.csv` | Usado en `clean_evolucion_nini.R` | `evolucion_nini.md` |
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
| Fiscalia muertes de mujeres | `fiscalia/muertes_fem_fiscalia_2026.csv` | Usado en `clean_femicidios.R` | `femicidios.md` |
| Exportaciones BCE | `exportaciones/05. Export. por Producto Principal y Pais.xlsx` | Usado en `clean_exportaciones_eeuu.R` | `exportaciones_eeuu.md` |
| EF EPI Ecuador 2025 | `ef_epi/ef_epi_ecuador_extracted.xlsx` | Usado en `clean_ef_epi_job.R` | `ef_epi_job.md` |
| WVS religion | `wvs/wvs_importance_of_religion_in_life.xls` | Usado en `clean_wvs_religion_importance.R` | `wvs_religion_importance.md` |
| WVS Ecuador actitudes hacia la homosexualidad | `wvs/WVSEcuador.dta` | Usado en `clean_wvs_homosexualidad_vecinos.R` y `clean_wvs_homosexualidad_padres.R` | `wvs_homosexualidad.md` |
| ENIGHUR tabulados de ingresos, gastos y promedios | `enighur/cuadro_2_1_1_ingresos.rds`, `cuadro_2_1_3_gastos.rds`, `cuadro_2_2_1_promedios.rds`, `mapeo_categorias_gasto.rds` | Usado en `clean_enighur_ingreso_gasto.R` | `enighur_ingreso_gasto.md` |
| ENIGHUR gasolina y transporte por quintil | `enighur/enighur_gasolina_transporte_publico_quintiles_2025.csv` | Usado en `clean_enighur_gasolina_transporte_quintiles.R` | `enighur_gasolina_quintiles.md` |
| ENIGHUR share de gasolina por quintil y encuesta | `enighur/enighur_gasolina_share_quintiles_2012_2025.csv` | Usado en `clean_enighur_gasolina_share_quintiles_years.R` | `enighur_gasolina_quintiles.md` |
| ENEMDU marzo 2026 ingreso laboral por sector | `enemdu/enemdu_persona_2026_03.sav` | Usado en `clean_enemdu_ingreso_sector_laboral.R` | `enemdu_ingreso_sector_laboral.md` |
| ENEMDU matriz de transicion laboral IV 2022-IV 2023 | `enemdu/Trimestre_IV_2022_2023_tabulados_matriz.xlsx` | Usado en `clean_enemdu_transicion_laboral_desempleo_zona.R` | `enemdu_transicion_laboral_desempleo_zona.md` |
| FBref minutos de ecuatorianos en Big Five 2019/20-2025/26 | `fbref/ecuatorianos_big5_minutos_2019_2026.csv` | Usado en `clean_fbref_ecuatorianos_big5.R` | `fbref_ecuatorianos_big5.md` |
| ENSANUT 2018 padre en hogar de menores | `ensanut/1_BDD_ENS2018_f1_personas.dta.zip` | Usado en `clean_ensanut_menores_padre_hogar.R` | `ensanut_menores_padre_hogar.md` |
| ENCV LGBTI+ 2025 aceptacion de orientacion o identidad | `lgbti/7. Base_datos_ENCV_LGBTI+_2025_tratada_fexp_VF_V3.xlsx` | Usado en `clean_lgbti_aceptacion_orientacion_identidad.R` | `lgbti_aceptacion_orientacion_identidad.md` |
| ENEMDU modulo de actividad fisica diciembre 2024 | `actividad_fisica_joan/2024_12/2_BDD_DATOS_ABIERTOS_ACTIVIDAD_FISICA_2024_12_CSV.csv` | Usado en `clean_enemdu_actividad_fisica_insuficiente.R` | `enemdu_actividad_fisica_insuficiente.md` |
| Aranceles aplicados a importaciones desde Colombia | `aranceles_colombia/COL_completo_con_arancel.xlsx`, `aranceles_colombia/lista-de-ecuador.xlsx` | Usado en `clean_aranceles_colombia.R` | `aranceles_colombia.md` |
| Ofertas de empleo de datos y tecnología, junio-julio 2026 | `jobs_scrape/jobs_20260617_043911.csv`, `jobs_scrape/jobs_20260716_013707.csv` | Usado en `clean_jobs_habilidades_mensuales.R` | `jobs_habilidades_demandadas.md` |
| ENEMDU anual 2025, informalidad y empleo no remunerado por provincia | `enemdu/enemdu_anual_2025_provincial.csv` | Usado en `clean_enemdu_informalidad_provincial.R` | `enemdu_informalidad_provincial.md` |
| ENEMDU anual 2025, razones para no estudiar ni trabajar entre jóvenes NINI | `enemdu/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip` | Usado en `clean_enemdu_nini_razones_sexo.R` | `enemdu_nini_razones_sexo.md` |
| Muertes violentas por hora, 2014–mayo 2026 | `homicidios/mdi_homicidiosintencionales_pm_2014_2026.xlsx` | Usado en `clean_homicidios_hora.R` | `homicidios_hora.md` |
| Muertes intencionales, desapariciones y presupuesto policial, 2017–2025 | `crimen/mdi_homicidiosintencionales_pm_2014_2026.xlsx`, `mdi_personasdesaparecidas_pm_2017_2026.xlsx`, `Gasto_proforma.xlsx` | Usado en `clean_crimen_desapariciones.R` | `crimen_desapariciones.md` |
| Desapariciones y fallecimientos, 2017–2025 | `desapariciones/mdi_personasdesaparecidas_pm_2017_2025.xlsx` | Usado en `clean_desaparecidos_fatalidad.R` | `desaparecidos_fatalidad.md` |
| Remesas recibidas, PIB nominal y país de origen, Ecuador, Colombia y Perú, 2000–2025 | `remesas_regional/world_bank_remesas_wdi.csv`, `remesas_regional/world_bank_ecuador_gdp_wdi.csv`, `remesas_regional/bce_eren2025anual.pdf`, `remesas_regional/bce_RemesasIntegradoWEB_PUB.xlsx` | Usado en `clean_remesas_regional.R` | `remesas_regional.md` |
| IPC por ciudad, enero de 2021–junio de 2026 | `ipc_inec_2026_06/` | Usado en `clean_ipc_ciudades_leonor.R` | `ipc_ciudades_leonor.md` |
| ENEMDU anual 2025, empleo adecuado y desempleo juvenil por sexo y provincia | `enemdu/2_BDD_DATOS_ABIERTOS_ENEMDU_2025_CSV.zip` | Usado en `clean_enemdu_juventud_empleo_2025.R` | `enemdu_juventud_empleo_2025.md` |
| Canasta básica e ingreso familiar, enero 2018–2026 | `inec_canasta_ingreso/canasta_vs_ingreso_karel.csv` | Usado en `clean_inec_canasta_ingreso.R` | `inec_canasta_ingreso.md` |
| ENEMDU, ingreso per cápita observado para la serie de canasta | `enemdu/ENEMDU_PERSONAS_2018_12_hom.sav`, `enemdu/enemdu_persona_201912.sav`, `enemdu/enemdu_persona_2020_12.sav`, `enemdu/enemdu_persona_2021_12.sav`, `enemdu/enemdu_persona_2022_12.sav`, `enemdu/enemdu_persona_2023_12.sav`, `enemdu/enemdu_persona_2024_12.sav`, `enemdu/enemdu_persona_2025_12.sav`, `enemdu/enemdu_persona_2026_l_trimestre.sav` | Usado en `clean_inec_canasta_ingreso.R` | `inec_canasta_ingreso.md` |
| ACLED conflicto en Ecuador, 2018-2025 | `acled_conflicto_ecuador/acled_ecuador_maestro_20260903.csv` | Usado en `clean_acled_conflicto_ecuador.R`; disponible previa solicitud por sensibilidad | `acled_conflicto_ecuador.md` |
| Marco Geoestadístico 2022 del INEC | `inec_geoestadistico_2022/shapefile/` | Usado en `plot_acled_conflicto_ecuador.R` | `inec_geoestadistico_2022.md` |

## Cobertura de documentacion

- Cada dataset usado por un script debe tener una ficha en este directorio.
- Si se agrega un archivo nuevo bajo `data/raw/`, debe registrarse aqui aunque todavia no tenga grafico asociado.
