# Fuente de datos: Salario promedio por industrias (REESS)

## Dataset
**Registro Estadístico de Empleo en la Seguridad Social (REESS)**
Corte mensual — febrero 2026

## Proveedor
Instituto Nacional de Estadística y Censos (INEC), Ecuador

## Acceso
Archivo utilizado: `data/raw/reess/Indicadores Laborales_Empleo_02_2026.xlsx`

Hojas utilizadas: `4_2_1` y `1_1_1`

Descripción del tabulado:
`SALARIO PROMEDIO DEL EMPLEO REGISTRADO - CARACTERÍSTICAS OCUPACIONALES Y DEL EMPLEADOR - POR RAMA DE ACTIVIDAD ECONÓMICA`

`EMPLEO REGISTRADO - CARACTERÍSTICAS OCUPACIONALES Y DEL EMPLEADOR - POR RAMA DE ACTIVIDAD ECONÓMICA`

## Variables utilizadas

| Variable | Descripción |
|---|---|
| `codigo` | Código agregado de rama CIIU Rev. 4.1 nivel 1 |
| `industria` | Descripción de la rama de actividad |
| `feb.-25` | Salario promedio del empleo registrado en febrero de 2025 |
| `feb.-26` | Salario promedio del empleo registrado en febrero de 2026 |
| `feb.-25` (`1_1_1`) | Empleo registrado en febrero de 2025 |
| `feb.-26` (`1_1_1`) | Empleo registrado en febrero de 2026 |

## Notas
- El gráfico usa la desagregación CIIU Rev. 4.1 nivel 1 (`4_2_1`).
- El color usa también el total de empleo registrado por industria desde `1_1_1`.
- Se excluyen `No clasificado`, `Doméstico`, `Campesino`, `Semicontribuyente` y `Voluntario`.
- El ranking se ordena por el salario promedio observado en febrero de 2026.
- Los datos de febrero de 2026 son preliminares y están sujetos a revisión en la siguiente publicación.
