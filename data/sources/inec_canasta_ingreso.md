# Canasta básica e ingresos comparables para un hogar tipo, Ecuador, 2018-2026

## Pieza

- `outputs/figures/43_canasta-basica-ingreso-ecuador.png`
- Limpieza: `scripts/data-cleaning/clean_inec_canasta_ingreso.R`
- Visualización: `scripts/plots/plot_inec_canasta_ingreso.R`

## Fuente y procedencia

La serie proviene de los boletines técnicos de enero del Índice de Precios al
Consumidor del Instituto Nacional de Estadística y Censos (INEC), para 2018 a
2026. Los enlaces directos a cada boletín se conservan en
`data/raw/inec_canasta_ingreso/canasta_vs_ingreso_karel.csv`.

El insumo fue entregado por Karel Lázaro González Ruíz como parte de su
participación en el concurso Ecuador Quantificado 2026. La entrega original y
su código reproducible están disponibles en:

https://github.com/karelgonzalezruiz/Concurso-Ecuador-Quantificado-2026-Participacion

## Variables y definición

El gráfico compara, para cada año:

- el costo nacional de la Canasta Familiar Básica para el hogar de referencia;
- el ingreso mensual del hogar tipo de cuatro miembros con 1,6 perceptores del
  salario básico unificado;
- la mediana del ingreso observado en ENEMDU para hogares de referencia, usando la
  variable oficial `ingpc` y expresándola como equivalente para cuatro personas;
- el salario básico unificado del año, conservado como variable de referencia.

El ingreso familiar del hogar tipo incluye las partes proporcionales de los
décimos tercero y cuarto sueldos y no considera fondos de reserva, de acuerdo
con la definición publicada por el INEC en la sección de Canastas Familiares.
El ingreso observado usa `ingpc`, que INEC define como ingreso per cápita del
hogar y emplea en la medición de pobreza por ingresos. Se calcula como mediana
ponderada por `fexp` entre hogares con cuatro miembros, dos adultos de 18 años o
más y dos hijos menores de 18 años, con un jefe y un cónyuge. Para 2026, la
mediana se calcula por trimestre y se promedia entre los periodos disponibles.
Para compararla con los montos del hogar de referencia, el resultado se
multiplica por cuatro. La fuente base sigue siendo per cápita.

## Cobertura de ENEMDU

La serie usa los archivos de diciembre de 2018 a 2025 ya conservados en
`data/raw/enemdu/`. Para 2026 usa el archivo del I trimestre, porque EcuDataMCP
no mostró un archivo anual completo de 2026. INEC publica bases anuales
consolidadas para años recientes, pero no existe una serie anual homogénea
2018-2026 en el repositorio.

Páginas oficiales consultadas mediante EcuDataMCP:

- https://www.ecuadorencifras.gob.ec/enemdu-2018/
- https://www.ecuadorencifras.gob.ec/enemdu-diciembre-2019/
- https://www.ecuadorencifras.gob.ec/empleo-dic-2020/
- https://www.ecuadorencifras.gob.ec/enemdu-anual-2021/
- https://www.ecuadorencifras.gob.ec/enemdu-anual-2022/
- https://www.ecuadorencifras.gob.ec/enemdu-anual-2023/
- https://www.ecuadorencifras.gob.ec/enemdu-anual-2024/
- https://www.ecuadorencifras.gob.ec/enemdu-anual/

## Construcción

El script de limpieza valida las columnas, incorpora `ingpc` desde ENEMDU y
calcula la cobertura del ingreso del hogar tipo sobre la canasta y la brecha en
dólares. El script de visualización conserva la escala del hogar de referencia y
adapta la comparación al lienzo vertical y al house style de El Quantificador.
