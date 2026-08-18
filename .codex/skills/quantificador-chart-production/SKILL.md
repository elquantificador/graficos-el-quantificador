---
name: quantificador-chart-production
description: Produce reproducible El Quantificador charts from data, existing R code, or a visual reference inside the graficos-el-quantificador repository, including data cleaning, house-style rendering, catalog registration, and QA.
metadata:
  short-description: Produce El Quantificador charts reproducibly
---

# Producción de gráficos de El Quantificador

Usa esta skill únicamente cuando el trabajo se haga dentro de
`graficos-el-quantificador` y el resultado esperado sea un gráfico reproducible
para El Quantificador. La salida normal es un PNG publicado o en borrador,
acompañado por código y documentación suficiente para volver a generarlo.

## Proceso normal

Sigue este flujo, adaptándolo a los insumos disponibles:

1. **Definir la pieza.** Identifica la pregunta que responde el gráfico, la
   población y el periodo, la unidad de medida, la fuente, el estado esperado
   (`draft` o `published`) y el nombre de la persona o entidad autora. Confirma
   qué aporta cada insumo: datos, código, gráfico de referencia, copy editorial
   o metadatos de publicación.
2. **Inspeccionar el repositorio.** Desde la raíz, lee `AGENTS.md`,
   `HOUSE_STYLE.md`, `scripts/utils.R`, `scripts/packages.R`, las plantillas y
   los scripts o fichas de fuente más cercanos. Revisa también el catálogo y
   los gráficos relacionados para conservar nombres, colores y convenciones.
3. **Resolver la procedencia de los datos.** Ubica el insumo crudo en
   `data/raw/[fuente]/` o registra con precisión por qué el usuario entregó un
   archivo externo. Lee la ficha correspondiente en `data/sources/`. Verifica
   variables, unidades, cobertura temporal, categorías, valores faltantes,
   duplicados, ponderadores y transformaciones necesarias. No inventes datos,
   categorías, fuentes ni resultados cuando falte información esencial.
4. **Separar limpieza y visualización.** Crea o adapta un script
   `scripts/data-cleaning/clean_[fuente]_[tema].R` que transforme los datos
   crudos y guarde un único derivado reproducible en `data/processed/`. Luego
   crea o adapta `scripts/plots/plot_[fuente]_[tema].R`, que lea ese derivado y
   produzca el PNG en `outputs/figures/`. Conserva la semántica del código
   existente y haz el cambio mínimo que permita reproducibilidad y claridad.
5. **Aplicar el house style.** Usa las funciones de `scripts/utils.R`, el
   lienzo vertical estándar, los envoltorios de texto, el tema compartido y el
   logo. No introduzcas excepciones ópticas o tipográficas por iniciativa
   propia.
6. **Renderizar y revisar.** Ejecuta los scripts desde la raíz del repositorio,
   inspecciona el PNG producido y comprueba que no haya cortes, solapamientos,
   etiquetas ilegibles, leyendas ambiguas, escalas engañosas, logo superpuesto
   al contenido o caption fuera del lienzo. Itera el código si la revisión
   visual o numérica encuentra un problema.
7. **Registrar y documentar.** Añade o actualiza la fila correspondiente en
   `outputs/chart_catalog/chart_catalog.csv`, mantén estable el `Chart ID`,
   actualiza `README.md` y `data/sources/README.md` cuando sea una pieza nueva,
   y agrega notas metodológicas o editoriales breves cuando hagan falta.
8. **Validar y entregar.** Ejecuta la validación del catálogo, verifica que el
   PNG, los scripts y los enlaces existan, revisa `git diff` y `git status`, y
   entrega las rutas y los comandos de reproducción. No hagas commit, push ni
   publiques en LinkedIn salvo que el usuario lo pida explícitamente.

## Cómo interpretar los insumos

### Datos sin código

- Inspecciona primero el formato, los nombres de variables y la documentación
  de la fuente.
- Usa `scripts/templates/clean_template.R` y
  `scripts/templates/plot_template.R` como punto de partida.
- Si el archivo no puede interpretarse de forma segura o no existe la
  documentación mínima, detén la producción y pide la aclaración concreta que
  falta.

### Código existente

- Lee el código completo antes de modificarlo y rastrea su entrada, salida y
  transformaciones.
- Conserva la lógica estadística y los nombres de resultados cuando sean parte
  de una serie existente; corrige solo lo necesario para cumplir el flujo del
  repositorio, el house style o la solicitud del usuario.
- No copies patrones antiguos que contradigan estas instrucciones. Algunos
  scripts históricos no usan todavía `ensure_packages()`, los helpers de
  envoltura o el caption estándar; los archivos nuevos y las modificaciones
  sustantivas deben seguir las reglas actuales.

### Gráfico ya producido

- Trátalo como referencia visual o editorial, no como fuente de datos.
- Reproduce sus resultados solo si los datos y la lógica que los generan están
  disponibles o el usuario confirma que el gráfico es la fuente autorizada.
- Identifica qué debe conservarse: composición, comparación, orden, texto,
  colores o etiquetas. Después adapta la pieza al formato y las reglas actuales
  de El Quantificador.
- Si solo existe una imagen y no hay datos o código, entrega una evaluación o
  una adaptación limitada, pero no afirmes que el gráfico es reproducible.

## Reglas del repositorio

### Estructura y nombres

- Trabaja desde la raíz. Usa únicamente rutas relativas en los scripts.
- Guarda datos crudos versionados en `data/raw/[fuente]/` y derivados en
  `data/processed/`. No agregues archivos de `data/processed/` al commit.
- Usa un par de scripts centralizados: `clean_*.R` para datos y `plot_*.R`
  para la figura. No crees carpetas autónomas por gráfico.
- Usa nombres de salida `NN_slug-ecuador.png`, con el prefijo alineado al
  `Chart ID` secuencial del catálogo. La salida de publicación es PNG, no SVG.
- Cada script nuevo debe tener el bloque de encabezado con descripción,
  `Requiere`, `Guarda` y el comando ejecutable desde la raíz.

### Limpieza de datos

Un `clean_*.R` nuevo o sustancialmente modificado debe:

- cargar paquetes mediante `source("scripts/packages.R")` y
  `ensure_packages(...)`, nunca mediante `install.packages()` o `library()`
  suelto en el nivel superior;
- leer desde `data/raw/`;
- hacer explícitas las recodificaciones, filtros, ordenamientos, unidades y
  ponderaciones;
- guardar un único `.rds` en `data/processed/`;
- terminar con `message("Guardado: ", out_path)`.

Conserva en el `.rds` metadatos de origen cuando sean útiles para la
reproducción, por ejemplo una lista con `summary` y `source`, pero no uses el
derivado como sustituto de la documentación de la fuente.

### Render del gráfico

Un `plot_*.R` nuevo o sustancialmente modificado debe:

- cargar `scripts/utils.R` y `scripts/packages.R`;
- usar `ensure_packages(...)` para las dependencias;
- leer desde `data/processed/`;
- construir la figura con `ggplot2` y aplicar `theme_quantificador()` o
  `theme_women()` cuando corresponda a una serie de género o mujeres;
- usar `wrap_title_house()`, `wrap_subtitle_house()` y
  `wrap_caption_house()` en lugar de anchos arbitrarios;
- aplicar `house_apply_logo()` o `add_logo()` con el logo, posición y tamaño
  predeterminados. Solo modifica `y` si el contenido realmente lo exige y el
  usuario lo autoriza;
- guardar con `ragg::agg_png`, `4 × 5` pulgadas y `300 dpi`, normalmente usando
  `house_spec("portrait")`;
- terminar con un mensaje que indique la ruta guardada.

El caption debe pasar como un único texto completo por
`wrap_caption_house()` y seguir este orden:

1. `Fuente:`
2. `Elaboración:`
3. `Nota:`

No separes el caption en varios llamados de wrap ni uses saltos manuales salvo
que el usuario los solicite expresamente. Mantén títulos, subtítulos, etiquetas
y caption coherentes con los datos realmente representados. Usa los
formateadores compartidos, como `label_number_intl()`,
`label_percent_intl()` y `label_dollar_intl()`, para números en español.

### House style visual

La especificación vigente está en `HOUSE_STYLE.md` y tiene prioridad sobre
ejemplos particulares. Como mínimo, verifica:

- título de 12.5 pt en negrita, subtítulo de 9 pt y caption de 6.5 pt;
- etiquetas por defecto de tamaño 3;
- texto principal `grey20`, texto secundario `grey30` y líneas de eje `grey60`;
- márgenes `margin(6, 36, 6, 16)`;
- título, subtítulo y caption alineados a la izquierda;
- anchos de wrap de casa, equivalentes a 38 para título, 60 para subtítulo y
  83 para caption;
- logo de El Quantificador sin deformación ni colisión con el caption.

No cambies tipografías, márgenes, anchos de wrap, orientación, tamaño del
canvas, paleta de una serie o posición/tamaño del logo para resolver una
incomodidad local sin autorización. Si una excepción es indispensable, explica
el motivo y pide decisión del usuario antes de consolidarla.

## Catálogo y documentación

`outputs/chart_catalog/chart_catalog.csv` es el único catálogo manual que
consume el sitio. Usa exactamente sus columnas actuales:

`Chart Name`, `Subtitle`, `Date`, `LinkedIn Link`, `Image Filename`,
`Image Path`, `Author`, `Description`, `Script Link`, `Chart ID`, `Status`,
`Series`, `Notes`.

Al registrar una pieza:

- conserva el `Chart ID` existente cuando edites una pieza;
- usa un `Status` permitido: `published`, `draft`, `supplementary`, `archived`
  o `hold`;
- haz que `Image Filename` coincida exactamente con el PNG y que `Image Path`
  sea `outputs/figures/<Image Filename>`;
- usa un nombre visible simple en `Author`, sin URL ni slug;
- escribe `Description` como texto de publicación, sin sintaxis Markdown de
  imágenes o enlaces;
- no redactes ni completes el post de LinkedIn o `Description` como parte de
  esta skill. Trátalos como insumos o metadatos editoriales que deben venir
  proporcionados por el usuario o por el flujo de publicación;
- si el copy va a sincronizarse con el sitio `quantificador`, aplica también
  sus reglas editoriales: tuteo, sentence case, cero em dashes y ausencia de
  fórmulas de contraste artificiales;
- copia del script los campos mecánicos cuando sea posible y completa de forma
  editorial la fecha, el enlace de LinkedIn, la descripción y las notas;
- no marques una pieza como `published` si faltan fecha, enlace de LinkedIn,
  imagen publicada o `Description` completo;
- considera que una fila `draft` puede tener `LinkedIn Link` vacío mientras el
  post no se haya publicado, pero debe tener el texto listo. Esa fila es una
  entrega parcial, no una entrada de catálogo completa;
- si faltan `LinkedIn Link` o `Description`, reporta que la entrada del catálogo
  está incompleta y no inventes ni generes esos campos;
- cuando el post ya exista, registra su URL real en `LinkedIn Link`. Nunca
  inventes, infieras ni sustituyas ese enlace por una URL genérica;
- mantén `Script Link` apuntando al archivo real en el repositorio principal.

Para una pieza nueva, actualiza también el listado de gráficos en
`README.md` y el inventario de `data/sources/README.md` cuando el cambio
corresponda. No conviertas el catálogo en un paso generado automáticamente ni
reintroduzcas un catálogo paralelo.

## Verificación obligatoria

Ejecuta, desde la raíz, los comandos que correspondan:

```powershell
Rscript scripts/data-cleaning/clean_fuente_tema.R
Rscript scripts/plots/plot_fuente_tema.R
python scripts/validate_chart_catalog.py
git diff --check
git status --short
```

Si el script de limpieza no es necesario porque el usuario entregó un derivado
ya documentado, verifica que exista una ruta reproducible y deja constancia de
la excepción. Si el script de render admite una ruta de salida temporal, úsala
para probar variantes sin sobrescribir una pieza publicada hasta aprobar el
resultado.

Antes de entregar, comprueba:

- el PNG existe, abre y tiene el tamaño esperado;
- los valores visibles coinciden con los datos procesados;
- no hay clipping, etiquetas cortadas, leyendas duplicadas, logo ilegible ni
  caption superpuesto;
- la fuente, la elaboración y las notas son verificables;
- el catálogo pasa `validate_chart_catalog.py`;
- no se incluyen derivados de `data/processed/`, archivos temporales ni
  cambios ajenos.

Si no puedes ejecutar R, abrir el PNG o validar el catálogo por una limitación
del entorno, dilo claramente y separa las comprobaciones realizadas de las que
quedan pendientes. No presentes el gráfico como verificado cuando solo fue
revisado en el código.

## Criterio de entrega

Considera terminado el trabajo solo cuando la pieza tenga, según corresponda:

- datos crudos o insumos identificados y documentados;
- script de limpieza reproducible o una excepción explícita y justificada;
- script de render reproducible;
- PNG en `outputs/figures/`;
- copy y caption coherentes con la evidencia;
- fila válida en el catálogo;
- documentación actualizada cuando sea una pieza nueva;
- revisión visual y validación técnica reportadas.

No hagas commit, push, publicación en el sitio ni sincronización con el repo
`quantificador` a menos que el usuario lo solicite además de producir el
gráfico.
