---
name: el-quanti-voice
description: Escribir, reescribir y revisar artículos de El Quantificador con su voz editorial basada en datos sobre Ecuador. Usar al crear borradores, pulir títulos, entradillas, secciones, explicaciones de gráficos, conclusiones y metadatos, o al auditar rigor, claridad, estructura, tuteo y consistencia de artículos en Markdown, R Markdown o HTML generado. Dar autoridad principal al artículo ENIGHUR de 2026 y más peso a autores del grupo quantificador que a trainees.
---

# Voz de El Quanti

## Preparar el contexto

1. Leer `references/voice-guide.md` completo.
2. Leer `references/corpus-map.md` para conocer la jerarquía de fuentes.
3. Abrir siempre el artículo ancla indicado en el mapa del corpus.
4. Abrir uno o dos textos centrales escritos por quantificadores que se parezcan al tema o formato solicitado.
5. Consultar textos de trainees solo para resolver una necesidad concreta que el corpus central no cubra. No dejar que su estilo prevalezca.
6. Leer `references/review-rubric.md` cuando la tarea incluya revisar, editar, aprobar o comprobar un artículo.

No deducir la voz a partir de un promedio indiferenciado del archivo. Conservar la jerarquía de fuentes.

## Elegir el flujo

### Escribir un artículo nuevo

1. Confirmar o inferir de forma segura:
   - pregunta central;
   - audiencia;
   - hallazgo principal;
   - datos, periodo y población;
   - método y límites;
   - gráficos disponibles;
   - autoría, categoría y edición.
2. No inventar cifras, fuentes, resultados, código ni detalles metodológicos. Marcar vacíos con indicaciones concretas entre corchetes.
3. Proponer un esquema de hallazgos antes de redactar si el encargo todavía no trae una estructura.
4. Redactar con encabezados que comuniquen hallazgos, no temas genéricos.
5. Integrar cada gráfico mediante contexto, lectura y consecuencia. No describir colores o formas sin explicar qué significan.
6. Cerrar con implicaciones proporcionadas a la evidencia, límites y una nota de reproducibilidad.
7. Aplicar la lista de control de `references/review-rubric.md` antes de entregar.

### Revisar un borrador

1. Preservar el argumento, los resultados y la voz individual del autor cuando sean compatibles con El Quantificador.
2. Separar correcciones necesarias de mejoras opcionales.
3. Comprobar primero rigor y trazabilidad; después estructura, voz y estilo.
4. Ejecutar `scripts/check_article.ps1 -Path <ruta>` para detectar problemas mecánicos. Tratar sus resultados como señales, no como sustituto de la revisión editorial.
5. Verificar cada afirmación fuerte contra el texto, tabla, gráfico o fuente que supuestamente la respalda.
6. Corregir directamente cuando el usuario haya pedido editar. Si solo pidió revisar, informar hallazgos con citas de línea y cambios propuestos.
7. No reescribir todo por preferencia personal. Mantener giros humanos útiles si son claros, precisos y compatibles con la guía.

### Pulir una sección o fragmento

1. Identificar la función del fragmento: planteamiento, método, resultado, interpretación, transición o cierre.
2. Resolver primero ambigüedades de sujeto, periodo, denominador y alcance.
3. Reducir jerga y repeticiones sin borrar matices.
4. Devolver el fragmento listo para insertar y explicar brevemente cualquier cambio sustantivo.

## Mantener los no negociables

- Dirigirse al lector con tuteo. No usar `usted` ni imperativos formales.
- Usar sentence case en títulos y encabezados.
- No usar raya larga.
- Evitar fórmulas de contraste propias de texto automático, como `no es X, es Y`, `no solo..., sino...` y `no se trata de..., sino de...`.
- No confundir correlación, asociación y causalidad.
- Identificar población, periodo, unidad, fuente y denominador de las cifras importantes.
- Separar lo observado de las hipótesis o interpretaciones.
- Explicar términos técnicos en lenguaje común cuando aparezcan por primera vez.
- Reconocer límites relevantes sin convertirlos en una defensa interminable.
- Mantener la reproducibilidad mediante fuentes, notas, bibliografía y acceso al código cuando exista.
- No reproducir erratas, mayúsculas excesivas, rutas locales, dependencias antiguas ni defectos técnicos de artículos históricos.

## Entregar resultados útiles

Para una revisión, presentar en este orden:

1. veredicto editorial breve;
2. problemas críticos que impiden publicar;
3. mejoras importantes;
4. detalles menores;
5. texto corregido o cambios aplicados, según el encargo.

Para un artículo nuevo, entregar un borrador coherente y publicable, acompañado solo por una lista corta de datos o decisiones pendientes. No llenar el texto de comentarios de proceso.

## Recursos

- `references/voice-guide.md`: rasgos de voz, estructura y recursos retóricos.
- `references/corpus-map.md`: fuentes, pesos y rutas del corpus.
- `references/review-rubric.md`: auditoría editorial, cuantitativa y técnica.
- `scripts/check_article.ps1`: comprobaciones mecánicas de frontmatter y estilo.
