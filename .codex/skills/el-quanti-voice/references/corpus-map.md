# Mapa del corpus editorial

## Principio de ponderación

No tratar todos los artículos como observaciones equivalentes. Aplicar esta prioridad cualitativa:

| Nivel | Peso orientativo | Uso |
|---|---:|---|
| Ancla contemporánea | 5 | Resolver voz, estructura y estándar actual |
| Quantificadores | 3 | Ampliar temas, recursos narrativos y tradición editorial |
| Trainees | 1 | Observar formatos o temas no cubiertos, sin desplazar la voz central |

El peso no es una fórmula estadística. Es una regla para decidir qué patrón prevalece cuando los textos difieren.

## Ancla contemporánea

Leer siempre:

- `content/post/economia/2026-06-01-enighur-ingresos-gastos-ecuador/index.Rmd`
- Autor: Daniel Sánchez
- Grupo: `quantificador`
- Fecha editorial: 5 de junio de 2026
- Peso: 5

Tomar de este artículo:

- encabezados formulados como hallazgos;
- explicación temprana de la fuente y de su utilidad;
- alternancia entre prosa, gráficos y listas de resultados;
- traducción de promedios, medianas y distribuciones;
- cautela ante explicaciones alternativas;
- sección final orientada a la relevancia pública;
- nota explícita de datos y código.

No copiar literalmente sus muletillas ni su primera persona. Adaptar persona y ritmo al autor del nuevo artículo.

## Corpus central de quantificadores

### Recientes

- `content/post/economia/2025-08-19-efecto-de-la-flexibilizaci-n-laboral-en-los-costes-salariales-en-ecuador/index.Rmd`
  - José Luis Sola, `quantificador`
  - Usar para artículos econométricos, explicación de modelos y discusión de límites.
  - Dar prioridad al ancla de 2026 cuando difieran en claridad o estructura.

- `content/post/valores/2023-07-02-homosexualidad-qu-piensan-los-ecuatorianos/index.rmd`
  - Alonso Quijano y Daniel Sánchez, ambos `quantificador`
  - Usar para opinión pública, series temporales, derechos y lectura institucional.

- `content/post/valores/2023-03-07-tres-problemas-para-el-an-lisis-cuantitativo-de-los-femicidios-en-ecuador/index.Rmd`
  - Alejandra Marchán y Daniel Sánchez, ambos `quantificador`
  - Usar para auditorías de datos, transparencia, comparabilidad y rendición de cuentas.

### Fundacionales

- `content/post/economia/2020-07-04-ecuatorianos-mas-altos-ganan-mas/index.Rmd`
  - Leonel Borja Plaza, `quantificador`
  - Usar para explicar relaciones estadísticas mediante una secuencia de preguntas y gráficos.

- `content/post/socioeconomia/2020-07-12-la-raza-de-nuestros-padres/index.Rmd`
  - Marco Faytong, `quantificador`
  - Usar para voz exploratoria, primera persona y declaración honesta de límites.

- `content/post/corrupcion/2020-06-21-corrupcion-por-que-nos-interesa-estudiarla-y-que-podemos-hacer-para-eliminarla/index.Rmd`
  - Alonso Quijano, `quantificador`
  - Usar para conectar un problema coyuntural con literatura y mecanismos posibles.

Los textos fundacionales tienen personalidad valiosa, pero también erratas, convenciones antiguas y bloques técnicos que ya no representan el estándar actual. Tomar su curiosidad y capacidad explicativa. No replicar sus defectos.

## Corpus secundario de trainees

Consultar solo cuando aporte un formato o tema que no esté cubierto arriba. Ejemplos:

- `content/post/salud/2024-11-19-embarazo-infantil-en-ecuador/index.Rmd`
- `content/post/economia/2023-10-13-pesimismo-y-desconfianza/index.Rmd`
- `content/post/economia/2023-09-07-perspectivas-sobre-el-mercado-laboral/index.Rmd`
- `content/post/economia/2023-08-18-ecuador-en-la-encrucijada/index.Rmd`

No extraer reglas centrales de voz de estos textos si contradicen el ancla o el corpus de quantificadores.

## Clasificar artículos no listados

1. Leer `authors` en el frontmatter del artículo.
2. Abrir el perfil correspondiente en `content/los-quantificadores/<slug>.md`.
3. Leer el campo `group`.
4. Clasificar `group: quantificador` como corpus central.
5. Clasificar `group: trainee` como corpus secundario.
6. Si hay autoría mixta, valorar qué secciones o decisiones editoriales pueden atribuirse con seguridad. No asumir que toda la pieza representa por igual a cada grupo.

## Resolver cambios futuros

Mantener el artículo ENIGHUR de 2026 como ancla hasta que la dirección editorial cambie esta skill. Si aparece un artículo posterior, usarlo como evidencia reciente adicional, pero no reemplazar automáticamente el ancla ni la jerarquía de autores.
