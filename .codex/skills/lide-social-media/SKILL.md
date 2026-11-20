---
name: lide-social-media
description: Todo el trabajo de redes sociales de El Quantificador y LIDE en LinkedIn, X, Instagram y Facebook. Redactar textos a partir de gráficos, análisis o convocatorias; hacer artes y carruseles; programar, publicar y corregir posts; y dar likes y reposts desde las otras cuentas. Valida X (280 caracteres ponderados) e Instagram (sin enlaces, máximo 5 hashtags).
---

# Redes sociales de El Quantificador y LIDE

Una sola skill para todo el flujo: texto, arte, publicación, corrección y
amplificación.

Responde en español, con tuteo y sin rayas largas (em dashes). Daniel suele
delegar todo ("ya sabes todo tú"). Decide los detalles, haz el trabajo y
reporta al final.

Archivos de apoyo:

- `references/platform-spec.md`: reglas de texto por red. Léelo antes de
  redactar.
- `references/social-media-runbook.md`: paso a paso de publicación y
  amplificación, y criterios para detenerse.
- `scripts/validate_x_posts.py` y `scripts/validate_instagram_caption.py`:
  validan los textos de X e Instagram.

## 1. Cuentas

| Red | Cuenta que publica | Otras cuentas del ecosistema |
|---|---|---|
| Instagram | elquantificador | laboratoriolide, newdimensions_ec. Están en el mismo inicio de sesión. Cambia con Más > Switch accounts. |
| Facebook | Página El Quantificador (id 101766768141723) | CM Lide (perfil que administra las páginas), página LIDE ("Laboratorio de Investigación para el Desarrollo del Ecuador"), New Dimensions, Bridges to the Future |
| LinkedIn | Página El Quantificador (company 65035345) | LIDE Community Management (perfil), Ecuadorian Development Research Lab (LIDE) (página), New Dimensions, Bridges to the Future |
| X | @ElQuantificador | @LaboratorioLIDE, @newdimension_ec. Se cambia con "Cambiar cuentas". |

El Quantificador también tiene Threads (@elquantificador) y Bluesky
(elquantificador.bsky.social). Úsalos solo si Daniel los pide.

Otros datos fijos:

- Meta business_id: 1285696443612615.
- Correo de becas: capacitaciones@laboratoriolide.org.
- Página de cursos: elquantificador.org/talleres.
- Donaciones: Givebutter. El enlace está en la página de cada curso en el
  repo `adminlide/quantificador`.

## 2. Texto

Sigue `references/platform-spec.md`. Lo esencial:

- Di qué significa el gráfico, no todo lo que contiene. Abre con el hallazgo.
- Para cada patrón empírico, busca la explicación con mejor respaldo y dila
  directamente. No uses frases como "los datos no permiten" en lugar de una
  explicación. Si no hay fuente, pídela o limítate al hallazgo verificado.
- Nunca inventes cifras, fuentes, autores ni fechas. Conserva unidades,
  periodos y grupos de comparación.
- Da crédito al autor del análisis: hallazgo, análisis, autor, El
  Quantificador / LIDE. LIDE no es el protagonista de un post de datos.
- Un texto distinto por red. No copies el de una red en otra.
- Si Daniel entrega un texto final, solo corrige ortografía y lectura. No
  agregues matices ni advertencias.
- Promociones (New Dimensions, cursos) van separadas del hallazgo.
- Español natural de Ecuador. Sin "Desde LIDE queremos compartir", sin
  lenguaje de ONG ni de marketing, pocos o ningún emoji.

Reglas por red:

| Red | Largo | Enlaces | Hashtags |
|---|---|---|---|
| LinkedIn | 80 a 180 palabras | Sí, enlace completo | 0 a 3 |
| X | 280 caracteres ponderados por post, un tema por post en hilos | Sí (cuenta como 23) | 0 a 2 |
| Instagram | 50 a 120 palabras, un solo texto aunque sea carrusel | Nunca. Usa "link en la bio" | 3 a 5, máximo 5, al final |
| Facebook | 70 a 160 palabras | Sí, el enlace directo va aquí | 0 a 3 |

Valida antes de publicar:

```
python scripts/validate_x_posts.py posts.json
python scripts/validate_instagram_caption.py caption.txt
```

El validador de X necesita el paquete de Python `regex` (`pip install regex`).
`posts.json` es una lista de textos. `caption.txt` es el texto de Instagram en
UTF-8. Corrige y vuelve a validar hasta que pasen.

### Texto base para cursos (Facebook)

```
¿<Gancho en forma de pregunta>? Abrimos una nueva edición del curso gratuito de El Quantificador y LIDE.

Del <fechas>, de 19:30 a 20:30 (hora de Quito, Lima y Bogotá). Son <n> clases en vivo por YouTube para <objetivo>.

Si está dentro de tus posibilidades, apoya esta iniciativa con una donación voluntaria desde $5 y accede a las grabaciones, a las clases por Teams y al certificado. Si no puedes donar, pide una beca escribiendo a capacitaciones@laboratoriolide.org.

Inscríbete gratis: <enlace de Luma>
Más información: elquantificador.org/talleres

#AnalíticaDeDatos #CursoGratuito #Ecuador #ElQuantificador #LaboratorioLIDE
```

- Instagram: quita los dos enlaces y pon "Inscríbete con el link en la bio."
- LinkedIn: menciona a @LIDE, usa el enlace completo
  https://elquantificador.org/talleres y quita los hashtags.
- X: versión corta con @LaboratorioLIDE y el enlace de Luma.
- Menciona siempre la donación desde $5. Alonso lo pidió para que nadie
  piense que tiene que pagar mucho.

## 3. Artes

- Hazlos en Claude Design (artifact de tipo Design), no en Canva. Daniel
  encontró que los de Canva se ven "muy AI".
- Carrusel de 3 artes en 1080 × 1350.
- Colores de El Quantificador: azul #006b8f (oscuro #00566f), rojo de
  categoría #a8363d. Colores del logo: cian #00a1cb, rojo #d54d54, naranja
  #ef9f4e, pizarra #647a8f. Letra Open Sans.
- Fondo blanco con letras azules. Mayúscula solo al inicio de la frase. Sin
  emoji.
- El nombre del curso va grande, en una sola línea si cabe. Horarios, fechas
  y "clases en vivo" también van grandes. Los logos de El Quantificador y LIDE
  van grandes. La regla de Alonso: todo lo importante se tiene que leer en un
  teléfono.
- En un calendario pon el mes bajo cada número ("Dic"), no "Clase 1".
- Para gráficos de datos, sigue `HOUSE_STYLE.md` y `AGENTS.md` del repo
  `graficos-el-quantificador`.
- Exporta cada arte a JPG 1080 × 1350 con Playwright desde el HTML del board.
  Cambia las rutas `/_blob/` por rutas locales. Une los JPG en un PDF con PIL
  para LinkedIn.
- Reels: video vertical de 1080 × 1920. Si el diseño es 1080 × 1350, ponlo
  dentro de un lienzo 1080 × 1920 con margen de marca. Deja texto y logos lejos
  de los bordes y de los controles de abajo.
- Si Daniel quiere las imágenes en el iPhone, mándale los JPG con
  SendUserFile.

## 4. Publicar

- Publica siempre desde El Quantificador, nunca desde LIDE.
- Instagram: con laboratoriolide como colaborador. Después entra como
  laboratoriolide y acepta la invitación. Aparece como aviso "Review" sobre el
  post o en Notificaciones. Desactiva la publicación simultánea en Threads.
- Facebook: solo El Quantificador, sin colaborador. En Meta Business Suite
  activa "Customize post for Facebook and Instagram" para que Facebook lleve
  los enlaces e Instagram no.
- LinkedIn: carrusel como PDF (documento). Una página no puede hacer una
  colaboración formal con documento. Menciona a LIDE con @ en el texto
  (autocompletar "Ecuadorian Development Research Lab (LIDE)"). No uses el
  editor colaborativo del feed: al adjuntar un documento cambia el autor a la
  cuenta personal.
- X: hasta 4 imágenes por post.
- Si piden publicar "mañana" sin hora, usa las 12:30, hora de Quito.
- Después de publicar, confirma el aviso de éxito y guarda el enlace de cada
  post.

## 5. Likes y reposts

Patrón de los posts anteriores. Si hay duda, abre un post viejo y mira quién
reaccionó y quién compartió.

| Red | Like y repost (o compartir) | Solo like |
|---|---|---|
| LinkedIn | LIDE Community Management, página LIDE, New Dimensions | Bridges to the Future |
| Facebook | Página LIDE, New Dimensions, CM Lide ("Share now") | El Quantificador (su propio post), Bridges to the Future |
| Instagram | newdimensions_ec | laboratoriolide (además acepta la colaboración) |
| X | @LaboratorioLIDE, @newdimension_ec | |

- En X usa "Repost", nunca "Quote".
- Si el like ya está activo ("Unlike", "Remove Like"), no vuelvas a pulsarlo.
- En Instagram, la cuenta autora a veces no retiene su propio like. Si el
  botón vuelve a "Like", no lo cuentes como hecho.
- Al terminar, deja otra vez a El Quantificador como identidad activa en cada
  red.

## 6. Navegador (Claude in Chrome)

- Hay varios Chrome conectados. El que tiene las sesiones de X, Meta,
  LinkedIn e Instagram ha sido "Browser 2". Si hay duda, pregunta cuál usar.
  No elijas solo. No hay forma de mandar un aviso de verificación al
  navegador.
- No hagas clic en "Add photo" o "Add a document" sin antes cambiar el
  comportamiento del campo de archivo. Si no, se abre el selector de archivos
  del sistema y la pestaña se congela. Ejecuta esto en la página:

  ```js
  if(!window.__claudeFilePatch){const P=HTMLInputElement.prototype;const orig=P.click;P.click=function(){if(this.type==='file'){if(!this.isConnected)document.body.appendChild(this);this.setAttribute('aria-label','claude media input');this.style.cssText='display:block!important;opacity:1!important;visibility:visible!important;position:fixed!important;top:5px;left:5px;width:220px;height:30px;z-index:2147483647';return;}return orig.call(this)};if(P.showPicker){P.showPicker=function(){}};window.__claudeFilePatch=true;}
  ```

  Luego haz clic en el botón, busca "claude media input" con `find` y sube
  los archivos con `file_upload`.
- En Meta, escribir un texto largo puede dar timeout pero igual termina.
  Revisa el texto con `innerText` del campo editable.
- LinkedIn: adjunta el documento antes de escribir. Una vista previa de
  enlace oculta la barra de archivos. Para reaccionar como otra cuenta, usa
  el botón "Open menu for switching identity" del post (búscalo por
  `data-urn`), elige la cuenta y pulsa Save.
- Facebook: cambia de perfil con el avatar de arriba a la derecha > See all
  profiles. Confirma la identidad con el texto "Comment as ..." del campo de
  comentarios.
- Si aparece CAPTCHA, Cloudflare, código QR, 2FA o código de seguridad,
  detente y pide a Daniel que lo resuelva. Nunca los resuelvas.
- Cierra las pestañas que abriste. Deja las sesiones iniciadas.

## 7. Corregir un post

1. Corrige el board en Claude Design. Exporta otra vez los JPG y el PDF.
2. LinkedIn programado: Edit post > quita el documento > Add a document >
   sube el PDF nuevo con su título > Schedule. Si piden publicar ya, en la
   lista de posts programados elige "Post now".
3. Meta programado: borra los posts de Facebook e Instagram (Posts & reels >
   Scheduled > ... > Manage post > Delete). Crea uno nuevo y pulsa Publish con
   "Set date and time" apagado.
4. Solo el texto de Instagram: abre el post > ... > Edit, cambia el texto y
   pulsa Done.
5. X publicado: no se pueden cambiar las imágenes. Borra el tweet y publica
   otro.
6. Repite los likes y reposts.

## 8. Actualizar el sitio web

El repo `adminlide/quantificador` guarda datos que dependen de los posts:

- Página de cada curso (`content/talleres/<curso>/index.md`): la lista
  `participation_social_posts` tiene los enlaces a las publicaciones oficiales
  que los participantes deben compartir. Cuando publiques una edición nueva,
  actualiza esos enlaces con una PR.
- Visualizaciones: el catálogo de `graficos-el-quantificador` necesita el
  enlace real del post de LinkedIn (`LinkedIn Link`) antes de marcar un
  gráfico como `published`.

Sigue el `AGENTS.md` de cada repo para cualquier cambio.

## 9. Cuándo detenerse

Detente y pregunta si falta el arte, el video o el enlace oficial; si no
encuentras el post exacto; si la cuenta activa no coincide; si una acción no
cambia de estado; o si la plataforma ofrece una acción distinta de la pedida
(por ejemplo "Quote" o "Create post" en un flujo de likes). La lista completa
está en `references/social-media-runbook.md`.

## 10. Reporte final

Una línea por red y cuenta: qué quedó publicado y su enlace, qué likes y
reposts se hicieron, quién tiene que aceptar algo (la colaboración de
Instagram) y qué no se pudo hacer y por qué.
