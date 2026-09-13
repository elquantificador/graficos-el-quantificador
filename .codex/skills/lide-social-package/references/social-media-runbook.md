# Runbook de amplificación en redes sociales

## Propósito

Esta guía describe cómo repetir la distribución y amplificación de publicaciones de El Quantificador en X, LinkedIn y Facebook.

El agente debe usar el navegador interno de Codex, verificar cada cuenta antes de actuar y conservar abiertas las sesiones al terminar.

## Fuente de verdad

El paquete **LIDE Social** es la fuente de verdad para las publicaciones de LinkedIn y Facebook. Contiene el texto, los enlaces, los hashtags, las imágenes y las versiones específicas para cada plataforma.

El agente no debe reescribir, resumir ni inventar copy. Si falta una versión preparada, una imagen o un enlace exacto, debe detenerse y pedir instrucciones.

## Reglas de seguridad y autorización

- Las reacciones, reposts y compartidos son interacciones públicas. Requieren confirmación explícita del usuario justo antes de ejecutarse.
- La autorización para interactuar no autoriza a publicar contenido nuevo.
- No guardar ni reproducir contraseñas, códigos OTP, códigos de seguridad o datos de recuperación.
- Si aparece CAPTCHA, autenticación, código de seguridad o 2FA, el usuario debe resolverlo.
- No aceptar permisos de cámara, micrófono, ubicación o descargas sin autorización específica.
- Si la cuenta, autor, página o publicación no coincide exactamente, detenerse.
- Si aparece un editor de publicación durante un flujo de interacción, no escribir ni publicar nada.

## Matriz de acciones

| Plataforma | Fuente | Cuentas o páginas | Acciones del ejemplo confirmado |
| --- | --- | --- | --- |
| X | Hilo publicado de `@ElQuantificador` | `@ElQuantificador`, `@LaboratorioLIDE`, `@newdimension_ec` | Like a los 4 tuits; repost del primero y el cuarto |
| LinkedIn | Publicación preparada en LIDE Social | Perfil o página exactos por verificar | Reacción y repost solo si están autorizados |
| Facebook | Publicación preparada en LIDE Social | Perfil o página exactos por verificar | Reacción y compartir solo si están autorizados |

Los nombres de perfiles y páginas de LinkedIn y Facebook no deben inferirse a partir de las cuentas de X.

## Flujo para X

### Precondiciones

- El hilo ya debe estar publicado.
- Debe existir un enlace exacto a la publicación original de `@ElQuantificador`.
- Si se publicó una versión nueva, usar el enlace de esa versión. Los enlaces antiguos no deben reutilizarse.
- Debe estar disponible el cambio de cuentas entre `@ElQuantificador`, `@LaboratorioLIDE` y `@newdimension_ec`.

### Procedimiento

1. Abre la publicación original de `@ElQuantificador` y copia su enlace exacto.
2. Comprueba que el enlace abre el hilo correcto y que `@ElQuantificador` aparece como autor.
3. Cambia a `@LaboratorioLIDE` mediante “Cambiar cuentas”.
4. Abre el mismo enlace y verifica nuevamente que el autor sea `@ElQuantificador`.
5. Pulsa “Me gusta”. Si aparece “Unlike” o “Liked”, el like ya está marcado: no vuelvas a pulsarlo.
6. Pulsa “Repost” y selecciona “Repost”. No selecciones “Quote”.
7. Verifica el repost desde el perfil de LIDE, en la pestaña “Reposts”, o mediante el estado “Reposted” en la publicación.
8. Repite los pasos 3–7 con `@newdimension_ec`.
9. Repite las mismas acciones con `@ElQuantificador` cuando el alcance confirmado incluya la cuenta original.
10. Para cada cuenta, confirma que los cuatro tuits muestran “Liked” y que solo el primero y el cuarto muestran “Reposted”.
11. Deja abierta la cuenta activa y registra las acciones completadas.

### Qué no hacer en X

- No pulsar “New post”, “Post” o “Post all”.
- No pulsar “Share”.
- No elegir “Quote”.
- No cargar imágenes.
- No escribir respuestas, comentarios ni publicaciones nuevas.

Nota: X usa “Repost”; la referencia a “Instagram” en versiones anteriores de esta guía era incorrecta.

## Flujo para LinkedIn

LinkedIn no puede interactuar directamente con el hilo de X. El agente debe trabajar con la publicación de LinkedIn preparada en LIDE Social.

1. Localiza en LIDE Social la versión exacta de LinkedIn.
2. Identifica el perfil o la página autora mediante su nombre y URL.
3. Comprueba que el texto, el enlace, los hashtags y las imágenes coincidan con el paquete.
4. Si la publicación todavía no existe, detenerse. Publicar requiere una autorización independiente y explícita.
5. Si el alcance confirmado es solo de interacción, añade la reacción solicitada.
6. Usa “Repost” únicamente si está incluido en la autorización. No agregues comentarios propios.
7. Si la reacción ya está seleccionada, no vuelvas a pulsarla.
8. Verifica el resultado desde la publicación y, cuando esté disponible, desde la actividad o los reposts del perfil o página.
9. No abrir “Start a post”, no crear una copia y no adjuntar archivos fuera de lo indicado en LIDE Social.

## Flujo para Facebook

Facebook no puede interactuar directamente con el hilo de X. El agente debe trabajar con la publicación correspondiente preparada en LIDE Social.

1. Localiza en LIDE Social la versión exacta de Facebook.
2. Identifica el perfil o la página autora mediante su nombre y URL.
3. Comprueba que el texto, el enlace, los hashtags y las imágenes coincidan con el paquete.
4. Si la publicación todavía no existe, detenerse. Publicar requiere una autorización independiente y explícita.
5. Si el alcance confirmado es solo de interacción, añade la reacción solicitada.
6. Pulsa “Share” solo si compartir está incluido expresamente en el alcance.
7. Confirma el destino del share antes de ejecutarlo. No compartir hacia otra cuenta o página no indicada.
8. Si la reacción ya está activa, no vuelvas a pulsarla.
9. Verifica la reacción y el share desde el perfil o página correspondiente.
10. No abrir “Create post”, no publicar una copia y no añadir comentarios no solicitados.

## Criterios de parada

El agente debe detenerse y pedir instrucciones si:

- no encuentra el enlace exacto;
- hay varias publicaciones parecidas y no puede identificar la correcta;
- el autor o la cuenta activa no coincide;
- falta el post preparado en LIDE Social;
- la plataforma abre un editor o muestra un botón de publicación;
- no está claro si corresponde reaccionar, repostear o compartir;
- aparece CAPTCHA, OTP, 2FA o una verificación de seguridad;
- una acción produce un error o no cambia de estado después de una verificación;
- la interfaz ofrece “Quote”, “Create post” u otra acción distinta de la autorizada.

## Registro final

Al terminar, el agente debe informar por plataforma y cuenta:

- publicación o hilo utilizado;
- cuenta o página activa;
- likes o reacciones completados;
- reposts o shares completados;
- acciones que ya estaban hechas;
- acciones no completadas y motivo;
- URL que queda abierta para revisión.

La sesión debe quedar abierta, sin cerrar X, LinkedIn o Facebook y sin dejar un editor de publicación listo para enviar.
