# Revisión R: `plot_enemdu_nini_razones_sexo.R`

## Resumen por severidad

- Crítica: 0
- Alta: 0
- Media: 0
- Baja: 0

Estado: listo para uso.

## Hallazgos

No se encontraron hallazgos que requieran cambios.

## Comprobaciones realizadas

- **Correctitud:** las barras apiladas conservan los porcentajes por razón, sexo y nivel educativo calculados en el archivo procesado.
- **Etiquetas:** los totales se calculan antes del redondeo y las categorías inferiores a 0,1% se muestran sin exagerar su precisión.
- **Legibilidad:** ambas imágenes fueron inspeccionadas a resolución original; no presentan textos cortados, superpuestos ni elementos fuera del lienzo.
- **Estilo:** usa el lienzo estándar de 4 × 5 pulgadas, tipografía de casa, etiquetas internas de tamaño 3, envolturas compartidas y el logotipo mediante `house_apply_logo()`.
- **Reproducibilidad:** usa rutas relativas, crea el directorio de salida y ejecuta la limpieza previa cuando falta el archivo procesado.
- **Salida:** genera `outputs/figures/37_a_ninis-razones-estudio-ecuador.png` y `outputs/figures/37_b_ninis-razones-trabajo-ecuador.png` a 300 dpi mediante `ragg::agg_png`.
- **Documentación:** el pie identifica la fuente, la autoría, la adaptación y la definición del universo NINI.

## Verificación ejecutada

```text
Rscript scripts/plots/plot_enemdu_nini_razones_sexo.R
Guardados: outputs/figures/37_a_ninis-razones-estudio-ecuador.png y outputs/figures/37_b_ninis-razones-trabajo-ecuador.png
```
