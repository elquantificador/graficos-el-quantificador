# ============================================================
# Weighted boxplot (p05–p95) of ingrl by escolaridad
# ENEMDU complex design: upm / estrato / fexp
# ============================================================

library(dplyr)
library(survey)
library(ggplot2)
library(scales)
library(cowplot)
library(haven)

out_path  <- "figures/escolaridad_ingreso.png"
logo_path <- "quantificador.png"

df <- read_sav("c:/Users/user/Downloads/1_BDD_ENEMDU_2026_01_SPSS/enemdu_persona_2026_01.sav")

# ------------------------------------------------------------
# 1) Clean data and set category order
# ------------------------------------------------------------


df2 <- df %>%
  mutate(
    # asegurar tipos
    p03  = as.numeric(p03),
    p10a = as.numeric(p10a),
    ingrl = as.numeric(ingrl),

    # limpiar códigos de no respuesta (ajusta si en tu base hay otros)
    ingrl = na_if(ingrl, 999999),
    ingrl = if_else(ingrl == -1, NA_real_, ingrl),

    escolaridad = case_when(
      p10a %in% c(1, 2, 3, 4, 5) ~ "Menos de secundaria",
      p10a %in% c(6, 7)          ~ "Secundaria/Bachillerato",
      p10a == 8                  ~ "Tecnología / sup. no universitario",
      p10a == 9                  ~ "Universidad",
      p10a == 10                 ~ "Postgrado",
      TRUE                       ~ NA_character_
    )
  ) %>%
  filter(p03 >= 15, !is.na(escolaridad))

df2_plot <- df2 %>%
  mutate(
    ingrl = as.numeric(ingrl),
    ingrl = na_if(ingrl, 999999),
    ingrl = if_else(ingrl == -1, NA_real_, ingrl),
    escolaridad = factor(
      escolaridad,
      levels = c(
        "Menos de secundaria",
        "Secundaria/Bachillerato",
        "Tecnología / sup. no universitario",
        "Universidad",
        "Postgrado"
      )
    )
  ) %>%
  filter(!is.na(ingrl), ingrl >= 0, !is.na(escolaridad))

options(survey.lonely.psu = "certainty")

# ------------------------------------------------------------
# 2) Declare complex survey design
# ------------------------------------------------------------

des2 <- svydesign(
  id      = ~upm,
  strata  = ~estrato,
  weights = ~fexp,
  data    = df2_plot,
  nest    = TRUE
)

# ------------------------------------------------------------
# 3) Weighted quantiles by group
# ------------------------------------------------------------

q <- svyby(
  ~ingrl, ~escolaridad, des2,
  svyquantile,
  quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90),
  na.rm = TRUE,
  ci = FALSE,
  se = FALSE,
  keep.var = FALSE
)

# NOTE: svyquantile output columns are usually named "1".."5" here.
# If yours are "0.05", "0.25", ... then replace `1` with `0.05`, etc.
boxstats <- as.data.frame(q) %>%
  transmute(
    escolaridad = escolaridad,
    ymin   = `1`,  # p10
    lower  = `2`,  # p25
    middle = `3`,  # p50 (median)
    upper  = `4`,  # p75
    ymax   = `5`   # p90
  )

# ------------------------------------------------------------
# 4) Plot: match the visual style from your example
# ------------------------------------------------------------

p_base_box <- ggplot(boxstats, aes(x = escolaridad)) +
  geom_boxplot(
    aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
    stat = "identity",
    fill = "#ef9f4e",
    linewidth = 0.6
  ) +
  labs(
    title = "El retorno a la educación: solo una maestría\nsupera los USD 1.000 mensuales",
    subtitle = "Distribución del ingreso laboral mensual por nivel educativo\nEcuador, enero 2026 · población de 15 años y más",
    x = "",
    y = "Ingreso laboral mensual (USD)",
    caption = paste0(
      "Fuente: ENEMDU - INEC, enero 2026. Cálculos propios.\n",
      "Caja = p25–p75, línea = mediana, bigotes = p10–p90 (percentiles ponderados)."
    )
  ) +
  scale_y_continuous(
    labels = label_dollar(big.mark = ",", accuracy = 1),
    breaks = seq(0, 3000, 500),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 3000)) +
  theme_classic() +
  theme(
    axis.text.y  = element_text(colour = "grey20", size = 7.5),
    axis.text.x  = element_text(colour = "grey20", size = 7.5, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 7, margin = margin(t = 8, r = 0, b = 0, l = 0), hjust = 0),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 1),
    plot.title    = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption  = element_text(colour = "grey30", size = 5, lineheight = 1.1, hjust = 0, margin = margin(t = 6, r = 0, b = 0, l = 0)),
    axis.line = element_line(colour = "grey60"),
    legend.position = "none",
    plot.margin = margin(14, 36, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

# ------------------------------------------------------------
# 5) Logo overlay
# ------------------------------------------------------------

p_box <- ggdraw() +
  draw_plot(p_base_box, x = 0, y = 0, width = 1, height = 1) +
  draw_image(
    logo_path,
    x = 0.88, y = 0.20,
    width = 0.10, height = 0.10
  )

p_box

# ---- Guardar ----
ggsave(out_path, p_box, width = 4, height = 5, units = "in", dpi = 300, device = ragg::agg_png)