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
  quantiles = c(0.05, 0.25, 0.50, 0.75, 0.95),
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
    ymin   = `1`,  # p05
    lower  = `2`,  # p25
    middle = `3`,  # p50 (median)
    upper  = `4`,  # p75
    ymax   = `5`   # p95
  )

# ------------------------------------------------------------
# 4) Plot: match the visual style from your example
# ------------------------------------------------------------

p_base_box <- ggplot(boxstats, aes(x = escolaridad)) +
  geom_boxplot(
    aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
    stat = "identity",
    linewidth = 0.6
  ) +
  labs(
    title = "Los ingresos laborales suben con el nivel educativo\npero con alta dispersión",
    subtitle = "Boxplot ponderado (p05–p95) del ingreso laboral mensual (ingrl)\npor nivel educativo en Ecuador (15+ años)",
    x = "",
    y = "Ingreso laboral mensual (USD)",
    caption = paste0(
      "Fuente: ENEMDU - INEC. Cálculos por el autor.\n",
      "Caja = p25–p75, línea = mediana, bigotes = p05–p95 (ponderado con fexp)."
    )
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  theme_classic() +
  theme(
    axis.text.y  = element_text(colour = "grey20", size = 6.5),
    axis.text.x  = element_text(colour = "grey20", size = 6.5, angle = 45, hjust = 1),
    axis.title.x = element_text(size = 6.5, margin = margin(t = 6), hjust = 0),
    axis.title.y = element_text(size = 6.5, margin = margin(r = 5), hjust = 0),
    plot.title = element_text(colour = "grey20", size = 9.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "grey30", size = 7.5, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(
      colour = "grey30", size = 5.5, lineheight = 1.1, hjust = 0,
      margin = margin(t = 3)
    ),
    axis.line = element_line(colour = "grey60"),
    legend.position = "none",
    plot.margin = margin(12, 20, 8, 14),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

p_base_box

# ------------------------------------------------------------
# 5) Optional: logo overlay (same approach as your example)
# ------------------------------------------------------------
# logo_path <- file.path(project_root, "quantificador.png")
#
# p_box <- ggdraw() +
#   draw_plot(p_base_box, x = 0, y = 0, width = 1, height = 1) +
#   draw_image(
#     logo_path,
#     x = 0.86, y = 0.14,
#     width = 0.10, height = 0.10
#   )
#
# p_box