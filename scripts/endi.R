# Librerías
library(haven)
library(tidyverse)
library(survey)
library(srvyr)

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("scales", "ragg"))

# Ruta
path_rawdata_r2 <- "data/endi_r2"

# Cargar datos
endi_r2_personas <- readRDS(file.path(path_rawdata_r2, "BDD_ENDI_R2_f1_personas.rds"))
 
# Crear variables demográficas ==========
endi_r2_personas <- endi_r2_personas %>%
  mutate(
    sexo = as_factor(f1_s1_2),
    etnia = as_factor(etnia)
  )

# Crear diseño de encuesta ==========
endi_r2_svy <- as_survey_design(
  endi_r2_personas,
  ids = id_upm,
  weights = fexp,
  strata = estrato,
  nest = TRUE
)

# Prevalencia de desnutrición ==========
r2_prev_dcronica_etnia <- endi_r2_svy %>% 
  group_by(etnia) %>%
  summarize(
    prev_dcronica = survey_mean(dcronica_2, vartype = c("ci"), na.rm = TRUE)
  )


# Gráficos ==========

out_path <- "figures/prev_dcronica_etnia_endi_r2.png"

p_base <- ggplot(r2_prev_dcronica_etnia, aes(x = fct_reorder(etnia, prev_dcronica), y = prev_dcronica)) +
  geom_col(fill = "#EF9F4E", width = 0.65) +
  geom_text(
    aes(label = percent(prev_dcronica, accuracy = 1)),
    vjust = -0.2,
    size = 2.6
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    title = "Casi uno de cada tres niños indígenas\npadece de desnutrición en Ecuador",
    subtitle = "Prevalencia de desnutrición crónica por etnia, niños y niñas\nen Ecuador (Ronda 2 ENDI 2023-2024)",
    x = NULL,
    y = "Porcentaje con desnutrición crónica",
    caption = "Fuente: Encuesta Nacional de Desnutrición Infantil, Ronda 2 2023-2024. Cálculos propios. Proporciones son\nponderadas de acuerdo a pesos muestrales. La desnutrición presentada es para niños menores de 2 años."
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(colour = "black", size = 8),
    axis.text.x = element_text(colour = "black", size = 8, angle = 25, margin = margin(t = 10)),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 1, colour = "black"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 4)),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(6, 36, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.88, y = 0.10)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)
message("Guardado: ", out_path)