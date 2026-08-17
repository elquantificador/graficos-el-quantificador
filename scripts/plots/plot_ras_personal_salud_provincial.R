# ============================================================
# plot_ras_personal_salud_provincial.R
# Genera el gráfico territorial complementario de la serie RAS:
# personal del MSP por provincia en el último año disponible.
# Requiere: data/raw/ras/msp_serie_*.rds
# Guarda:   outputs/figures/38_personal-salud_provincia-ecuador.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_ras_personal_salud_provincial.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "ggplot2", "ragg", "scales", "tidyr"))

raw_dir <- "data/raw/ras"
out_path <- "outputs/figures/38_personal-salud_provincia-ecuador.png"

raw <- setNames(
  lapply(
    c("nac", "prov", "cant", "parr", "area"),
    function(level) readRDS(file.path(raw_dir, paste0("msp_serie_", level, ".rds")))
  ),
  c("nac", "prov", "cant", "parr", "area")
)

occupation_cols <- c("tmedicos", "tenf", "tobst", "ttaps")
medicos_label <- paste0("M", intToUtf8(233), "dicos")
occupation_labels <- c(
  tmedicos = medicos_label,
  tenf = "Enfermeros",
  tobst = "Obstetrices",
  ttaps = "TAPS"
)

target_year <- max(raw$nac$anio, na.rm = TRUE)

# Validate the supplied geographic aggregates before building the chart.
sum_at_year <- function(data) {
  data %>%
    filter(anio == target_year) %>%
    summarise(across(all_of(occupation_cols), ~ sum(.x, na.rm = TRUE)))
}

reference <- as.numeric(sum_at_year(raw$nac)[1, occupation_cols])
names(reference) <- occupation_cols

for (level in c("prov", "cant", "parr", "area")) {
  candidate <- as.numeric(sum_at_year(raw[[level]])[1, occupation_cols])
  names(candidate) <- occupation_cols
  if (any(abs(candidate - reference) > 1e-8)) {
    stop("La suma de ", level, " no coincide con la serie nacional en ", target_year, ".")
  }
}

plot_df <- raw$prov %>%
  filter(anio == target_year) %>%
  select(prov_nom, all_of(occupation_cols)) %>%
  pivot_longer(
    cols = all_of(occupation_cols),
    names_to = "ocupacion",
    values_to = "total"
  ) %>%
  mutate(
    ocupacion = factor(
      recode(ocupacion, !!!occupation_labels),
      levels = unname(occupation_labels)
    ),
    prov_nom = as.character(prov_nom)
  )

province_totals <- plot_df %>%
  group_by(prov_nom) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(total)
province_levels <- province_totals %>% pull(prov_nom)
top_two <- province_totals %>%
  arrange(desc(total)) %>%
  slice_head(n = 2)
top_two_share <- sum(top_two$total) / sum(province_totals$total)
title_raw <- paste0(
  top_two$prov_nom[1],
  " y ",
  top_two$prov_nom[2],
  " concentran el ",
  label_percent_intl(accuracy = 1)(top_two_share),
  " del personal del MSP"
)

plot_df <- plot_df %>%
  mutate(prov_nom = factor(prov_nom, levels = province_levels))

caption_raw <- paste0(
  "Fuente: Registro de Actividades y Recursos de Salud (RAS), ", target_year, ". ",
  "Elaboración: Odalis Clemente y Alonso Quijano Ruiz para el Quantificador de Laboratorio LIDE. ",
  "Nota: incluye médicos, enfermeros, obstetrices y TAPS. TAPS: Técnicos de Atención Primaria en Salud."
)

build_chart <- function() {
  ggplot(plot_df, aes(x = prov_nom, y = total, fill = ocupacion)) +
    geom_col(
      width = 0.78,
      position = position_stack(reverse = TRUE)
    ) +
    geom_text(
      data = province_totals,
      aes(
        x = factor(prov_nom, levels = province_levels),
        y = total,
        label = label_number_intl(accuracy = 1)(total)
      ),
      inherit.aes = FALSE,
      hjust = -0.15,
      size = 2.5,
      colour = "grey20"
    ) +
    scale_y_continuous(
      labels = label_number_intl(accuracy = 1),
      expand = expansion(mult = c(0, 0.14))
    ) +
    scale_fill_manual(
      values = setNames(
        c("#D04A3E", "#00A8CB", "#F0A145", "#7B8D97"),
        unname(occupation_labels)
      )
    ) +
    coord_flip(clip = "off") +
    labs(
      title = wrap_title_house(title_raw),
      subtitle = wrap_subtitle_house(
        paste0(medicos_label, ", enfermeros, obstetrices y TAPS, ", target_year)
      ),
      x = NULL,
      y = paste0("N", intToUtf8(250), "mero de profesionales"),
      fill = NULL,
      caption = wrap_caption_house(caption_raw)
    ) +
    theme_quantificador() +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.key.size = grid::unit(0.3, "lines"),
      legend.text = element_text(size = 6.3),
      legend.spacing.x = grid::unit(0.15, "lines"),
      legend.box.spacing = grid::unit(0, "lines"),
      legend.margin = margin(t = 0, b = 0, r = 0, l = 0),
      plot.margin = margin(6, 42, 6, 16)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
}

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
spec <- house_spec("portrait")

ggsave(
  filename = out_path,
  plot = house_apply_logo(
    build_chart(),
    "portrait",
    x = 0.88,
    y = 0.18,
    width = 0.07,
    height = 0.07
  ),
  width = spec$width,
  height = spec$height,
  units = "in",
  dpi = spec$dpi,
  device = ragg::agg_png,
  bg = "white"
)

message("Guardado: ", out_path)
