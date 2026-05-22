# ============================================================
# plot_reess_salario_industrias.R
# Genera el top 5 de industrias mejor pagadas usando salario
# promedio del empleo registrado en REESS.
# Requiere: data/processed/reess_salario_industrias_feb_2025_2026.rds
# Guarda:   figures/reess_salario_industrias.png
# ============================================================
# Ejecutar desde la raíz del proyecto:
#   Rscript scripts/plots/plot_reess_salario_industrias.R
# ============================================================

source("scripts/utils.R")
source("scripts/packages.R")
ensure_packages(c("dplyr", "tidyr", "ggplot2", "scales", "stringr", "ragg"))

input_path <- "data/processed/reess_salario_industrias_feb_2025_2026.rds"
out_path <- "figures/reess_salario_industrias.png"

plot_df <- readRDS(input_path)
total_empleo <- sum(plot_df$empleo_feb_2026, na.rm = TRUE)

top5_df <- plot_df %>%
  arrange(desc(salario_feb_2026)) %>%
  slice_head(n = 5)

share_top5 <- 100 * sum(top5_df$empleo_feb_2026, na.rm = TRUE) / sum(plot_df$empleo_feb_2026, na.rm = TRUE)

plot_df <- top5_df %>%
  transmute(
    industria = stringr::str_wrap(industria, width = 38),
    salario = salario_feb_2026,
    empleo = empleo_feb_2026,
    pct_empleo = 100 * empleo_feb_2026 / total_empleo
  )

industry_order <- plot_df %>%
  arrange(salario) %>%
  pull(industria)

plot_df <- plot_df %>%
  mutate(industria = factor(industria, levels = industry_order))

label_usd <- function(x) {
  format(round(x, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

label_pct <- function(x) {
  ifelse(
    x < 0.1,
    "<0,1",
    format(round(x, 1), nsmall = 1, decimal.mark = ",", scientific = FALSE)
  )
}

plot_df <- plot_df %>%
  mutate(
    label_barra = paste0("$", label_usd(salario), " (", label_pct(pct_empleo), "%)")
  )

p_base <- ggplot(plot_df, aes(x = industria, y = salario, fill = empleo)) +
  geom_col(
    width = 0.62
  ) +
  geom_text(
    aes(label = label_barra),
    hjust = -0.08,
    size = 2.3,
    colour = "grey20"
  ) +
  coord_flip(clip = "off") +
  scale_fill_gradient(
    low = "#BDD7E4",
    high = "#0F6B84",
    labels = function(x) format(round(x, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    name = "Número de empleos formales"
  ) +
  scale_y_continuous(
    labels = function(x) paste0("$", label_usd(x)),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = paste0(
      "Los sectores mejor pagados del Ecuador\n",
      "apenas concentran ", format(round(share_top5, 0), decimal.mark = ","), "% del empleo formal"
    ),
    subtitle = "Top 5 industrias por salario mensual promedio, feb. 2026",
    x = NULL,
    y = "Salario mensual promedio",
    caption = paste0(
      "Fuente: Registro Estadístico de Empleo en la Seguridad Social (REESS), corte de febrero 2026. Cálculos\n",
      "por Santiago Guamán para El Quantificador de Laboratorio LIDE. Salario mensual promedio del empleo\n",
      "registrado por rama de actividad económica CIIU Rev. 4.1 nivel 1. El color más oscuro indica más\n",
      "empleo formal registrado. Los datos de febrero son preliminares y están sujetos a revisión."
    )
  ) +
  theme_quantificador() +
  theme(
    legend.position = "bottom",
    legend.justification = "left",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.key.width = unit(0.62, "cm"),
    legend.key.height = unit(0.18, "cm"),
    legend.spacing.x = unit(0.05, "cm"),
    legend.margin = margin(0, 0, 0, -52),
    legend.box.margin = margin(0, 0, 0, -18),
    legend.text = element_text(size = 6.2),
    legend.title = element_text(size = 6.4),
    axis.text.y = element_text(colour = "grey20", size = 6.6),
    axis.text.x = element_text(angle = 25, hjust = 0.68, vjust = 0.82),
    axis.title.x = element_text(size = 7),
    plot.caption = element_text(colour = "grey30", size = 5.9, lineheight = 1.12, hjust = 0, margin = margin(t = 8)),
    plot.margin = margin(8, 38, 6, 6)
  ) +
  guides(fill = guide_colorbar(order = 1, barwidth = unit(3.2, "cm"), barheight = unit(0.18, "cm")))

dir.create("figures", showWarnings = FALSE)
p_final <- add_logo(p_base, x = 0.89, y = 0.27, width = 0.09, height = 0.09)

ggsave(
  filename = out_path,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)

message("Guardado: ", out_path)
