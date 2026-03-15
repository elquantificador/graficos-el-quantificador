source("scripts/packages.R")
ensure_packages(c("readxl", "dplyr", "tidyr", "stringr", "ggplot2", "scales", "cowplot", "ragg"))

# -----------------------------
# Files
# -----------------------------

file_2010 <- "data/_tmp_9533641 (1).xlsX"
file_2022 <- "data/_tmp_8861401.xlsX"
out_path  <- "figures/cohab_parents_ecuador.png"
out_path_ig <- "figures/cohab_parents_ecuador_instagram.png"
logo_path <- "quantificador.png"

# -----------------------------
# Helper to read REDATAM tabulation
# -----------------------------
read_redatam_tab <- function(path, year) {
  raw <- read_excel(path, sheet = "Output", col_names = FALSE)

  # Age groups are on row 12, starting at column C
  age_groups <- raw[12, 3:ncol(raw)] |> unlist() |> as.character()

  # Data start at row 13
  dat <- raw[13:nrow(raw), 2:ncol(raw)]
  names(dat) <- c("relationship", age_groups)

  dat |>
    mutate(
      relationship = str_squish(as.character(relationship))
    ) |>
    filter(!is.na(relationship), relationship != "", relationship != "Total") |>
    pivot_longer(
      cols = -relationship,
      names_to = "age_group",
      values_to = "n"
    ) |>
    mutate(
      n = ifelse(n == "-", 0, n),
      n = suppressWarnings(as.numeric(n)),
      year = year
    ) |>
    filter(!is.na(n))
}

# -----------------------------
# Read both tables
# -----------------------------
tab_2010 <- read_redatam_tab(file_2010, 2010)
tab_2022 <- read_redatam_tab(file_2022, 2022)

# -----------------------------
# Keep only age groups you want in the chart
# -----------------------------
age_keep <- c("10 - 19 años", "20 - 29 años", "30 - 39 años", "40 - 49 años")

tab_all <- bind_rows(tab_2010, tab_2022) |>
  filter(age_group %in% age_keep)

# -----------------------------
# Define numerator categories
# Based on the simple Excel/Copilot logic
# -----------------------------
num_2010 <- c("Hijo o hija", "Nieto o nieta")
num_2022 <- c("Hija o hijo", "Hijastra o hijastro", "Nieta o nieto")

plot_df <- tab_all |>
  mutate(
    in_numerator = case_when(
      year == 2010 & relationship %in% num_2010 ~ TRUE,
      year == 2022 & relationship %in% num_2022 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  group_by(year, age_group) |>
  summarise(
    numerator = sum(n[in_numerator], na.rm = TRUE),
    denominator = sum(n, na.rm = TRUE),
    share = numerator / denominator,
    .groups = "drop"
  ) |>
  mutate(
    age_group = factor(age_group, levels = age_keep)
  )

print(plot_df)

# -----------------------------
# Chart
# -----------------------------
p_base <- ggplot(plot_df, aes(x = age_group, y = share, fill = factor(year))) +
  geom_col(position = position_dodge(width = 0.85), width = 0.65) +
  geom_text(
    aes(label = percent(share, accuracy = 1)),
    position = position_dodge(width = 0.85),
    vjust = -0.2,
    size = 2.4,
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.06))) +
  scale_fill_manual(values = c("2010" = "#00A1CB", "2022" = "#EF9F4E")) +
  labs(
    title = "¿Cada vez es más difícil independizarse?",
    subtitle = "La proporción de adultos jóvenes en Ecuador que vive con sus\npadres y abuelos* ha aumentado de 2010 a 2022",
    x = NULL,
    y = "Porcentaje de personas que viven con sus padres o abuelos*",
    fill = NULL,
    caption = paste(
      "Fuente: Censo de Población y Vivienda 2010 y 2022, archivo REDATAM.",
      "Nota: La proporción graficada considera individuos que reportan ser hijo/a, hijastro/a o nieto/a del\nrepresentante o jefe del hogar. No se incluyen personas que son padres, padrastros o abuelos\ndel representante, ni relaciones entre miembros del hogar que no sean el representante.",
      sep = "\n"
    )
  ) +
  theme_classic() +
  theme(
    axis.text.y  = element_text(colour = "black", size = 8),
    axis.text.x  = element_text(colour = "black", size = 8),
    axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 1, colour = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 8, colour = "black"),
    legend.key.size = grid::unit(0.35, "cm"),
    plot.title = element_text(colour = "black", size = 12.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(colour = "black", size = 9, lineheight = 1.1, hjust = 0),
    plot.caption = element_text(colour = "black", size = 5.5, lineheight = 1.1, hjust = 0, margin = margin(t = 6)),
    axis.line = element_line(colour = "black"),
    plot.margin = margin(6, 36, 6, 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank()
  )

p_final <- ggdraw() +
  draw_plot(p_base, x = 0, y = 0, width = 1, height = 0.99) +
  draw_image(
    logo_path,
    x = 0.88, y = 0.20,
    width = 0.10, height = 0.10
  )

p_final

# -----------------------------
# Optional: save plot
# -----------------------------
ggsave(
  filename = out_path,
  plot = p_final,
  width = 9,
  height = 6,
  dpi = 300,
  device = ragg::agg_png
)

ggsave(
  filename = out_path_ig,
  plot = p_final,
  width = 4,
  height = 5,
  dpi = 300,
  device = ragg::agg_png
)