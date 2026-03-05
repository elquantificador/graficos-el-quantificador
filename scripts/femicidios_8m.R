# Librerías

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(png))     install.packages("png",     repos = "https://cloud.r-project.org")
if(!require(jpeg))    install.packages("jpeg",    repos = "https://cloud.r-project.org")
if(!require(grid))    install.packages("grid",    repos = "https://cloud.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "https://cloud.r-project.org")

library(png)
library(jpeg)
library(grid)
library(cowplot)

# ---- Rutas ----
logo_path <- "quantificador.png"
out_path  <- "figures/femicidios_8m.png"

# Datos

# Carga de datos del Fiscalía (Femicidios Oficiales + Otras Muertes)

muertes_fem<-
  read.csv('data/muertes_fem_fiscalia_2026.csv') %>% 
  rename(año = year, cantidad = value, tipo = category) %>% 
  filter(año < 2026) |> 
  mutate(tipo = case_when(
    tipo == 'Femicidios' ~ 'Femicidios',
    TRUE ~ 'Otras muertes'
  )) # Renombrar categorías para que sean más claras en el gráfico

# Se construye una base de solamente los totales

femicidios_totales <- 
  muertes_fem %>% 
  group_by(año) %>% 
  summarize(cantidad = sum(cantidad)) %>% 
  ungroup()

# Formatos

# Colores para mujeres - estilo naranja/marrón para coincidir con diseño limpio

purple_women <- "#88398a" # Color morado para estadísticas de mujeres
purple_women2 <- '#52307c'  # Color morado más oscuro para estadísticas de mujeres

# Tema para los gráficos de mujeres - diseño limpio y minimal

theme_women <-
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.caption = element_text(hjust = 0, face = 'italic', size = 5,
                                    colour = "grey30", lineheight = 1.1,
                                    margin = margin(t = 6, r = 0, b = 0, l = 0)),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        text = element_text(color = 'black', family = 'sans'),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = 'black'),
        axis.line.x = element_line(color = 'black'),
        axis.line.y = element_line(color = 'black'),
        plot.title    = element_text(colour = "grey20", size = 12.5, face = "bold", hjust = 0),
        plot.subtitle = element_text(colour = "grey30", size = 9, lineheight = 1.1, hjust = 0, margin = margin(b = 8)),
        axis.text = element_text(size = 7.5),
        axis.title.x = element_text(size = 7, margin = margin(t = 8, r = 0, b = 0, l = 0), hjust = 0),
        axis.title.y = element_text(size = 7, margin = margin(r = 6), hjust = 1),
        plot.margin = margin(14, 36, 4, 16),
        plot.title.position = "plot",
        plot.caption.position = "plot")

# Caption largo para el gráfico 

caption_grafo1 <- paste0(
  'Fuente: Fiscalía General del Estado. Femicidios corresponden al delito de femicidio según el art. 141 del Código Orgánico',
  '\n',
  str_wrap('Integral Penal. Las cifras de otras muertes incluyen asesinatos, homicidios intencionales, sicariatos, robos, ejecuciones extrajudiciales, entre otros. Elaboración por los autores.', 121)
)

# Gráfico

femicidios_col <- 
  ggplot(muertes_fem, aes(x = as.character(año), y = cantidad, fill = tipo))+
  geom_col(width = 0.7,
           position = 'stack',
           color = 'black')+
  labs(x = '',
       y = 'Número de muertes',
       title = 'El gobierno ecuatoriano no conoce con exactitud\ncuantas mujeres mueren por femicidio cada año',
       subtitle = 'Los femicidios en Ecuador han caído en los últimos años, pero las mujeres\nsiguen muriendo en contexto delictivo',
       fill = 'Tipo de muerte',
       caption = caption_grafo1)+
  scale_fill_manual(values = c('Femicidios' = purple_women, 'Otras muertes' = purple_women2),
                    limits = c('Femicidios', 'Otras muertes'))+ # Utilizando el argumento "limits" no tengo que incluir el total en la leyenda 
  scale_y_continuous(breaks = seq(0, 800, 100),
                     limits = c(0, 800),
                     expand = c(0, 0))+
  geom_text(aes(label = cantidad),
            position = position_stack(vjust = 0.5),
            color = 'white',
            size = 2.5,
            fontface = 'bold')+
  theme_women+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6, color = 'black'),
        legend.title = element_text(size = 6, color = 'black'),
        axis.text.y = element_text(size = 6, color = 'black'),
        axis.text.x = element_text(size = 6, color = 'black'),
        axis.ticks.y = element_line(color = 'black', linewidth = 0.3),
        legend.box.spacing = unit(2, 'pt')) # Mostrar ticks en el eje y

# ---- Agregar logo ----
femicidios_final <- ggdraw() +
  draw_plot(femicidios_col, x = 0, y = 0, width = 1, height = 1) +
  draw_image(
    logo_path,
    x = 0.90, y = 0.07,
    width = 0.10, height = 0.10
  )

femicidios_final

# ---- Guardar ----
ggsave(out_path, femicidios_final, width = 4.5, height = 5.5, units = "in", dpi = 300, device = ragg::agg_png)
