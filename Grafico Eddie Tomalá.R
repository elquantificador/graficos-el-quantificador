##TOMALÁ FIGUEROA EDDIE BRYAN##
##GRÁFICO DE INGRESO MENSUAL POR SECTOR LABORAL##
install.packages("dplyr")
install.packages("ggplot2")
install.packages("visdat")
install.packages("scales")
install.packages("cowplot")
install.packages("magick")

library(dplyr)
library(ggplot2)
library(visdat)
library(scales)
library(cowplot)
library(magick)
enemdu <- read_sav("enemdu_persona_2026_03.sav")

##############################
## CLASIFICACIÓN DEL SECTOR ##
##############################
enemdu <- enemdu %>%
  mutate(
    ## Asignar Sector Formal o Informal a observaciones con NA usando p05a y p05b (Preguntas de seguridad social)
    secemp = ifelse(
      is.na(secemp) & p05a == 10 & p05b == 10,
      2,
      ifelse(
        is.na(secemp) & (p05a != 10 | p05b != 10),
        1,
        secemp
      )
    ) 
  )


##VERIFICAR VALORES FALTANTES EN EL SECTOR LABORAL##
enemdu %>%
  select(secemp) %>%
  vis_miss()

##OBSERVACION: USAR P90##
p90_ingreso <- enemdu %>%
  filter(!is.na(ingrl), ingrl != -1, ingrl != 999999, ingrl > 0) %>%
  pull(ingrl) %>%
  quantile(probs = 0.90)
##VERIFICAR VALORES FALTANTES EN LA VARIABLE INGRESOS##
faltantes_ingrl= enemdu %>%
  select(ingrl) %>%
  filter(!is.na(ingrl),        
         ingrl != -1,          
         ingrl != 999999,      
         ingrl > 0,
         ingrl <= p90_ingreso)%>%
  vis_miss()
faltantes_ingrl


# GRÁFICO MAS FILTRO DE INGRESO IGNORANDO VALORES INVALIDOS
grafico_final <- enemdu %>%
  filter(
    p03 >= 15,            #Población en edad de trabajar
    secemp %in% c(1,2),   #Solo Formal (1) e Informal (2)
    !is.na(ingrl),        
    ingrl != -1,          
    ingrl != 999999,      
    ingrl > 0,
    ingrl <= p90_ingreso  
  ) %>%
  mutate(Sector = factor(secemp, labels = c("Formal", "Informal"))) %>%
  ggplot(aes(
    x = Sector,      
    y = ingrl,
    fill = Sector    
  )) +
  geom_boxplot() +
  
  labs(
    title = "Ingreso laboral por sector",
    x = "Sector laboral",
    y = "Ingreso laboral",
    fill = "Sector"
  ) + coord_flip() +
  labs(
    title = "¿Cuánto gana un trabajador en Ecuador? La brecha real del mercado",
    subtitle = "Distribución del ingreso laboral mensual (personas de 15 años o más)",
    y = "Ingreso laboral mensual (USD)",
    x = NULL, 
    caption = "Fuente: ENEMDU - INEC, marzo 2026. Cálculos por el autor.\nNota metodológica: Las omisiones en la variable de sectorización fueron clasificadas usando la tenencia de seguridad social como proxy de formalidad.\nSectores definidos según la variable 'secemp' (1: Formal, 2: Informal). Los valores perdidos o respuestas inválidas (NA, -1, 999999) fueron excluidos del análisis por no contener información válida.\nEstructura: Los bigotes abarcan al p90."
  )+  scale_y_continuous(
    
    labels = dollar_format(prefix = "$", big.mark = ","),
    
    breaks = seq(0, 1200, by = 200) 
    
  )

grafico_final


########################################################################################################
## GRAFICO ESTILO EL QUANTIFICADOR CON AYUDA DE INTELIGENCIA ARTIFICIAL USANDO CODIGO DE grafico_final##
########################################################################################################


quantificador <- enemdu %>%
  filter(
    p03 >= 15,            
    secemp %in% c(1, 2),  
    !is.na(ingrl),        
    ingrl != -1,          
    ingrl != 999999,      
    ingrl > 0             
  ) %>%
  group_by(
    Sector = factor(secemp, labels = c("Formal", "Informal"))
  ) %>%
  summarise(
    ymin   = quantile(ingrl, 0.10), 
    lower  = quantile(ingrl, 0.25), 
    middle = quantile(ingrl, 0.50), 
    upper  = quantile(ingrl, 0.75), 
    ymax   = quantile(ingrl, 0.90)  
  )


grafico_sector_final <- ggplot(quantificador, aes(x = Sector, ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax)) +
  
  
  geom_boxplot(stat = "identity", fill = "#e79f43", color = "#222222", width = 0.45, linewidth = 0.6) +
  
  
  geom_label(aes(y = middle, label = paste0("$", round(middle, 0))), 
             fill = "white", color = "#222222", fontface = "bold", size = 3.5, 
             label.padding = unit(0.2, "lines"), nudge_x = 0.35) +
  coord_flip() +
  
  
  labs(
    title = "Ingreso laboral por sector",
    subtitle = "Ingresos de la población en edad de trabajar (15+ años)",
    y = "Ingreso laboral mensual (USD)",
    x = NULL, 
    caption = "Fuente: ENEMDU - INEC, marzo 2026. Cálculos por el autor.\nNota metodológica: Las omisiones en la variable de sectorización fueron clasificadas usando la tenencia de seguridad social como proxy de formalidad.\nSectores definidos según la variable 'secemp' (1: Formal, 2: Informal). Los valores perdidos o respuestas inválidas (NA, -1, 999999) fueron excluidos del análisis por no contener información válida.\nCaja = p25-p75, línea = mediana, bigotes = p10-p90."
  ) +
  
  scale_y_continuous(
    labels = dollar_format(prefix = "$", big.mark = ","),
    breaks = seq(0, 1400, by = 200) 
  ) +
  
  theme_classic(base_size = 11) + 
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#111111", margin = margin(b = 4)),
    plot.subtitle = element_text(color = "gray30", size = 10, margin = margin(b = 15)),
    plot.caption = element_text(color = "gray40", size = 8, hjust = 0, margin = margin(t = 15), lineheight = 1.2),
    axis.line = element_line(color = "#444444", linewidth = 0.5),
    axis.text = element_text(color = "black", size = 10, face = "bold"), 
    axis.title.x = element_text(color = "black", size = 10, margin = margin(t = 10)), 
    legend.position = "none"
  )


grafico_sector_final

##Añadir logo QUANTIFICADOR##

grafico_con_logo <- ggdraw(grafico_sector_final) +
  draw_image(
    "quantificador.png",       
    x = 0.85,            
    y = 0.05,            
    width = 0.12,        
    height = 0.12        
  )
grafico_con_logo