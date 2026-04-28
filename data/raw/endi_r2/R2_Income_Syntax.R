
#==============================================================================#
####                          Carga de base de datos                        ####
#==============================================================================#

library(tidyverse)
library(Hmisc)
library(summarytools)
#==============================================================================#
####                               Quintiles                                ####
#==============================================================================#

# Tratamiento 9s --------------------------------------------------------------# 

# Creacion de vector de variable involucradas 
var <- c("f1_s2_9", "f1_s2_10_2", "f1_s2_11", 
         "f1_s2_12", "f1_s2_13","f1_s2_14_2", 
         "f1_s2_15", "f1_s2_16_2", "f1_s2_17_2", 
         "f1_s2_18_2", "f1_s2_19_2", "f1_s2_20_2", 
         "f1_s2_22")

# Formato numerico para variables 
ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(across(c(all_of(var)), as.numeric)) 

# Homologacion de 9s a una cantidad fija
for (i in var) {
  ENDI_R2_persons[[i]] <- case_when(
    ENDI_R2_persons[[i]] == 9999999 ~ 999999,
    ENDI_R2_persons[[i]] == 99999999 ~ 999999,
    TRUE ~ ENDI_R2_persons[[i]])
}

# Creacion de variable de control para diferentes escenarios
# x = 1:
# Si el individuo en su actividad laboral principal, no informa (no conoce) de 
# sus ingresos como dependiente o independiente, segun sea el caso. (Un 
# individuo solo puede tener ingresos como dependiente o independiente ya que 
# las categorias son excluyentes). Si el individuo no informa de ambas fuentes 
# de la actividad secundaria (monetario y especies) y no posee ingresos no 
# laborales.
# x = 2:
# Si el individuo no informa de ambas fuentes de la actividad secundaria 
# (monetario y especies) pero si posee ingresos no laborales, entonces el 
# ingreso individual total es el ingreso no laboral (esto ultimo se considera 
# mas adelante).

ENDI_R2_persons <- ENDI_R2_persons %>%
  mutate(x = case_when(
    (f1_s2_9 == 999999 | f1_s2_12 == 999999 ) |
      (f1_s2_15 == 999999 & f1_s2_16_2 == 999999 &
         is.na(f1_s2_17_2) & is.na(f1_s2_18_2) & is.na(f1_s2_19_2) & 
         is.na(f1_s2_20_2) & is.na(f1_s2_22)) ~ 1,
    (f1_s2_15 == 999999 & f1_s2_16_2 == 999999) & 
      (!is.na(f1_s2_17_2) | !is.na(f1_s2_18_2) | !is.na(f1_s2_19_2) |
         !is.na(f1_s2_20_2) | !is.na(f1_s2_22)) ~ 2,
    TRUE ~ 0))

# Control de 9s 
for (i in var) {
  ENDI_R2_persons[[i]] <- case_when(
    ENDI_R2_persons[[i]] == 999999 ~ NA_real_, 
    TRUE ~ ENDI_R2_persons[[i]])
}

for (i in var) {
  ENDI_R2_persons <- ENDI_R2_persons %>%
    mutate(x = case_when(
      (!!sym(i) == 999 | !!sym(i) == 9999 | !!sym(i) == 99999) ~ 1, 
      TRUE ~ x))
}

# Ingreso laboral -------------------------------------------------------------#															   

# Actividad principal - Asalariados e Independientes
ENDI_R2_persons <- ENDI_R2_persons %>%
  mutate(f1_s2_11 = -f1_s2_11)

ENDI_R2_persons <- ENDI_R2_persons %>%
  rowwise() %>%
  mutate(ind = sum(c(f1_s2_9, f1_s2_10_2, f1_s2_11), na.rm = T)) %>%
  mutate(asal = sum(c(f1_s2_12, f1_s2_13, f1_s2_14_2), na.rm = T)) %>% 
  ungroup()

ENDI_R2_persons <- ENDI_R2_persons %>%
  rowwise() %>% 
  mutate(ila1 = sum(c(ind, asal), na.rm = T)) %>% 
  ungroup()

# Actividad secundaria - Asalariados e Independientes
ENDI_R2_persons <- ENDI_R2_persons %>%
  rowwise() %>% 
  mutate(ila2 = sum(c(f1_s2_15, f1_s2_16_2), na.rm = T)) %>% 
  ungroup()

# Ingreso Laboral
ENDI_R2_persons <- ENDI_R2_persons %>% 
  rowwise() %>% 
  mutate(ila = sum(c(ila1, ila2), na.rm = T)) %>% 
  ungroup()

ENDI_R2_persons <- ENDI_R2_persons %>%
  mutate(ila = case_when(
    ila1 < 0 ~ ila2,
    TRUE ~ ila))

# "ineg" es una variable que identifica a las personas 
# que gastan mas de lo que ganan monetariamente.

ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(ineg = case_when(
    ila1 < 0 & ila == NA_real_ ~ 1,
    x == 1 ~ NA_real_))

# Ingreso no laboral ----------------------------------------------------------# 

# Rentas del capital, Propiedad, Transferencias y regalos 

# Ingresos de capital
ENDI_R2_persons <- ENDI_R2_persons %>%  
  mutate(icap = f1_s2_17_2)

# Ingresos por transferencias
ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(ipens = f1_s2_18_2,
         ilocal = f1_s2_19_2,
         iextr = f1_s2_20_2,
         isocial = f1_s2_22)

ENDI_R2_persons <- ENDI_R2_persons %>% 
  rowwise() %>% 
  mutate(itrans = sum(c(ipens, ilocal, iextr, isocial), na.rm = T)) %>% 
  ungroup()

# Ingresos no laborales
ENDI_R2_persons <- ENDI_R2_persons%>%
  rowwise() %>% 
  mutate(inla = sum(c(icap, itrans), na.rm = T)) %>% 
  ungroup()

# Si es incoherente se hace missing todas las fuentes del ingreso individual
var1 <- c("ind", "asal", "ila", "icap", "ipens", 
          "ilocal", "iextr", "isocial", "itrans", 
          "inla")

for (i in var1) {
  ENDI_R2_persons <- ENDI_R2_persons %>% 
    mutate(!!sym(i) := case_when(
      x == 1 ~ NA_real_, 
      TRUE ~ !!sym(i))
    )
}

# Ingreso individual-----------------------------------------------------------#												 		   

ENDI_R2_persons <- ENDI_R2_persons %>% 
  rowwise() %>% 
  mutate(ii = sum(c(ila, inla), na.rm = T)) %>% 
  ungroup()

# Si es incoherente se hace missing el ingreso individual 
ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(ii = case_when(
    x == 1 ~ NA_real_, 
    TRUE ~ ii))

# Si no informa de ambas fuentes de la actividad secundaria pero si posee
# ingresos no laborales, el ingreso individual total es el ingreso no laboral
ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(ii = case_when(
    x == 2 ~ inla, 
    TRUE ~ ii))

# Si existen ingresos individuales "cero" se los reemplaza por NAs
ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(ii = case_when(
    ii == 0 ~ NA_real_, 
    TRUE ~ ii))

# Ingreso familiar-------------------------------------------------------------#	

ENDI_R2_persons <- ENDI_R2_persons %>% 
  arrange(id_hogar) 

ENDI_R2_persons <- ENDI_R2_persons %>% 
  group_by(id_hogar) %>% 
  mutate(ilaf = sum(ila),
         icapf = sum(icap),
         ipensf = sum(ipens),
         ilocalf = sum(ilocal),
         iextrf = sum(iextr),
         isocialf = sum(isocial),
         itransf = sum(itrans),
         inlaf = sum(inla)) %>% 
  ungroup()

var2 <- c("ilaf", "icapf", "ipensf", "ilocalf", 
          "iextrf", "isocialf", "itransf", "inlaf")

for (i in var2) {
  ENDI_R2_persons <- ENDI_R2_persons %>% 
    mutate(!!sym(i) := case_when(
      !!sym(i) == 0 ~ NA_real_, 
      TRUE ~ !!sym(i))
    )
}

# Ingreso familiar
ENDI_R2_persons <- ENDI_R2_persons %>% 
  group_by(id_hogar) %>% 
  mutate(ih = sum((ii), na.rm = T)) %>% 
  ungroup()

ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(ih = case_when(
    ih == 0 ~ NA_real_, 
    TRUE ~ ih))

# Ingreso percapita familiar---------------------------------------------------# 	

ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(nump = 1)

# Ingreso percapita familiar
ENDI_R2_persons <- ENDI_R2_persons %>% 
  group_by(id_hogar) %>% 
  mutate(hsize = sum(nump)) %>% # Miembros por hogar   
  mutate(ipcf = ih / hsize) %>%  # Ingreso per capita familiar
  ungroup()

# Logaritmo del ingreso per capita familiar
ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(lipcf = log(ipcf))

# Calculo de quintiles
ENDI_R2_persons <-ENDI_R2_persons%>%
  mutate(quintil = as.numeric(
    cut(lipcf, breaks = wtd.quantile(
      lipcf, w = fexp, probs = seq(0,1, length = 6), na.rm = TRUE),
      include.lowest = TRUE)))

ENDI_R2_persons$quintil <- factor(ENDI_R2_persons$quintil,
                                 levels = c("1","2","3","4","5"),
                                 labels = c("Quintil 1", "Quintil 2", 
                                            "Quintil 3", "Quintil 4", 
                                            "Quintil 5"))

ENDI_R2_persons %>% 
  freq(quintil, cumul = F)

ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(ipcf_new = ipcf)

#==============================================================================#
####                 Linea de pobreza y extrema pobre                       ####			  
#==============================================================================#

ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(ipc_ = as.numeric(case_when(
    fecha_anio == "2023" & 
      fecha_mes == "07" ~ 111.181541477615, 
    fecha_anio == "2023" & 
      fecha_mes == "08" ~ 111.780858152468, 
    fecha_anio == "2023" & 
      fecha_mes == "09" ~ 112.343081099914, 
    fecha_anio == "2023" & 
      fecha_mes == "10" ~ 112.385606673683, 
    fecha_anio == "2023" & 
      fecha_mes == "11" ~ 112.188911619948,
    fecha_anio == "2023" & 
      fecha_mes == "12" ~ 111.741053926941,
    fecha_anio == "2024" & 
      fecha_mes == "01" ~ 111.715101416755,
    fecha_anio == "2024" & 
      fecha_mes == "02" ~ 111.855131486751,
    fecha_anio == "2024" & 
      fecha_mes == "03" ~ 111.958952108953,
    fecha_anio == "2024" & 
      fecha_mes == "04" ~ 112.281439749787,
    fecha_anio == "2024" & 
      fecha_mes == "05" ~ 113.710894638438,
    fecha_anio == "2024" & 
      fecha_mes == "06" ~ 113.575358026089,
    fecha_anio == "2024" & 
      fecha_mes == "07" ~ 112.494039825892,
    fecha_anio == "2024" & 
      fecha_mes == "08" ~ 113.540676380544,
  )))

ENDI_R2_persons <- ENDI_R2_persons %>% 
  rename(ingtot_pc = ipcf)

# Linea de pobreza
ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(lpobre = 56.64 * (ipc_ / 70.262819184092)) 

ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(pobreza = case_when(
    ingtot_pc < lpobre ~ 1,
    !is.na(ingtot_pc) ~ 0))

ENDI_R2_persons %>% 
  freq(pobreza, cumul = F)

# Linea de extrema pobreza
ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(lepobre = 31.92 * (ipc_ / 70.262819184092))

ENDI_R2_persons <- ENDI_R2_persons %>% 
  mutate(epobreza = case_when(
    ingtot_pc < lepobre ~ 1,
    !is.na(ingtot_pc) ~ 0))

ENDI_R2_persons %>% 
  freq(epobreza, cumul = F)
