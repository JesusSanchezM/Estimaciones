#-----------------------------------
#---------- Librerías --------------
#-----------------------------------
library(dplyr)
library(readr)
library(readxl)

#-----------------------------------
#---------- Datos ------------------
#-----------------------------------
# Ajusta rutas según tu PC
poblacion_2018_1 <- read_csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2018_ns_poblacion_csv/poblacion.csv")
con_hogar_2018_1 <- read_csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2018_ns_concentradohogar_csv/concentradohogar.csv")
poblacion_2024_1 <- read_csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2024_ns_poblacion_csv/poblacion.csv")
con_hogar_2024_1 <- read_csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2024_ns_concentradohogar_csv/concentradohogar.csv")

#-----------------------------------
#---------- Preprocesamiento -------
#-----------------------------------
preparar_poblacion <- function(df) {
  df <- df %>%
    filter(edad >= 15, edad <= 24, parentesco == 301) %>%
    mutate(
      padre_madre = case_when(
        madre_hog == 2 & padre_hog == 2 ~ 0,
        madre_hog == 1 & padre_hog == 2 ~ 1,
        madre_hog == 2 & padre_hog == 1 ~ 2,
        madre_hog == 1 & padre_hog == 1 ~ 3,
        TRUE ~ NA_real_
      ),
      padre_madre = factor(padre_madre,
                           levels = c(0, 1, 2, 3),
                           labels = c("Ninguno", "Solo madre", "Solo padre", "Ambos")),
      asis_esc = factor(asis_esc, 
                        levels = c(1, 2), 
                        labels = c("Asiste", "No asiste"))
    )
  return(df)
}

poblacion_2018 <- preparar_poblacion(poblacion_2018_1)
poblacion_2024 <- preparar_poblacion(poblacion_2024_1)

# Columnas a conservar
cols_poblacion <- c('folioviv','foliohog','sexo','edad','parentesco','madre_hog','padre_hog','asis_esc', "padre_madre")
cols_hogar <- c('folioviv','foliohog','est_dis','upm','factor','tot_integ','ing_cor')

poblacion_2018 <- poblacion_2018 %>% select(all_of(cols_poblacion))
poblacion_2024 <- poblacion_2024 %>% select(all_of(cols_poblacion))
con_hogar_2018 <- con_hogar_2018_1 %>% select(all_of(cols_hogar))
con_hogar_2024 <- con_hogar_2024_1 %>% select(all_of(cols_hogar))

# Unir con concentrado del hogar
datos_2018 <- inner_join(con_hogar_2018, poblacion_2018, by = c("folioviv","foliohog"), multiple="all")
datos_2024 <- inner_join(con_hogar_2024, poblacion_2024, by = c("folioviv","foliohog"), multiple="all")

# Añadir columna año
datos_2018 <- datos_2018 %>% mutate(año = 2018)
datos_2024 <- datos_2024 %>% mutate(año = 2024)

# Unir ambos años
datos_unidos <- bind_rows(datos_2018, datos_2024)

write_csv(datos_unidos, "datos_unidos.csv")

#Asistencia escolar por edades
{

  est_desc <- read_excel("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/Estadistica descriptiva con diseño de muestreo.xlsx", 
                                                                sheet = "Distribucion general ")
  ggplot(est_desc, aes(x = as.factor(Edad), y = Asiste*100, fill = factor(Año))) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8, color = "black", size = 0.3) +
    geom_text(aes(y = (Asiste*100)/2, label = sprintf("%.1f%%", Asiste*100)),
              position = position_dodge(width = 0.9),
              angle = 90, hjust = 0.5,
              size = 3.5, color = "black") +
    labs(
      x = "Edad",
      y = "Porcentaje de asistencia",
      fill = "Año"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}

#Asistencia escolar por sexos
{
  est_desc_1 <- read_excel("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/Estadistica descriptiva con diseño de muestreo.xlsx", 
                           sheet = "Distribucion por sexo")
  
  est_desc_1 %>%
  mutate(Sexo = factor(Sexo,
                         levels = c(1, 2),
                         labels = c("Hombres", "Mujeres"))) %>% 
  ggplot(aes(x = as.factor(Edad), y = Asiste*100, fill = factor(Año))) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8, color = "black", size = 0.3) +
    geom_text(aes(y = (Asiste*100)/2, label = sprintf("%.1f%%", Asiste*100)),
              position = position_dodge(width = 0.9),
              angle = 90, hjust = 0.5,
              size = 3.5, color = "black") +
    labs(
      x = "Edad",
      y = "Porcentaje de asistencia",
      fill = "Año"
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )+ 
    facet_wrap(~Sexo)
  
}



