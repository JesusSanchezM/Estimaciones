#-----------------------------------
#---------- Librerías --------------
#-----------------------------------
library(dplyr)
library(readr)

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
        madre_hog == 2 & padre_hog == 2 ~ "Ninguno",
        madre_hog == 1 & padre_hog == 2 ~ "Solo madre",
        madre_hog == 2 & padre_hog == 1 ~ "Solo padre",
        madre_hog == 1 & padre_hog == 1 ~ "Ambos"
      ),
      asis_esc = recode(asis_esc, `1` = "Asiste", `2` = "No asiste")
    )
  return(df)
}

poblacion_2018 <- preparar_poblacion(poblacion_2018_1)
poblacion_2024 <- preparar_poblacion(poblacion_2024_1)

# Columnas a conservar
cols_poblacion <- c('folioviv','foliohog','sexo','edad','parentesco','madre_hog','padre_hog','asis_esc')
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





