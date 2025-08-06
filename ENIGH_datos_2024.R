#-----------------------------------
#----------Librerias----------------
#-----------------------------------

{
  library(readr) #abrir csv
  library(haven) #abrir dta
  library(tidyverse)
  library(scales)  # Para label_comma()
  library(psych)
  
}

#-----------------------------------
#--------------Datos----------------
#-----------------------------------

{
  poblacion_2024_2024 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2024_ns_poblacion_csv/poblacion.csv")
  View(poblacion_2024_2024)
  str(poblacion_2024_2024$edad)
  describe(poblacion_2024_2024$edad)
  table(poblacion_2024_2024$asis_esc)
}


ggplot(
  data = poblacion_2024 %>%
    filter(edad >= 15 & edad <= 24 & !is.na(asis_esc)) %>%
    mutate(asis_esc = factor(asis_esc, levels = c(1, 2), labels = c("Asiste", "No asiste"))),
  aes(x = as.factor(edad), fill = asis_esc)
) +
  geom_bar(position = "fill", color="black", size=1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proporción de asistencia escolar por edad (15 a 24 años)",
    x = "Edad",
    y = "Porcentaje",
    fill = "Asistencia escolar"
  ) +
  theme_classic()



poblacion_2024_1 <- poblacion_2024 %>%
  filter(edad >= 15 & edad <= 24) %>%
  filter(parentesco == 301) %>%
  mutate(padre_madre = case_when(
    madre_hog == 2 & padre_hog == 2 ~ 1,  # No madre ni padre
    madre_hog == 1 & padre_hog == 2 ~ 2,  # Solo madre
    madre_hog == 2 & padre_hog == 1 ~ 3,  # Solo padre
    madre_hog == 1 & padre_hog == 1 ~ 4   # Ambos
  ))

table(poblacion_2024_1$padre_madre)


poblacion_2024_2 <- poblacion_2024_1 %>%
  filter(edad >= 15 & edad <= 24, !is.na(asis_esc)) %>%
  mutate(
    asis_esc = factor(asis_esc, levels = c(1, 2), labels = c("Asiste", "No asiste")),
    padre_madre = factor(padre_madre, levels = c(1, 2, 3, 4), labels = c("Ninguno", "Solo madre", "Solo padre", "Ambos"))
  )




# Preparar los datos con conteo y proporciones
datos_graf <- poblacion_2024_1 %>%
  filter(edad >= 15 & edad <= 24, !is.na(asis_esc)) %>%
  mutate(
    asis_esc = factor(asis_esc, levels = c(1, 2), labels = c("Asiste", "No asiste")),
    padre_madre = factor(padre_madre, levels = c(1, 2, 3, 4), 
                         labels = c("Ninguno", "Solo madre", "Solo padre", "Ambos"))
  ) %>%
  group_by(padre_madre, edad, asis_esc) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(padre_madre, edad) %>%
  mutate(
    prop = n / sum(n),
    label = paste0(round(prop * 100), "%")
  )

# Graficar con porcentajes dentro de las barras
ggplot(datos_graf, aes(x = as.factor(edad), y = prop, fill = asis_esc)) +
  geom_bar(stat = "identity", position = "fill", color = "black", size = 1) +
  geom_text(aes(label = label), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~padre_madre) +
  labs(
    title = "Proporción de asistencia escolar por edad (15-24 años) según presencia de padres",
    x = "Edad",
    y = "Porcentaje",
    fill = "Asistencia escolar"
  ) +
  theme_classic()
