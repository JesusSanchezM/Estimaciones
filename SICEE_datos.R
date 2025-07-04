
library(sf)
library(readr)
library(readxl)
library(tidyverse)


Datos_juntos <- read_excel("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/SICEE/Datos juntos.xlsx")


library(sf)
library(readr)

# Asegúrate que Entidad esté en mayúsculas y sin tildes, como en los shapefiles
datos_juntos <- Datos_juntos |>
  mutate(entidad = toupper(Entidad))  # renombrar si es necesario

# Leer archivo .shp del mapa de México (puedes ajustar la ruta)
mexico_mapa <- sf::st_read("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/SICEE/CONTINUO_NACIONAL_E")

# Unificar clave de enlace
mexico_mapa <- mexico_mapa |>
  mutate(entidad = toupper(NOM_ENT))  # columna puede ser NOMBRE, NOM_ENT o similar


# Unir los datos con el mapa
mapa_completo <- mexico_mapa |>
  left_join(datos_juntos, by = "entidad")


ggplot(data = mapa_completo) +
  geom_sf(aes(fill = `Escuelas Media Superior`), color = "white") +
  scale_fill_viridis_c(option = "C", name = "Media Superior") +
  labs(
    title = "Número de Escuelas de Media Superior por Estado",
    caption = "Fuente: datos_juntos"
  ) +
  theme_void()

library(ggplot2)
library(sf)
library(dplyr)

# Transformar datos a formato largo (tidy)
mapa_largo <- mapa_completo %>%
  pivot_longer(
    cols = c("Escuelas Media Superior", "Escuelas Superior"),
    names_to = "Tipo_Escuela",
    values_to = "Cantidad"
  )

# Mapa con facetas
ggplot(mapa_largo) +
  geom_sf(aes(fill = Cantidad), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Número de escuelas",
    direction = -1  # Invertir colores si lo prefieres
  ) +
  facet_wrap(~ Tipo_Escuela, ncol = 2) +  # Dos columnas para comparar
  labs(
    #title = "Mapa 1: Distribución de Escuelas por Nivel Educativo",
    #subtitle ="(Número de escuelas)",
    #caption = "Fuente: Elaboración propia con datos del \nSistema Interactivo de Consulta de Estadística Educativa") +
  )+
  theme_void() +
  theme(    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
            legend.position = "bottom",
            legend.title = element_text(face = "bold")
            
  )














