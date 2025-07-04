library(sf)
library(readr)
library(readxl)
library(tidyverse)


Datos_gini <- read_excel("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/DatamunDB (1).xlsx")


# Asegúrate que Entidad esté en mayúsculas y sin tildes, como en los shapefiles
Datos_gini <- Datos_gini |>
  mutate(MUNICIPIO = toupper(NOMBRE_MUNICIPIO))  # renombrar si es necesario

# Leer archivo .shp del mapa de México (puedes ajustar la ruta)
mexico_mapa_mun <- sf::st_read("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/mun22gw/mun22gw.shp")

# Unificar clave de enlace
mexico_mapa_mun <- mexico_mapa_mun |>
  mutate(MUNICIPIO = toupper(NOMGEO))  # columna puede ser NOMBRE, NOM_ENT o similar


mexico_mapa_mun_puebla <- mexico_mapa_mun %>% filter(NOM_ENT == "Puebla")
Datos_gini_puebla <- Datos_gini %>%  filter(NOMBRE_ENTIDAD =="Puebla")

# Unir los datos con el mapa
mapa_completo_mun <- mexico_mapa_mun_puebla |>
  left_join(Datos_gini_puebla, by = "MUNICIPIO")



mapa_completo_mun$VALOR_NUMERICO <- as.numeric(mapa_completo_mun$VALOR_NUMERICO)



ggplot(data = mapa_completo_mun) +
  geom_sf(aes(fill = `VALOR_NUMERICO`), color = "white") +
  scale_fill_viridis_c(option = "", name = "Coeficiente de GINI") +
  labs(
    title = "Indice de GINI",
    caption = "Fuente: Elaboración propia con datos del CONEVAL"
  ) +
  theme_void()


ggplot(data = mapa_completo_mun) +
  geom_sf(aes(fill = `VALOR_NUMERICO`), color = "white") +
  scale_fill_gradient(name = "Coeficiente de GINI", 
                      low = "#00ff2e", high = "#ff0000") +
  labs(
    title = "Coeficiente de GINI",
    caption = "Fuente: Elaboración propia con datos del CONEVAL"
  ) +
  theme_void()



#IGNORAR ESTA PARTE 


# Ver cuántos nombres están repetidos
duplicados <- mapa_completo_mun$NOMBRE_MUNICIPIO[duplicated(mapa_completo_mun$NOMBRE_MUNICIPIO)]

# Mostrar los nombres repetidos únicos (sin repetir la repetición)
nombres_duplicados <- unique(duplicados)

# Mostrar cuántas veces se repite cada uno
tabla_repetidos <- table(mapa_completo_mun$NOMBRE_MUNICIPIO)
repetidos_conteo <- tabla_repetidos[tabla_repetidos > 1]

# Ver resultados
print(nombres_duplicados)      # Nombres duplicados
print(repetidos_conteo)        # Cuántas veces se repiten






# Municipio con GINI máximo
max_gini <- mapa_completo_mun %>% 
  filter(VALOR_NUMERICO == max(VALOR_NUMERICO, na.rm = TRUE))

# Municipio con GINI mínimo
min_gini <- mapa_completo_mun %>% 
  filter(VALOR_NUMERICO == min(VALOR_NUMERICO, na.rm = TRUE, 1))


library(ggrepel)  # para que no se encimen las etiquetas

ggplot(data = mapa_completo_mun) +
  geom_sf(aes(fill = `VALOR_NUMERICO`), color = "white") +
  scale_fill_gradient(name = "Coeficiente de GINI", 
                      low = "#00ff2e", high = "#ff0000") +
  geom_sf_text(data = max_gini, aes(label = MUNICIPIO), 
               color = "black", size = 1, fontface = "bold") +
  geom_sf_text(data = min_gini, aes(label = MUNICIPIO), 
               color = "blue", size = 1, fontface = "bold") +
  labs(
    title = "Coeficiente de GINI en los municipios de Puebla",
    subtitle = paste("Máximo:", max_gini$MUNICIPIO, "| Mínimo:", min_gini$MUNICIPIO),
    caption = "Fuente: Elaboración propia con datos del CONEVAL"
  ) +
  theme_void()


library(ggplot2)
library(sf)
library(dplyr)
library(patchwork)

# Encontrar municipios con GINI máximo y mínimo
max_gini <- mapa_completo_mun %>% filter(VALOR_NUMERICO == max(VALOR_NUMERICO, na.rm = TRUE))
min_gini <- mapa_completo_mun %>% filter(VALOR_NUMERICO == min(VALOR_NUMERICO, na.rm = TRUE))

# Crear mapa resaltando contornos
mapa_gini <- ggplot(data = mapa_completo_mun) +
  geom_sf(aes(fill = `VALOR_NUMERICO`), color = "white") +
  geom_sf(data = max_gini, fill = NA, color = "black", size = 1.2) +
  geom_sf(data = min_gini, fill = NA, color = "blue", size = 1.2) +
  scale_fill_gradient(name = "Coeficiente de GINI", 
                      low = "#00ff2e", high = "#ff0000") +
  labs(
    title = "Coeficiente de GINI en los municipios de Puebla"
  ) +
  theme_void()

# Agregar columna de tipo a cada conjunto
max_gini_tabla <- max_gini %>%
  st_drop_geometry() %>%
  mutate(Tipo = "Máximo") %>%
  select(Tipo, Municipio = MUNICIPIO, GINI = VALOR_NUMERICO)

min_gini_tabla <- min_gini %>%
  st_drop_geometry() %>%
  mutate(Tipo = "Mínimo") %>%
  select(Tipo, Municipio = MUNICIPIO, GINI = VALOR_NUMERICO)

# Unir ambos
tabla_municipios <- bind_rows(max_gini_tabla, min_gini_tabla)


# Redondear GINI
tabla_municipios <- tabla_municipios %>%
  mutate(GINI = round(GINI, 3))


tabla_plot <- ggplot(tabla_municipios, aes(x = reorder(Municipio, GINI), y = GINI, fill = Tipo)) +
  geom_col(col="black", size=1) +
  coord_flip() +
  labs(title = "Municipios extremos del GINI",
       caption = "Fuente: Elaboración propia con datos del CONEVAL") +
  scale_fill_manual(values = c("Máximo" = "#ff0000", "Mínimo" = "#00ff2e")) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )


# Combinar mapa y tabla
mapa_gini + tabla_plot + plot_layout(ncol = 1, heights = c(3, 1))




















