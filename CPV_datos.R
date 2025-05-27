#-----------------------------------
#----------Librerias----------------
#-----------------------------------

{
  library(tidyverse)
  library(readxl)
}

#-----------------------------------
#--------------Datos----------------
#-----------------------------------

{
datos_escolarizacion <- data.frame(
  grupo_edad = c("3 a 5 años", "6 a 11 años", "12 a 14 años", "15 a 24 años"),
  tasa_escolarizacion = c(63.3, 95.5, 90.5, 45.3)
)
}

library(ggplot2)

# Primero preparo los datos en formato similar al que usas
datos_escolarizacion_long <- data.frame(
  Grupo = factor(c("3 a 5 años", "6 a 11 años", "12 a 14 años", "15 a 24 años"),
                 levels = c("3 a 5 años", "6 a 11 años", "12 a 14 años", "15 a 24 años")),
  Porcentaje = c(63.3, 95.5, 90.5, 45.3)
)

# Creo la gráfica con tu estilo estándar
ggplot(datos_escolarizacion_long, aes(x = Grupo, y = Porcentaje, fill = Grupo)) +
  geom_col(color = "black", size = 0.8, width = 0.7) +
  
  # Añadir etiquetas de porcentaje
  geom_text(aes(label = paste0(Porcentaje, "%")), 
            vjust = -0.5, size = 3.5, color = "black", fontface = "bold") +
  
  # Personalización al estilo de tu gráfico
  labs(
    title = "Gráfica 1: Porcentaje de la población de 3 a 24 años que asiste a la escuela por grupo de edad (2020)",
    subtitle = "Porcentaje",
    caption = "Fuente: Elaboración propia con datos de INEGI. Censo de Población y Vivienda 2020\nDatos procesados el 25/05/2025",
    x = "Grupo de edad",
    y = "Porcentaje escolarizado",
    fill = "Grupo de edad"
  ) +
  
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11)
  ) +
  
  # Escala de colores similar a tu gráfico OCDE
  scale_fill_manual(
    values = c(
      "3 a 5 años" = "#FF69B4",       # Rosa como "Below upper secondary"
      "6 a 11 años" = "#00BFFF",      # Azul como "Upper secondary"
      "12 a 14 años" = "mediumpurple", # Morado como "Tertiary"
      "15 a 24 años" = "#FFA07A"      # Salmón (nuevo color)
    )
  ) +
  
  # Ajustar el eje Y como en tu gráfico
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20))


