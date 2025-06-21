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
# Crear datos largos con error estándar
datos <- tibble::tibble(
  país = rep(c("México", "OCDE"), each = 3 * 19),
  año = rep(rep(2005:2023, each = 3), times = 2),
  nivel = rep(c(
    "Below upper secondary education",
    "Upper secondary or post-secondary non-tertiary education",
    "Tertiary education"
  ), times = 19 * 2),
  valor = c(
    # México
    71.8, 15.4, 12.7,
    71.0, 15.8, 13.3,
    70.4, 16.0, 13.6,
    70.1, 16.2, 13.7,
    68.7, 17.0, 14.3,
    67.9, 17.4, 14.7,
    66.9, 18.1, 15.0,
    66.0, 18.3, 15.7,
    65.2, 18.6, 16.2,
    64.9, 19.1, 16.0,
    64.3, 19.4, 16.3,
    63.4, 19.8, 16.8,
    62.3, 20.2, 17.4,
    60.9, 21.2, 18.0,
    60.2, 21.5, 18.3,
    58.4, 22.2, 19.4,
    57.2, 22.4, 20.5,
    56.2, 23.1, 20.6,
    55.6, 23.5, 21.0,
    # OCDE
    30.0, 44.2, 26.3,
    29.3, 44.7, 26.5,
    28.5, 44.2, 27.6,
    27.7, 44.1, 28.6,
    27.1, 43.9, 29.3,
    27.0, 43.5, 29.9,
    26.8, 43.3, 30.4,
    25.5, 43.2, 31.8,
    25.0, 43.1, 32.3,
    24.3, 42.4, 33.6,
    24.2, 42.2, 34.0,
    23.0, 42.2, 35.3,
    22.8, 41.8, 35.8,
    22.0, 41.5, 36.9,
    21.4, 41.1, 37.9,
    19.8, 41.4, 39.2,
    19.9, 40.5, 40.0,
    19.5, 40.2, 40.7,
    19.2, 40.2, 41.0
  )
)
}

datos <- datos |>
  mutate(
    nivel = factor(
      nivel,
      levels = c(
        "Tertiary education",
        "Upper secondary or post-secondary non-tertiary education",
        "Below upper secondary education"
      )
    )
  )


tema_grafico <- theme_classic(base_size = 13) +
  theme(legend.position = "top")


etiquetas <- geom_text(
  aes(label = sprintf("%.2f", valor)),
  position = position_dodge(width = 0.8),
  vjust = -0.5, size = 3.5
)


datos |> 
  filter(país == "México") |>
  ggplot(aes(x = factor(año), y = valor, fill = nivel)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9),
           color="black", size=1) +
  etiquetas +
  labs(
    title = "Distribución educativa en México con error estándar (2018–2023)",
    x = "Año", y = "Porcentaje", fill = "Nivel educativo"
  ) +
  tema_grafico



datos_pos <- datos |>
  group_by(país, año) |>
  arrange(país, año, desc(nivel)) |>
  mutate(
    pos = cumsum(valor) - valor / 2,
    etiqueta = sprintf("%.1f%%", valor)
  )


datos_pos |> 
  filter(país == "México") |>
  ggplot(aes(x = factor(año), y = valor, fill = nivel)) +
  geom_bar(stat = "identity", color="black", size=1) +
  geom_text(aes(y = pos, label = etiqueta), color = "white", size = 2.5) +
  labs(
    #title = "Gráfica 4: Distribución apilada del nivel educativo en México (2005–2023)",
    #subtitle = "Porcentaje de población por nivel de educación alcanzado",
    x = "Año", 
    y = "Porcentaje (%)", 
    fill = "Nivel educativo",
    #caption = "Fuente: Elaboración propia con datos de la OCDE. Datos extraídos el 25/05/2025\nNota: Porcentajes calculados sobre población de 25-64 años"
  )  +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = label_percent(scale = 1))



datos_pos |> 
  filter(país == "OCDE") |>
  ggplot(aes(x = factor(año), y = valor, fill = nivel)) +
  geom_bar(stat = "identity", color="black", size=1) +
  geom_text(aes(y = pos, label = etiqueta), color = "white", size = 2.5) +
  labs(
    title = "Distribución apilada del nivel educativo en países OCDE (2005–2023)",
    subtitle = "Porcentaje de población por nivel de educación alcanzado",
    x = "Año", 
    y = "Porcentaje total", 
    fill = "Nivel educativo",
    caption = "Fuente: OCDE. Datos extraídos el 25/05/2025\nNota: Porcentajes calculados sobre población de 25-64 años"
  )  +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 20))


ggplot(datos_pos, aes(x = factor(año), y = valor, fill = nivel)) +
  geom_bar(stat = "identity", color="black", size=1) + 
  facet_wrap(~ país) +
  labs(
    #title = "Gráfica 5: Distribución educativa apilada: México vs OCDE (2005–2023)",
    x = "Año", y = "Porcentaje (%)", fill = "Nivel educativo",
    #subtitle = "Porcentaje de población por nivel de educación alcanzado",
    #caption = "Fuente: Elaboración propia con datos de la OCDE. Datos extraídos el 25/05/2025\nNota: Porcentajes calculados sobre población de 25-64 años"
  ) +
  theme_classic() +
  theme(legend.position = "top")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12))+ 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = label_percent(scale = 1))



#DATOS MUNDIALES PARA COMPARATIVA 


Ocde_world_2023 <- read_excel("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/OCDE data/Educational attainment distribution 2023 World.xlsx", 
                                                             sheet = "Table", range = "B9:e56")
#View(Ocde_world_2023)

Ocde_world_2023_filtrado <- Ocde_world_2023 %>%
  filter(`Reference area` != "Non-OECD economies")


orden_paises <- Ocde_world_2023_filtrado %>%
  arrange((`Below upper secondary education`)) %>%
  pull(`Reference area`)


Ocde_world_2023_long <- Ocde_world_2023_filtrado %>%
  pivot_longer(
    cols = c(
      "Below upper secondary education",
      "Upper secondary or post-secondary non-tertiary education",
      "Tertiary education"
    ),
    names_to = "nivel",
    values_to = "porcentaje"
  ) %>%
  mutate(
    `Reference area` = factor(`Reference area`, levels = orden_paises),
    nivel = factor(nivel, levels = c(
      "Tertiary education",
      "Upper secondary or post-secondary non-tertiary education",
       "Below upper secondary education"
    ))
  )



ggplot(Ocde_world_2023_long, aes(x = `Reference area`, y = porcentaje, fill = nivel)) +
  geom_col(color = "black", size = 1) +
  # Resaltar la barra de OCDE con un borde más grueso
  geom_col(
    data = subset(Ocde_world_2023_long, `Reference area` == "OECD"),
    color = "#FFD700",  # Color del borde
    size = 1.5,     # Grosor del borde
    show.legend = FALSE
  ) +
  geom_col(
    data = subset(Ocde_world_2023_long, `Reference area` == "Mexico"),
    color = "#FFD700",  # Color del borde
    size = 1.5,     # Grosor del borde
    show.legend = FALSE
  )+
  labs(
    #title = "Gráfica 6: Nivel educativo en países OCDE y seleccionados (2023)",    
    #subtitle = "Porcentaje de población por nivel de educación alcanzado",
    #caption = "Fuente: Elaboración propia con datos de la OCDE. Datos extraídos el 25/05/2025\nNota: Porcentajes calculados sobre población de 25-64 años",
    x = "País",
    y = "Porcentaje (%)",
    fill = "Nivel educativo"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
    
  ) +
  scale_fill_manual(
    values = c(
      "Below upper secondary education" = "#FF69B4",
      "Upper secondary or post-secondary non-tertiary education" = "#00BFFF",
      "Tertiary education" = "mediumpurple"
    )
  )  +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = label_percent(scale = 1))


Ocde_bank_world_2023 <- read_excel("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/OCDE data/Educational attainment distribution 2023 World.xlsx", 
                              sheet = "Table", range = "B9:f56")
#View(Ocde_bank_world_2023)

# Cargar librerías necesarias
library(GGally)
library(ggplot2)

# 2. Scatter plot con línea de tendencia
ggplot(Ocde_bank_world_2023, 
       aes(x = `Tertiary education`, y = `GDP Per capita`)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    #title = "Gráfica 7: Relación entre educación terciaria y PIB per cápita",
       #subtitle = "A precios constantes de 2015 en dolares ", 
       x = "Porcentaje con educación terciaria",
       y = "PIB per cápita (USD)",
       #caption = "Fuente: Elaboración propia con datos de la OCDE y World Bank Group.\nDatos extraídos el 25/05/2025")
  )+
       theme_classic() +
  # Añadir etiquetas a los puntos extremos
  ggrepel::geom_text_repel(
    data = subset(Ocde_bank_world_2023, 
                  `Below upper secondary education` > 40 | `GDP Per capita` > 60000),
    aes(label = `Reference area`),
    box.padding = 0.5,
    max.overlaps = Inf
  ) +
  scale_y_continuous(labels = scales::dollar_format())+ 
  theme(    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
            
  )
















# Primero crear la variable educ_group correctamente
Ocde_bank_world_2023 <- Ocde_bank_world_2023 %>%
  mutate(educ_group = cut(`Below upper secondary education`,
                          breaks = quantile(`Below upper secondary education`, 
                                            probs = seq(0, 1, 0.25),
                                            na.rm = TRUE),
                          labels = c("Q1 (Bajo)", "Q2", "Q3", "Q4 (Alto)"),
                          include.lowest = TRUE))



# Luego generar el gráfico
ggplot(Ocde_bank_world_2023, 
       aes(x = educ_group, y = `GDP Per capita`, fill = educ_group)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, color = "black", size = 2) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  labs(title = "Gráfica 7: Distribución del PIB per cápita por nivel de educación inferior",
       x = "Grupo según % de población con educación inferior",
       y = "PIB per cápita (USD)",
       caption = "Q1: Países con menor % de baja educación\nQ4: Países con mayor %") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_classic() +
  theme(legend.position = "none")























