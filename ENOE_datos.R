#-----------------------------------
#----------Librerias----------------
#-----------------------------------

{
  library(readr) #abrir csv
  library(haven) #abrir dta
  library(tidyverse)
  library(scales)  # Para label_comma()
  library(gt)
  
}

#-----------------------------------
#--------------Datos----------------
#-----------------------------------

{
  SDEM_2018 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestría BUAP - Economía/0. TESIS/Datos/ENOE_datos/2018trim1_csv/SDEMT118.csv", header=T)
  SDEM_2019 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestría BUAP - Economía/0. TESIS/Datos/ENOE_datos/2019trim1_csv/sdemt119.csv")
  SDEM_2020 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestría BUAP - Economía/0. TESIS/Datos/ENOE_datos/2020trim1_csv/ENOE_SDEMT120.csv")
  SDEM_2021 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestría BUAP - Economía/0. TESIS/Datos/ENOE_datos/enoe_n_2021_trim1_csv/ENOEN_SDEMT121.csv")
  SDEM_2022 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestría BUAP - Economía/0. TESIS/Datos/ENOE_datos/enoe_n_2022_trim1_csv/ENOEN_SDEMT122.csv")
  SDEM_2023 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestría BUAP - Economía/0. TESIS/Datos/ENOE_datos/enoe_2023_trim1_csv/ENOE_SDEMT123.csv")
  SDEM_2024 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestría BUAP - Economía/0. TESIS/Datos/ENOE_datos/enoe_2024_trim1_csv/ENOE_SDEMT124.csv")
  
}

# Crear la tabla de correspondencia
niveles_escolaridad <- tibble(
  cs_p13_1 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 99),
  nivel_escolaridad = c(
    "Ninguno", "Preescolar", "Primaria", "Secundaria", 
    "Preparatoria o bachillerato", "Normal", "Carrera técnica", 
    "Profesional", "Maestría", "Doctorado", "No sabe nivel"
  )
)


# Agrupar, sumar y unir con etiquetas
Escolaridad_2018 <- SDEM_2018 %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2019 <- SDEM_2019 %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2020 <- SDEM_2020 %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2021 <- SDEM_2021 %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2022 <- SDEM_2022 %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2023 <- SDEM_2023 %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2024 <- SDEM_2024 %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

{
# Añadir columna de año a cada data frame
Escolaridad_2018$anio <- 2018
Escolaridad_2019$anio <- 2019
Escolaridad_2020$anio <- 2020
Escolaridad_2021$anio <- 2021
Escolaridad_2022$anio <- 2022
Escolaridad_2023$anio <- 2023
Escolaridad_2024$anio <- 2024

# Unir todos los años en un solo data frame
Escolaridad_todos <- bind_rows(
  Escolaridad_2018,
  Escolaridad_2019,
  Escolaridad_2020,
  Escolaridad_2021,
  Escolaridad_2022,
  Escolaridad_2023,
  Escolaridad_2024
)
}


# Filtrar NA y "No sabe nivel"
Escolaridad_filtrada <- Escolaridad_todos %>%
  filter(!is.na(nivel_escolaridad), nivel_escolaridad != "No sabe nivel")

# Reordenar los niveles
Escolaridad_filtrada$nivel_escolaridad <- factor(
  Escolaridad_filtrada$nivel_escolaridad,
  levels = c(
    "Ninguno", "Preescolar", "Primaria", "Secundaria",
    "Preparatoria o bachillerato", "Normal", "Carrera técnica",
    "Profesional", "Maestría", "Doctorado"
  )
)

# Crear gráfico con formato de miles

ggplot(data = Escolaridad_filtrada, aes(x = nivel_escolaridad, y = proporcion, fill = factor(anio))) +
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proporción por nivel de escolaridad (2018–2024)",
    x = "Nivel de escolaridad",
    y = "Proporción (%)",
    fill = "Año"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )

Escolaridad_filtrada %>%
  filter(nivel_escolaridad %in% c("Preparatoria o bachillerato", "Profesional", "Carrera técnica")) %>%
  ggplot(aes(x = nivel_escolaridad, y = proporcion, fill = factor(anio))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proporción por nivel de escolaridad (2018–2024)",
    x = "Nivel de escolaridad",
    y = "Proporción (%)",
    fill = "Año"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )



# Calcular el cambio porcentual por nivel y año
variaciones_porcentuales <- Escolaridad_todos %>%
  filter(!is.na(nivel_escolaridad), nivel_escolaridad != "No sabe nivel") %>%
  arrange(nivel_escolaridad, anio) %>%
  group_by(nivel_escolaridad) %>%
  mutate(
    variacion_pct = (proporcion - lag(proporcion)) / lag(proporcion) * 100
  ) %>%
  ungroup()


# Graficar la variación porcentual
{
ggplot(
  variaciones_porcentuales %>% filter(!is.na(variacion_pct)),
  aes(x = anio, y = variacion_pct, color = nivel_escolaridad)
) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Cambio porcentual anual por nivel de escolaridad (2019–2024)",
    x = "Año",
    y = "Variación porcentual (%)",
    color = "Nivel educativo"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggplot(
  variaciones_porcentuales %>%
    filter(
      !is.na(variacion_pct),
      nivel_escolaridad %in% c("Preparatoria o bachillerato", "Profesional", "Carrera técnica")
    ),
  aes(x = anio, y = variacion_pct, color = nivel_escolaridad)
) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Cambio porcentual anual por nivel de escolaridad (2019–2024)",
    x = "Año",
    y = "Variación porcentual (%)",
    color = "Nivel educativo"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

}

ggplot(
  variaciones_porcentuales %>% filter(!is.na(variacion_pct)),
  aes(x = anio, y = variacion_pct)
) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_point(color = "#2c3e50") +
  facet_wrap(~ nivel_escolaridad, scales = "free_y") +
  labs(
    title = "Cambio porcentual anual por nivel de escolaridad (2019–2024)",
    x = "Año",
    y = "Variación porcentual (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )



# Transformar a tabla wide (horizontal)
tabla_horizontal <- variaciones_porcentuales %>%
  filter(!is.na(variacion_pct), nivel_escolaridad != "No sabe nivel") %>%
  mutate(variacion_pct = round(variacion_pct, 2)) %>%
  select(nivel_escolaridad, anio, variacion_pct) %>%
  pivot_wider(names_from = anio, values_from = variacion_pct)

# Detectar las columnas numéricas (años)
columnas_numericas <- names(tabla_horizontal)[sapply(tabla_horizontal, is.numeric)]

# Mostrar tabla con formato bonito
tabla_horizontal %>%
  gt() %>%
  tab_header(
    title = "Variación porcentual interanual por nivel de escolaridad",
    subtitle = "De 2019 a 2024 (comparado con el año anterior)"
  ) %>%
  fmt_number(
    columns = all_of(columnas_numericas),
    decimals = 2
  ) %>%
  cols_label(
    nivel_escolaridad = "Nivel de escolaridad"
  ) %>%
  tab_options(
    table.font.size = "small",
    heading.align = "left"
  )


#ASISTENCIA ESCOLAR 

Asistencia_2018 <- SDEM_2018 %>%
  filter(eda>=15 & eda<=24) %>% 
  group_by(cs_p17) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) 

Asistencia_2019 <- SDEM_2019 %>%
  filter(eda>=15 & eda<=24) %>% 
  group_by(cs_p17) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) 

Asistencia_2020 <- SDEM_2020 %>%
  filter(eda>=15 & eda<=24) %>% 
  group_by(cs_p17) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) 

Asistencia_2021 <- SDEM_2021 %>%
  filter(eda>=15 & eda<=24) %>% 
  group_by(cs_p17) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) 

Asistencia_2022 <- SDEM_2022 %>%
  filter(eda>=15 & eda<=24) %>% 
  group_by(cs_p17) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) 

Asistencia_2023 <- SDEM_2023 %>%
  filter(eda>=15 & eda<=24) %>% 
  group_by(cs_p17) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) 

Asistencia_2024 <- SDEM_2024 %>%
  filter(eda>=15 & eda<=24) %>% 
  group_by(cs_p17) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) 

Asistencia_2018$anio <- 2018
Asistencia_2019$anio <- 2019
Asistencia_2020$anio <- 2020
Asistencia_2021$anio <- 2021
Asistencia_2022$anio <- 2022
Asistencia_2023$anio <- 2023
Asistencia_2024$anio <- 2024

# Unir todos los años en un solo data frame
Asistencia_todos <- bind_rows(
  Asistencia_2018,
  Asistencia_2019,
  Asistencia_2020,
  Asistencia_2021,
  Asistencia_2022,
  Asistencia_2023,
  Asistencia_2024
)


Asistencia_todos <- Asistencia_todos |>
  mutate(
    cs_p17_rotulo = recode(cs_p17,
                           `1` = "Sí",
                           `2` = "No",
                           `9` = "No sabe")
  )



  ggplot(data=Asistencia_todos, aes(x = cs_p17_rotulo, y = suma_factor_expansion, fill = factor(anio))) +
  geom_col(position = "dodge", color="black") +
  labs(
    title = "Asistencia por año y tipo de respuesta",
    x = "Respuesta",
    y = "Total ponderado",
    fill = "Año"
  ) +
  theme_classic()

  library(ggplot2)
  library(dplyr)
  library(scales)
  
  Asistencia_todos |>
    filter(cs_p17 != 9) |>
    mutate(
      cs_p17_rotulo = recode(cs_p17,
                             `1` = "Sí",
                             `2` = "No")
    ) |>
    ggplot(aes(x = cs_p17_rotulo, y = suma_factor_expansion, fill = factor(anio))) +
    geom_col(position = "dodge", color = "black", size=1) +
    labs(
      title = "Asistencia por año y tipo de respuesta",
      x = "Respuesta",
      y = "Total ponderado",
      fill = "Año"
    ) +
    scale_y_continuous(labels = label_comma()) +
    theme_classic()
  






