#-----------------------------------
#----------Librerias----------------
#-----------------------------------

{
  library(readr) #abrir csv
  library(haven) #abrir dta
  library(tidyverse)
  library(scales)  # Para label_comma()
  library(gt)
  library(forcats)
  
}

#-----------------------------------
#--------------Datos----------------
#-----------------------------------

{
  SDEM_2018 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENOE_datos/2018trim1_csv/SDEMT118.csv", header=T)
  SDEM_2019 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENOE_datos/2019trim1_csv/sdemt119.csv")
  SDEM_2020 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENOE_datos/2020trim1_csv/ENOE_SDEMT120.csv")
  SDEM_2021 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENOE_datos/enoe_n_2021_trim1_csv/ENOEN_SDEMT121.csv")
  SDEM_2022 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENOE_datos/enoe_n_2022_trim1_csv/ENOEN_SDEMT122.csv")
  SDEM_2023 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENOE_datos/enoe_2023_trim1_csv/ENOE_SDEMT123.csv")
  SDEM_2024 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENOE_datos/enoe_2024_trim1_csv/ENOE_SDEMT124.csv")
  
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
  filter(eda>=25) %>% 
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2019 <- SDEM_2019 %>%
  filter(eda>=25) %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2020 <- SDEM_2020 %>%
  filter(eda>=25) %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2021 <- SDEM_2021 %>%
  filter(eda>=25) %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2022 <- SDEM_2022 %>%
  filter(eda>=25) %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2023 <- SDEM_2023 %>%
  filter(eda>=25) %>%
  group_by(cs_p13_1) %>%
  summarise(suma_factor_expansion = sum(fac_tri, na.rm = TRUE)) %>%
  left_join(niveles_escolaridad, by = "cs_p13_1") %>%
  mutate(proporcion = suma_factor_expansion / sum(suma_factor_expansion, na.rm = TRUE)) %>%
  arrange(desc(suma_factor_expansion))

Escolaridad_2024 <- SDEM_2024 %>%
  filter(eda>=25) %>%
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

{
# Crear gráfico con formato de miles

Escolaridad_filtrada %>% 
  filter(nivel_escolaridad %in% c("Ninguno", "Preescolar","Primaria", "Secundaria","Preparatoria o bachillerato", "Carrera técnica", "Normal", "Profesional")) %>%
  ggplot(aes(x = nivel_escolaridad, y = proporcion, fill = factor(anio))) +
  geom_bar(stat = "identity", position = "dodge", color="black", size=1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Gráfica 2: Proporción por nivel de escolaridad (2018–2024)",
    x = "Nivel de escolaridad",
    y = "Proporción (%)",
    fill = "Año", 
    subtitle = "(Porcentaje)",
    caption = "Fuente: Elaboración propia con datos de INEGI. Encuesta de Ocupación y Empleo (ENOE) Datos extraídos el 25/05/2025\nNota: Porcentajes calculados sobre población mayor a 25 años"
    
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 






Escolaridad_filtrada %>%
  filter(nivel_escolaridad %in% c("Preparatoria o bachillerato", "Profesional", "Normal", "Carrera técnica")) %>%
  ggplot(aes(x = nivel_escolaridad, y = proporcion, fill = factor(anio))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size=1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Gráfica 3: Proporción por nivel de escolaridad (2018–2024)",
    x = "Nivel de escolaridad",
    y = "Proporción (%)",
    fill = "Año", 
    subtitle = "(Porcentaje)",
    caption = "Fuente: Elaboración propia con datos de INEGI. Encuesta de Ocupación y Empleo (ENOE) Datos extraídos el 25/05/2025\nNota: Porcentajes calculados sobre población mayor a 25 años"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) 



}


# Procesamiento de datos
Escolaridad_ordenada <- Escolaridad_filtrada %>%
  filter(!nivel_escolaridad %in% c("Maestría", "Doctorado", "Normal")) %>%
  mutate(grupo_escolaridad = case_when(
    nivel_escolaridad %in% c("Ninguno") ~ "1. Ninguno",
    nivel_escolaridad %in% c("Preescolar", "Primaria", "Secundaria") ~ "2. Menor a preparatoria",
    nivel_escolaridad %in% c("Preparatoria o bachillerato") ~ "3. Preparatoria y bachillerato",
    nivel_escolaridad %in% c("Carrera técnica", "Profesional") ~ "4. Carrera técnica y profesional",
    TRUE ~ as.character(nivel_escolaridad)
  )) %>%
    # Convertimos a factor ordenado
    mutate(grupo_escolaridad = fct_relevel(grupo_escolaridad, 
                                           "1. Ninguno",
                                           "2. Menor a preparatoria",
                                           "3. Preparatoria y bachillerato",
                                           "4. Carrera técnica y profesional")) %>%
    group_by(anio, grupo_escolaridad) %>%
    summarise(suma_total = sum(suma_factor_expansion),
              proporcion_total = sum(proporcion),
              .groups = 'drop')
  
  # Creamos el gráfico con el orden específico
  ggplot(Escolaridad_ordenada, aes(x = factor(anio), y = proporcion_total, 
                                   fill = grupo_escolaridad)) +
    geom_bar(stat = "identity", position = "stack", color="black", size=1) +
    labs(x = "Año",
         y = "Porcentaje (%)",
         fill = "Nivel de Escolaridad"
         #title = "Gráfica 2: Distribución de Niveles de Escolaridad por Año",
         #subtitle = "(Porcentaje de la población mayor a 25 años)",
         #caption = "Elaboración propia con datos de la Encuesta Nacional de Ocupación y Empleo (ENOE) \nNota: Se excluyen los datos de Maestría y Doctorado") +
    )+
         scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("#FF69B4", "#00BFFF", "mediumpurple", "#FFA07A")) +  # Colores distintivos
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) 
 



{
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
  geom_line(size = 2) +
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
}

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
    #title = "Asistencia por año y tipo de respuesta",
    x = "Respuesta",
    y = "Total ponderado",
    fill = "Año"
  ) +
  theme_classic()
  
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
      x = "Respuesta",
      y = "Total ponderado",
      fill = "Año",
      #title = "Gráfica 3: Asistencia por año y tipo de respuesta (2018–2024)",
      #subtitle = "(Población)",
      #caption = "Fuente: Elaboración propia con datos de INEGI. Encuesta de Ocupación y Empleo (ENOE) Datos extraídos el 25/05/2025\nNota: Porcentajes calculados sobre población de entre 15 y 24 años"
    ) +
    scale_y_continuous(labels = label_comma()) +
    theme_classic() +  
  theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) 







Asistencia_todos |>
  filter(cs_p17 != 9) |>
  mutate(
    cs_p17_rotulo = recode(cs_p17,
                           `1` = "Sí",
                           `2` = "No")
  ) |>
  group_by(anio) |>
  mutate(
    total_anio = sum(suma_factor_expansion),
    porcentaje = suma_factor_expansion / total_anio * 100
  ) |>
  ungroup() |>
  group_by(anio) |>
  arrange(cs_p17_rotulo) |>  # Para que las etiquetas estén en orden
  mutate(
    pos = cumsum(porcentaje) - 0.5 * porcentaje
  ) |>
  ggplot(aes(x = factor(anio), y = porcentaje, fill = cs_p17_rotulo)) +
  geom_col(color = "black", size = 1) +
  geom_text(aes(y = pos, label = paste0(round(porcentaje, 1), "%")),
            color = "black", size = 5) +
  labs(
    #title = "Asistencia escolar por año",
    #subtitle = "Distribución apilada por tipo de respuesta (2018–2024)",
    x = "Año", 
    y = "Porcentaje (%)", 
    fill = "Respuesta",
    #caption = "Fuente: Elaboración propia con datos de INEGI. Encuesta de Ocupación y Empleo (ENOE)\nNota: Población de entre 15 y 24 años. Datos extraídos el 25/05/2025"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.2, "cm"),  # Aumenta tamaño de recuadros
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = label_percent(scale = 1))


table(SDEM_2024$t_loc_men)
table(SDEM_2024$t_loc_tri)
table(SDEM_2024$ur)


SDEM_2024 %>% filter(cd_a==01) %>% pull(mun) %>%  table()

table(SDEM_2024$ur)

table(SDEM_2024$t_loc_tri)

table(SDEM_2024$cd_a)

SDEM_2024 %>%  filter(mun==21128)

table(SDEM_2024$mun)




