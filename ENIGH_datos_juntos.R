#Librerias

library(readr) #abrir csv
library(haven) #abrir dta
library(tidyverse)
library(scales)  # Para label_comma()
library(psych)

#Manejo de datos juntos 
{
datos_unidos_2018 <- datos_unidos_2018 %>%
  rename(ump_2018 = upm)
  
datos_unidos_2018 <- datos_unidos_2018 %>%
  rename(est_dis_2018 = est_dis)

datos_unidos_2018 <- datos_unidos_2018 %>%
  rename(factor_2018 = factor)

datos_unidos_2024 <- datos_unidos_2024 %>%
  rename(ump_2024 = upm.x)

datos_unidos_2024 <- datos_unidos_2024 %>%
  rename(est_dis_2024 = est_dis.x)

datos_unidos_2024 <- datos_unidos_2024 %>%
  rename(factor_2024 = factor.y)


datos_unidos <- bind_rows(
  datos_unidos_2018 %>% mutate(año = 2018),
  datos_unidos_2024 %>% mutate(año = 2024)
)


datos_unidos <- datos_unidos %>%
  filter(!is.na(asis_esc)) %>%
  mutate(
    asis_esc = factor(asis_esc, levels = c("Asiste", "No asiste"))
  )

table(datos_unidos$asis_esc)


datos_unidos <- datos_unidos %>%
  mutate(
    padre_madre = case_when(
      madre_hog == 2 & padre_hog == 2 ~ "Ninguno",
      madre_hog == 1 & padre_hog == 2 ~ "Solo madre",
      madre_hog == 2 & padre_hog == 1 ~ "Solo padre",
      madre_hog == 1 & padre_hog == 1 ~ "Ambos"
    )
  )
}

#Proporción de asistencia escolar
{
# Filtrar solo las columnas necesarias y rangos de edad
datos_graf <- datos_unidos %>%
  select(año, edad, asis_esc, factor.y) %>%
  filter(!is.na(asis_esc), edad >= 15 & edad <= 24) %>%
  mutate(
    asis_esc = as.character(asis_esc),
    asis_esc = case_when(
      asis_esc == "2" ~ "No asiste",
      asis_esc == "1" ~ "Asiste",
      TRUE ~ asis_esc),
    asis_esc = factor(asis_esc, levels = c("No asiste", "Asiste")),
  ) %>%
  group_by(año, edad, asis_esc) %>%
  summarise(total = sum(factor.y, na.rm = TRUE), .groups = "drop") %>%
  group_by(año, edad) %>%
  mutate(prop = total / sum(total))


ggplot(datos_graf, aes(x = as.factor(edad), y = prop, fill = asis_esc)) +
  geom_col(color = "black", size = 0.3) +
  geom_text(aes(label = sprintf("%.2f%%", prop*100)),  # dos dígitos
            position = position_fill(vjust = 0.5),
            size = 3.5, color = "black", angle=90) +
  facet_wrap(~año) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Edad",
    y = "Porcentaje",
    fill = "Asistencia escolar"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"   # Leyenda abajo
  ) 
}

#Presencia de padres 
{
datos_padres <- datos_unidos %>%
  group_by(año, padre_madre, edad, asis_esc) %>%
  summarise(total = sum(factor.y, na.rm = TRUE), .groups = "drop") %>%
  group_by(año, padre_madre, edad) %>%
  mutate(
    prop = total / sum(total),
    label = paste0(round(prop*100), "%"),
      asis_esc = as.character(asis_esc),
      asis_esc = case_when(
        asis_esc == "2" ~ "No asiste",
        asis_esc == "1" ~ "Asiste",
        TRUE ~ asis_esc),
      asis_esc = factor(asis_esc, levels = c("No asiste", "Asiste")),
    )
    

ggplot(datos_padres, aes(x = as.factor(edad), y = prop, fill = asis_esc)) +
  geom_bar(stat = "identity", position = "fill", color = "black", size = 0.3) +
  geom_text(aes(label = label), position = position_fill(vjust = 0.5), size = 3, color = "white") +
  facet_grid(padre_madre ~ año) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Edad",
    y = "Porcentaje",
    fill = "Asistencia escolar"
  ) +
  theme_classic()

}

#Sexo
{
# Unir datos y preparar
df_combinado <- bind_rows(
  datos_unidos_2018 %>%
    group_by(sexo, edad, asis_esc) %>%
    summarise(total_personas = sum(factor, na.rm = TRUE), .groups = "drop") %>%
    mutate(año = 2018),
  
  datos_unidos_2024 %>%
    group_by(sexo, edad, asis_esc) %>%
    summarise(total_personas = sum(factor.y, na.rm = TRUE), .groups = "drop") %>%
    mutate(año = 2024)
) %>% 
  filter(!is.na(asis_esc))

# Convertir a caracter y luego a factor
df_combinado <- df_combinado %>%
  mutate(
    asis_esc = as.character(asis_esc),
    asis_esc = case_when(
      asis_esc == "1" ~ "Asiste",
      asis_esc == "2" ~ "No asiste",
      TRUE ~ asis_esc
    ),
    sexo = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer",
      TRUE ~ as.character(sexo)
    )
  ) %>%
  filter(asis_esc %in% c("Asiste", "No asiste")) %>%
  group_by(año, sexo, edad) %>%
  mutate(
    prop = total_personas / sum(total_personas),
    porcentaje = round(prop * 100, 1),
    etiqueta = paste0(porcentaje, "%"),
    asis_esc = factor(asis_esc, levels = c("No asiste", "Asiste")),
    sexo = factor(sexo)
  )

# Verificar la estructura de los datos
print(head(df_combinado))
print(levels(df_combinado$asis_esc))


# Gráfico unificado con ajustes
ggplot(df_combinado,
       aes(x = interaction(edad, año, sep = " -" , lex.order = TRUE), 
           y = prop, fill = asis_esc)) +
  geom_col(color = "black", size = 0.3) +
  geom_text(aes(label = ifelse(porcentaje > 5, etiqueta, "")),
            position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0.5,  # Texto horizontal centrado
            size = 3.5, color = "black") +
  labs(x = "Edad (Año)", y = "Proporción", fill = "Asistencia escolar",
       title = "Asistencia escolar por edad y año") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"   # Leyenda abajo
  ) +
  facet_wrap(~sexo)
}

#Total integrantes
{
  library(dplyr)
  library(ggplot2)
  
  # Preparar datos 2024
  datos_unidos_2024 <- datos_unidos_2024 %>%
    mutate(
      tot_integ_grp = ifelse(tot_integ > 9, "10 o más", as.character(tot_integ)),
      tot_integ_grp = factor(tot_integ_grp, levels = c(as.character(1:9), "10 o más")),
      año = 2024,
      factor_exp = factor.y  # renombramos la variable para unificar
    )
  
  # Preparar datos 2018
  datos_unidos_2018 <- datos_unidos_2018 %>%
    mutate(
      tot_integ_grp = ifelse(tot_integ > 9, "10 o más", as.character(tot_integ)),
      tot_integ_grp = factor(tot_integ_grp, levels = c(as.character(1:9), "10 o más")),
      año = 2018,
      factor_exp = factor   # renombramos para unificar con 2024
    )
  
  # Unir ambas bases
  datos_combinados <- bind_rows(datos_unidos_2018, datos_unidos_2024)
  
  
  # Crear el correlograma de asistencia escolar
  datos_combinados %>%
    # Filtrar solo los que asisten o no asisten (excluir NA si es necesario)
    filter(!is.na(asis_esc)) %>%
    # Agrupar por las variables relevantes
    group_by(año, tot_integ_grp, edad) %>%
    summarise(
      total_personas = sum(factor_exp, na.rm = TRUE),
      asistentes = sum(factor_exp[asis_esc == "Asiste"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calcular porcentaje de asistencia
    mutate(
      porcentaje_asistencia = round((asistentes / total_personas) * 100, 1)
    ) %>%
    # Crear el correlograma
    ggplot(aes(x = as.factor(tot_integ_grp), y = as.factor(edad))) +
    geom_tile(aes(fill = porcentaje_asistencia), color = "black" ) +
    geom_text(aes(label = paste0(porcentaje_asistencia, "%")), 
              color = "black", size = 3) +
    facet_wrap(~ año, ncol = 2) +
    scale_fill_gradient2(low = "red", high = "green", 
                         midpoint = 50, name = "% Asistencia") +
    labs(
      x = "Número de integrantes del hogar",
      y = "Edad"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank())
  
  # Gráfico de porcentaje apilado por tamaño de hogar y año
  datos_combinados %>%
    group_by(año, tot_integ_grp, asis_esc) %>%
    summarise(total_personas = sum(factor_exp, na.rm = TRUE), .groups = "drop") %>%
    group_by(año, tot_integ_grp) %>%
    mutate(porcentaje = 100 * total_personas / sum(total_personas),
           etiqueta = paste0(round(porcentaje, 1), "%")) %>%
    ggplot(aes(x = tot_integ_grp, y = porcentaje, fill = asis_esc)) +
    geom_col(color = "black") +
    geom_text(aes(label = etiqueta), 
              position = position_stack(vjust = 0.5), size = 3) +
    facet_wrap(~ año) +  # separa por año
    labs(x = "Integrantes del hogar", y = "Porcentaje", 
         fill = "Asistencia escolar", title = "Distribución de asistencia por tamaño de hogar y año") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
}

#Ingreso
{
  # 1. Unir bases
  datos_var2 <- d %>%
    rename(total_2018 = total_poblacion) %>%
    inner_join(
      a %>% rename(total_2024 = total_poblacion),
      by = c("padre_madre", "edad")
    ) %>%
    # 2. Calcular variación porcentual
    mutate(
      variacion_pct = round(((total_2024 - total_2018) / total_2018) * 100, 1)
    )
  
  datos_var2_filtro <- datos_var2 %>%
    filter(total_2018 >= 2000)
  
  
  # 3. Crear mapa de calor
  ggplot(datos_var2_filtro, aes(x = padre_madre, y = factor(edad))) +
    geom_tile(aes(fill = variacion_pct), color = "black") +
    geom_text(aes(label = paste0(variacion_pct, "%")), 
              color = "black", size = 3) +
    scale_fill_gradient2(
      low = "red", high = "green", mid = "white",
      midpoint = 0, name = "Variación %"
    ) +
    labs(
      x = "Condición de padres",
      y = "Edad (15–24 años)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  library(dplyr)
  library(ggplot2)
  
  # Agrupar edades en dos bloques
  datos_var_agrup <- datos_var2 %>%
    mutate(grupo_edad = ifelse(edad >= 15 & edad <= 19, "15-19", "20-24")) %>%
    group_by(padre_madre, grupo_edad) %>%
    summarise(
      total_2018 = sum(total_2018, na.rm = TRUE),
      total_2024 = sum(total_2024, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      variacion_pct = round(((total_2024 - total_2018) / total_2018) * 100, 1)
    )
  
  # Mapa de calor con grupos de edad
  ggplot(datos_var_agrup, aes(x = padre_madre, y = grupo_edad)) +
    geom_tile(aes(fill = variacion_pct), color = "black") +
    geom_text(aes(label = paste0(variacion_pct, "%")),
              color = "black", size = 4) +
    scale_fill_gradient2(low = "red", high = "green", mid = "white",
                         midpoint = 0, name = "Variación %") +
    labs(
      x = "Condición de padres",
      y = "Grupo de edad",
      title = "Variación porcentual de población (2018–2024)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
}

#Comparativa de 2018 y 2024 ingreso 
{
  # --- 1) Limpieza de datos 2018 ---
  df_clean_2018 <- datos_unidos_2018 %>%
    select(ing_cor, edad, asis_esc) %>%
    filter(
      !is.na(ing_cor), 
      !is.na(edad), 
      !is.na(asis_esc),
      edad >= 15, edad <= 24
    ) %>%
    filter(
      ing_cor >= quantile(ing_cor, 0.25, na.rm = TRUE) - 1.5 * IQR(ing_cor, na.rm = TRUE),
      ing_cor <= quantile(ing_cor, 0.75, na.rm = TRUE) + 1.5 * IQR(ing_cor, na.rm = TRUE)
    ) %>%
    mutate(
      edad = as.integer(edad),
      asis_esc = factor(asis_esc, labels = c("No asiste", "Asiste"))
    )
  
  # --- 2) Limpieza de datos 2024 ---
  df_clean_2024 <- datos_unidos_2024 %>%
    select(ing_cor, edad, asis_esc) %>%
    filter(
      !is.na(ing_cor), 
      !is.na(edad), 
      !is.na(asis_esc),
      edad >= 15, edad <= 24
    ) %>%
    filter(
      ing_cor >= quantile(ing_cor, 0.25, na.rm = TRUE) - 1.5 * IQR(ing_cor, na.rm = TRUE),
      ing_cor <= quantile(ing_cor, 0.75, na.rm = TRUE) + 1.5 * IQR(ing_cor, na.rm = TRUE)
    ) %>%
    mutate(
      edad = as.integer(edad),
      asis_esc = factor(asis_esc, labels = c("No asiste", "Asiste"))
    )
  
  # --- 3) Función para resumen ---
  get_summary_stats_age <- function(data) {
    data %>%
      group_by(edad, asis_esc) %>%
      summarise(
        Media = mean(ing_cor, na.rm = TRUE),
        Mediana = median(ing_cor, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # --- 4) Crear tablas por año ---
  stats_2018 <- get_summary_stats_age(df_clean_2018) %>% mutate(año = 2018)
  stats_2024 <- get_summary_stats_age(df_clean_2024) %>% mutate(año = 2024)
  
  # --- 5) Juntar y calcular variaciones ---
  stats_both <- bind_rows(stats_2018, stats_2024)
  
  stats_variacion <- stats_both %>%
    pivot_wider(
      names_from = año, 
      values_from = c(Media, Mediana)
    ) %>%
    mutate(
      Var_pct_media   = 100 * (Media_2024 - Media_2018) / Media_2018,
      Var_pct_mediana = 100 * (Mediana_2024 - Mediana_2018) / Mediana_2018
    )
  
  # --- 6) Heatmap con variación porcentual (Media) ---
  ggplot(stats_variacion, aes(x = asis_esc, y = as.factor(edad), fill = Var_pct_media)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(round(Var_pct_media, 1), "%")), color = "black", size = 3) +
    scale_fill_gradient2(
      low = "red"
      high = "green",
      midpoint = 0,
      name = "Variación % (Media)"
    ) +
    labs(
      title = "Variación porcentual del ingreso corriente (2018–2024)",
      subtitle = "Por edad y asistencia escolar",
      x = "Asistencia escolar",
      y = "Edad"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(face = "bold"))
  
}





