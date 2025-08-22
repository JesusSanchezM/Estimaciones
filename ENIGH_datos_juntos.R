library(tidyverse)


#Manejo de datos juntos 
{
datos_unidos_2018 <- datos_unidos_2018 %>%
  rename(factor.y = factor)

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

library(dplyr)
library(tidyr)

# Pivotear para tener 2018 y 2024 en columnas
variacion_padres <- datos_padres %>%
  select(año, padre_madre, edad, asis_esc, prop) %>%
  pivot_wider(names_from = año, values_from = prop, names_prefix = "prop_") %>%
  mutate(
    variacion_pct = (prop_2024 - prop_2018) / prop_2018 * 100
  )

# Ver tabla ordenada
variacion_padres %>%
  arrange(padre_madre, edad, asis_esc)

variacion_padres <- variacion_padres %>%
  mutate(variacion_pct = round(variacion_pct, 2))

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
