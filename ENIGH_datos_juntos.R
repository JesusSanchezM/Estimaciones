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

# Gráfico
ggplot(df_combinado %>% filter(sexo == "Hombre"),
       aes(x = interaction(edad, año, sep = " -" , lex.order = TRUE), 
           y = prop, fill = asis_esc)) +
  geom_col(color = "black", size = 0.3) +
  geom_text(aes(label = ifelse(porcentaje > 5, etiqueta, "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(x = "Edad (Año)", y = "Proporción", fill = "Asistencia escolar",
       title = "Asistencia escolar por edad y año (Hombres)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Asiste" = "salmon", "No asiste" = "lightblue"))

ggplot(df_combinado %>% filter(sexo == "Mujer"),
       aes(x = interaction(edad, año, sep = " -" , lex.order = TRUE), 
           y = prop, fill = asis_esc)) +
  geom_col(color = "black", size = 0.3) +
  geom_text(aes(label = ifelse(porcentaje > 5, etiqueta, "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(x = "Edad (Año)", y = "Proporción", fill = "Asistencia escolar",
       title = "Asistencia escolar por edad y año (Mujeres)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Asiste" = "salmon", "No asiste" = "lightblue"))



# Gráfico unificado con facet_wrap
ggplot(df_combinado,
       aes(x = interaction(edad, año, sep = " -" , lex.order = TRUE), 
           y = prop, fill = asis_esc)) +
  geom_col(color = "black", size = 0.3) +
  geom_text(aes(label = ifelse(porcentaje > 5, etiqueta, "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  labs(x = "Edad (Año)", y = "Proporción", fill = "Asistencia escolar",
       title = "Asistencia escolar por edad y año") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Asiste" = "salmon", "No asiste" = "lightblue")) +
  facet_wrap(~sexo)


# Gráfico unificado con ajustes
ggplot(df_combinado,
       aes(x = interaction(edad, año, sep = " -" , lex.order = TRUE), 
           y = prop, fill = asis_esc)) +
  geom_col(color = "black", size = 0.3) +
  geom_text(aes(label = ifelse(porcentaje > 5, etiqueta, "")),
            position = position_stack(vjust = 0.5),
            angle = 90, hjust = 0.5,  # Texto horizontal centrado
            size = 3, color = "black") +
  labs(x = "Edad (Año)", y = "Proporción", fill = "Asistencia escolar",
       title = "Asistencia escolar por edad y año") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"   # Leyenda abajo
  ) +
  facet_wrap(~sexo)
