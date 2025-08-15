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
  poblacion_2024 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2024_ns_poblacion_csv/poblacion.csv")
  View(poblacion_2024)
  con_hogar_2024 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2024_ns_concentradohogar_csv/concentradohogar.csv")
  str(poblacion_2024$edad)
  describe(poblacion_2024$edad)
  table(poblacion_2024$asis_esc)
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
datos_graf_2024 <- poblacion_2024_1 %>%
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
ggplot(datos_graf_2024, aes(x = as.factor(edad), y = prop, fill = asis_esc)) +
  geom_bar(stat = "identity", position = "fill", color = "black", size = 1) +
  geom_text(aes(label = label), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~padre_madre) +
  labs(
    title = "Proporción de asistencia escolar por edad (15-24 años) según presencia de padres 2024",
    x = "Edad",
    y = "Porcentaje",
    fill = "Asistencia escolar"
  ) +
  theme_classic()



# Unir por folio de vivienda y folio de hogar
datos_unidos_2024 <- con_hogar_2024 %>%
  inner_join(poblacion_2024_2, by = c("folioviv", "foliohog"), multiple="all")

#poblacion es y
#concentrado del hogar es x

#print(datos_unidos)

sum(datos_unidos_2024$factor.y)
head(datos_unidos_2024$factor.x)

datos_unidos_2024 %>% filter(factor.y != factor.x)

attach(datos_unidos_2024)

all(factor.y == factor.x)
which(factor.y != factor.x)

detach(datos_unidos_2024)

datos_unidos_2024 %>%
  group_by(padre_madre) %>%
  summarise(total_poblacion = sum(factor.y, na.rm = TRUE))


#-------------------------
#-------DEMOGRAFÍA--------
#-------------------------

#SEXO
{
  datos_unidos_2024 %>%
    group_by(sexo, edad, asis_esc) %>%
    summarise(total_personas = sum(factor.y, na.rm = TRUE), .groups = "drop") %>%
    group_by(sexo, edad) %>%
    mutate(
      prop = total_personas / sum(total_personas),
      porcentaje = round(prop * 100, 1),
      etiqueta = paste0(porcentaje, "%"),
      sexo = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
      asis_esc = factor(asis_esc)
    ) %>%
    ggplot(aes(x = as.factor(edad), y = prop, fill = asis_esc)) +
    geom_col(color = "black", size = 0.3) +  # igual que geom_bar(stat="identity")
    geom_text(aes(label = ifelse(porcentaje > 5, etiqueta, "")),  # solo etiqueta si >5% para evitar saturar
              position = position_stack(vjust = 0.5),
              size = 3, color = "white") +
    facet_wrap(~ sexo) +
    labs(x = "Edad", y = "Proporción", fill = "Asistencia escolar") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Total de población
{
  
  datos_unidos_2024 %>%
    group_by(tot_integ, edad, asis_esc) %>%
    summarise(total_personas = sum(factor.y, na.rm = TRUE), .groups = "drop") %>%
    group_by(tot_integ, edad) %>%
    mutate(
      prop = total_personas / sum(total_personas),
      porcentaje = round(prop * 100, 1),
      etiqueta = paste0(porcentaje, "%"),
      asis_esc = factor(asis_esc)
    ) %>%
    ggplot(aes(x = as.factor(edad), y = prop, fill = asis_esc)) +
    geom_col(color = "black", size = 0.3)  +
    facet_wrap(~ tot_integ) +
    labs(x = "Edad", y = "Proporción", fill = "Asistencia escolar") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  datos_unidos_2024 %>%
    pull(tot_integ) %>% 
    table()
  
  datos_unidos_2024 %>%
    group_by(tot_integ) %>%
    summarise(total_poblacion = sum(factor.y, na.rm = TRUE))
  
  ggplot(datos_unidos_2024 %>% group_by(tot_integ) %>% summarise(total_poblacion = sum(factor.y, na.rm = TRUE)), 
         aes(x = as.factor(tot_integ), y = total_poblacion, fill = as.factor(tot_integ))) + 
    geom_col(show.legend = FALSE) + 
    geom_text(aes(label = scales::comma(total_poblacion)), vjust = -0.3, size = 3.5) +
    labs(x = "Integrantes del hogar", y = "Total población", title = "Población por tamaño de hogar") + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Sin factor de expansión
  ggplot(datos_unidos_2024 %>% 
           group_by(tot_integ) %>% 
           summarise(total_poblacion = n()), 
         aes(x = as.factor(tot_integ), y = total_poblacion, fill = as.factor(tot_integ))) + 
    geom_col(show.legend = FALSE) + 
    geom_text(aes(label = scales::comma(total_poblacion)), vjust = -0.3, size = 3.5) +
    labs(x = "Integrantes del hogar", y = "Total población (conteo registros)", 
         title = "Población por tamaño de hogar (sin factor de expansión)") + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Porcentaje
  ggplot(datos_unidos_2024 %>% group_by(tot_integ) %>% summarise(total_poblacion = sum(factor.y, na.rm = TRUE)) %>% 
           mutate(porcentaje = 100 * total_poblacion / sum(total_poblacion)), 
         aes(x = as.factor(tot_integ), y = porcentaje, fill = as.factor(tot_integ))) + 
    geom_col(show.legend = FALSE) + 
    geom_text(aes(label = paste0(round(porcentaje, 2), "%")), vjust = -0.3, size = 3.5) +
    labs(x = "Integrantes del hogar", y = "Porcentaje", title = "Porcentaje por tamaño de hogar") + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}

#Ingreso corriente
{
  
  # --- 1) Limpieza y transformación de datos ---
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
      edad = factor(edad),
      asis_esc = factor(asis_esc, labels = c("No asiste", "Asiste"))
    )
  
  # --- 2) Funciones auxiliares ---
  # Formato de tabla
  format_table <- function(data, caption = "") {
    data %>%
      kable("html", caption = caption) %>%
      kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "hover", "condensed")
      )
  }
  
  # Estadísticas resumen
  get_summary_stats <- function(data, group_var = NULL) {
    if (!is.null(group_var)) data <- data %>% group_by(across({{group_var}}))
    data %>%
      summarise(
        n = n(),
        Media = mean(ing_cor),
        Mediana = median(ing_cor),
        SD = sd(ing_cor),
        Q1 = quantile(ing_cor, 0.25),
        Q3 = quantile(ing_cor, 0.75),
        Mínimo = min(ing_cor),
        Máximo = max(ing_cor),
        .groups = "drop"
      )
  }
  
  # --- 3) Tablas descriptivas ---
  # Tabla: asistencia escolar
  df_clean_2024 %>%
    count(asis_esc, name = "n") %>%
    mutate(Porcentaje = round(100 * n / sum(n), 1)) %>%
    format_table("Frecuencia y porcentaje de asistencia escolar")
  
  # Tabla: resumen general de ingresos
  get_summary_stats(df_clean_2024) %>%
    format_table("Resumen general de ingreso corriente")
  
  # Tabla: ingresos por asistencia escolar
  get_summary_stats(df_clean_2024, "asis_esc") %>%
    format_table("Resumen de ingreso por asistencia escolar")
  
  # Tabla: ingresos por edad y asistencia
  get_summary_stats(df_clean_2024, c("edad", "asis_esc")) %>%
    format_table("Resumen de ingreso por edad y asistencia escolar")
  
  # --- 4) Visualizaciones ---
  # Boxplot ingresos por edad y asistencia
  ggplot(df_clean_2024, aes(x = edad, y = ing_cor, fill = asis_esc)) +
    geom_boxplot(position = position_dodge2(width = 0.75, preserve = "single"), outlier.alpha = 0.2) +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Distribución de ingresos por edad y asistencia escolar",
      subtitle = "Población de 15 a 24 años",
      x = "Edad", y = "Ingreso corriente", fill = "Asistencia escolar"
    ) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "top")
  
  # Histogramas ingresos por asistencia
  ggplot(df_clean_2024, aes(x = ing_cor, fill = asis_esc)) +
    geom_histogram(
      bins = 30,
      color = "white",
      alpha = 0.7,
      position = "identity" # superpone las barras
    ) +
    scale_fill_manual(values = c("No asiste" = "#F8766D", "Asiste" = "#00BFC4")) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title = "Distribución de ingresos por asistencia escolar",
      x = "Ingreso corriente",
      y = "Frecuencia",
      fill = "Asistencia escolar"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "top"
    )
  
}




