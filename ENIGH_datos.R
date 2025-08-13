#-----------------------------------
#----------Librerias----------------
#-----------------------------------

{
  library(readr) #abrir csv
  library(haven) #abrir dta
  library(tidyverse)
  library(scales)  # Para label_comma()
  
}

#-----------------------------------
#--------------Datos----------------
#-----------------------------------

{
  poblacion_2018 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2018_ns_poblacion_csv/poblacion.csv")
  View(poblacion_2018)
  poblacion_2018 <- poblacion_2018 %>%  rename(folioviv = ï..folioviv)
  con_hogar_2018 <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/ENIGH_datos/enigh2018_ns_concentradohogar_csv/concentradohogar.csv")
  con_hogar_2018 <- con_hogar_2018 %>%  rename(folioviv = ï..folioviv)
  str(poblacion_2018$edad)
  describe(poblacion_2018$edad)
  table(poblacion_2018$asis_esc)
}


poblacion_2018_1 <- poblacion_2018 %>%
  filter(edad >= 15 & edad <= 24) %>%
  filter(parentesco == 301) %>%
  mutate(padre_madre = case_when(
    madre_hog == 2 & padre_hog == 2 ~ 1,  # No madre ni padre
    madre_hog == 1 & padre_hog == 2 ~ 2,  # Solo madre
    madre_hog == 2 & padre_hog == 1 ~ 3,  # Solo padre
    madre_hog == 1 & padre_hog == 1 ~ 4   # Ambos
  ))

table(poblacion_2018_1$padre_madre)


poblacion_2018_2 <- poblacion_2018_1 %>%
  filter(edad >= 15 & edad <= 24, !is.na(asis_esc)) %>%
  mutate(
    asis_esc = factor(asis_esc, levels = c(1, 2), labels = c("Asiste", "No asiste")),
    padre_madre = factor(padre_madre, levels = c(1, 2, 3, 4), labels = c("Ninguno", "Solo madre", "Solo padre", "Ambos"))
  )



{
# Preparar los datos con conteo y proporciones
datos_graf_2018 <- poblacion_2018_1 %>%
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
ggplot(datos_graf_2018, aes(x = as.factor(edad), y = prop, fill = asis_esc)) +
  geom_bar(stat = "identity", position = "fill", color = "black", size = 1) +
  geom_text(aes(label = label), 
            position = position_fill(vjust = 0.5), 
            size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~padre_madre) +
  labs(
    title = "Proporción de asistencia escolar por edad (15-24 años) según presencia de padres 2018",
    x = "Edad",
    y = "Porcentaje",
    fill = "Asistencia escolar"
  ) +
  theme_classic()
}

# Unir por folio de vivienda y folio de hogar
datos_unidos_2018 <- con_hogar_2018 %>%
  inner_join(poblacion_2018_2, by = c("folioviv", "foliohog"), multiple="all")

#poblacion es y
#concentrado del hogar es x

#print(datos_unidos)

sum(datos_unidos_2018$factor)

datos_unidos_2018 %>%
  group_by(padre_madre) %>%
  summarise(total_poblacion = sum(factor, na.rm = TRUE))

table(datos_unidos_2018$padre_madre)

sum((datos_unidos_2018 %>%
      group_by(padre_madre) %>%
      summarise(total_poblacion = sum(factor, na.rm = TRUE)))$total_poblacion)

#-------------------------
#-------DEMOGRAFÍA--------
#-------------------------

#SEXO
{
datos_unidos_2018 %>%
  group_by(sexo, edad, asis_esc) %>%
  summarise(total_personas = sum(factor, na.rm = TRUE), .groups = "drop") %>%
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
  datos_unidos_2018 %>%
    group_by(tot_integ, edad, asis_esc) %>%
    summarise(total_personas = sum(factor, na.rm = TRUE), .groups = "drop") %>%
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
  
  datos_unidos_2018 %>%
    pull(tot_integ) %>% 
    table()
  
  datos_unidos_2018 %>%
    group_by(tot_integ) %>%
    summarise(total_poblacion = sum(factor, na.rm = TRUE))


ggplot(datos_unidos_2018 %>% group_by(tot_integ) %>% summarise(total_poblacion = sum(factor, na.rm = TRUE)), 
       aes(x = as.factor(tot_integ), y = total_poblacion, fill = as.factor(tot_integ))) + 
  geom_col(show.legend = FALSE) + 
  geom_text(aes(label = scales::comma(total_poblacion)), vjust = -0.3, size = 3.5) +
  labs(x = "Integrantes del hogar", y = "Total población", title = "Población por tamaño de hogar") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# Sin factor de expansión
  ggplot(datos_unidos_2018 %>% 
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
ggplot(datos_unidos_2018 %>% group_by(tot_integ) %>% summarise(total_poblacion = sum(factor, na.rm = TRUE)) %>% 
         mutate(porcentaje = 100 * total_poblacion / sum(total_poblacion)), 
       aes(x = as.factor(tot_integ), y = porcentaje, fill = as.factor(tot_integ))) + 
  geom_col(show.legend = FALSE) + 
  geom_text(aes(label = paste0(round(porcentaje, 2), "%")), vjust = -0.3, size = 3.5) +
  labs(x = "Integrantes del hogar", y = "Porcentaje", title = "Porcentaje por tamaño de hogar") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
}







