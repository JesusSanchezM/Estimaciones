#-----------------------------------
#----------Librerias----------------
#-----------------------------------

{
library(readr) #abrir csv
library(haven) #abrir dta
library(tidyverse)
}

#-----------------------------------
#--------------Datos----------------
#-----------------------------------

{
cpv_2020 <- read_csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/Historico CPV INEGI/Censo2020_CA_pue_csv/Personas21.csv")
#cpv_2010 <- read_dta("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/Historico CPV INEGI/cpv_2010_personas/DUMMY_PERSONAS.dta")
}

#-----------------------------------
#-------------Analisis--------------
#-----------------------------------


cpv_2020_128 <- cpv_2020 %>% filter(cpv_2020$MUN=="128")

sum(cpv_2020_128$FACTOR)

{
table(cpv_2020_128$AGUINALDO)
table(cpv_2020_128$VACACIONES)
table(cpv_2020_128$SERVICIO_MEDICO)
table(cpv_2020_128$UTILIDADES)
table(cpv_2020_128$INCAP_SUELDO)
table(cpv_2020_128$SAR_AFORE)
table(cpv_2020_128$CREDITO_VIVIENDA)


# --- 1. Crear variable de empleo formal/informal ---
cpv_2020_128 <- cpv_2020_128 %>%
  mutate(empleo_formal = case_when(
    (AGUINALDO == 1 | UTILIDADES == 7) & VACACIONES == 3 & SERVICIO_MEDICO == 5 &
      INCAP_SUELDO == 1 & SAR_AFORE == 3 & CREDITO_VIVIENDA == 5 ~ "Formal",
    TRUE ~ "Informal"
  ))

table(cpv_2020_128$empleo_formal)

#Con factor de expansión
cpv_2020_128 %>% 
  group_by(empleo_formal)  %>%
  summarise(suma_factor_expansion = sum(FACTOR, na.rm = TRUE))


resumen_factor <- function(var, nombre_var){
  cpv_2020_128 %>%
    group_by(empleo_formal, !!sym(var)) %>%
    summarise(poblacion_estim = sum(FACTOR, na.rm = TRUE), .groups = "drop") %>%
    mutate(Variable = nombre_var) %>%
    rename(Valor = !!sym(var))
}

# --- 3. Aplicar función a todas las prestaciones ---
tab_aguinaldo      <- resumen_factor("AGUINALDO", "AGUINALDO")
tab_vacaciones     <- resumen_factor("VACACIONES", "VACACIONES")
tab_servicio       <- resumen_factor("SERVICIO_MEDICO", "SERVICIO_MEDICO")
tab_utilidades     <- resumen_factor("UTILIDADES", "UTILIDADES")
tab_incap          <- resumen_factor("INCAP_SUELDO", "INCAP_SUELDO")
tab_sar            <- resumen_factor("SAR_AFORE", "SAR_AFORE")
tab_credito        <- resumen_factor("CREDITO_VIVIENDA", "CREDITO_VIVIENDA")

# --- Unir todas en una sola tabla ---
tabla_completa <- bind_rows(tab_aguinaldo, tab_vacaciones, tab_servicio,
                            tab_utilidades, tab_incap, tab_sar, tab_credito)


# --- 5. Ver resultado ---
View(tabla_completa)

# Graficar población estimada por prestación y tipo de empleo
ggplot(tabla_completa, aes(x = Variable, y = poblacion_estim, fill = empleo_formal)) +
  geom_col(position = "dodge", color="black") +  # barras lado a lado
  labs(
    title = "Población estimada por prestaciones y tipo de empleo",
    x = "Prestación",
    y = "Población estimada",
    fill = "Tipo de empleo"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Formal" = "steelblue", "Informal" = "tomato"))

}


# Graficar población estimada por prestación y tipo de empleo
ggplot(tabla_completa, aes(x = Variable, y = poblacion_estim, fill = empleo_formal)) +
  geom_col(position = "dodge") +  # barras lado a lado
  labs(
    title = "Población estimada por prestaciones y tipo de empleo",
    x = "Prestación",
    y = "Población estimada",
    fill = "Tipo de empleo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Formal" = "steelblue", "Informal" = "tomato"))

# --- 2. Filtrar NAs y valores extremos irreales ---
datos <- cpv_2020_128 %>% 
  filter(!is.na(INGTRMEN) & INGTRMEN < 999999) %>% 
  select(INGTRMEN, empleo_formal)

# --- 3. Calcular IQR ---
Q1 <- quantile(datos$INGTRMEN, 0.25)
Q3 <- quantile(datos$INGTRMEN, 0.75)
IQR <- Q3 - Q1

lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

# --- 4. Filtrar quitando outliers ---
datos_sin_outliers <- datos %>% 
  filter(INGTRMEN >= lim_inf & INGTRMEN <= lim_sup)

# --- 5. Gráficas ---
# Histograma general
ggplot(datos_sin_outliers, aes(x = INGTRMEN)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "black") +
  labs(title = "Histograma de ingreso mensual (sin outliers, método IQR)",
       x = "Ingreso mensual",
       y = "Frecuencia") +
  theme_classic()

# Histograma por tipo de empleo
ggplot(datos_sin_outliers, aes(x = INGTRMEN, fill = empleo_formal)) +
  geom_histogram(binwidth = 1000, alpha = 0.6, position = "identity", color="black") +
  facet_wrap(~empleo_formal, scales = "free_y") +
  labs(title = "Distribución del ingreso mensual (Formal vs Informal, sin outliers)",
       subtitle = "SAN JOSE CHIAPAS",
       x = "Ingreso mensual",
       y = "Frecuencia") +
  theme_classic()

# --- 6. Estadísticos resumen ---
datos_sin_outliers %>%
  group_by(empleo_formal) %>%
  summarise(
    media = mean(INGTRMEN),
    mediana = median(INGTRMEN),
    sd = sd(INGTRMEN),
    n = n(),
    .groups = "drop"
  )




