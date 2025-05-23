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
cpv_2020 <- read_csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/Historico CPV INEGI/cpv_2020_personas/Censo2020_CPV_CB_Personas_ejemplo_csv.CSV")
cpv_2010 <- read_dta("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestria BUAP - Economia/0. TESIS/Datos/Historico CPV INEGI/cpv_2010_personas/DUMMY_PERSONAS.dta")




}

#-----------------------------------
#-------------Analisis--------------
#-----------------------------------

table(cpv_2020$ALFABET)

View(cpv_2020 %>%
  group_by(EDAD) %>%
  summarise(Suma = sum(HIJOS_NAC_VIVOS, na.rm = TRUE)))












