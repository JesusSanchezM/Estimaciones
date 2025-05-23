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
  poblacion <- read.csv("C:/Users/Jesus Sanchez/Desktop/ALEXIS/1. Estudio/Maestría BUAP - Economía/0. TESIS/Datos/ENIGH_datos/enigh2022_ns_poblacion_csv/poblacion.csv")
  View(poblacion)
  poblacion$fa
  
}










