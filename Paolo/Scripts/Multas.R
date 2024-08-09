
rm(list = ls())

require(pacman)
p_load(tidyverse, rio, here, dplyr, viridis, readxl, stringr, 
       RColorBrewer, flextable, officer, classInt)


# Carga de información

M2024 <-read_excel("D:/NUEVO D/LOCACION OEFA/Busqueda/Informes_2024.xlsx", sheet = "Componentes")