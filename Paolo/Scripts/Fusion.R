rm(list = ls())

require(pacman)
p_load(tidyverse, rio, here, dplyr, viridis, readxl, stringr, 
       RColorBrewer, flextable, officer, classInt)


# Carga de información

M2022 <-read_excel("D:/NUEVO D/LOCACION OEFA/Busqueda/Informes_2022.xlsx", sheet = "Componentes")
M2023 <-read_excel("D:/NUEVO D/LOCACION OEFA/Busqueda/Informes_2023.xlsx", sheet = "Componentes")
M2024 <-read_excel("D:/NUEVO D/LOCACION OEFA/Busqueda/Informes_2024.xlsx", sheet = "Componentes")