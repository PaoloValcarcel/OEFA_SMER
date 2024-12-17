

rm(list = ls())
getwd()


library(foreign)
library(dplyr)
library(readxl)
library(httr)

######    Base de datos   ########


#Año 2022 primera revisión
url1 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/refs/heads/main/Oscar/info_sistematizada/Info_2022_1p.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url1, write_disk(temp_file, overwrite = TRUE))
info2022_1p <- read_excel(temp_file, sheet = "Componentes")  
rm(temp_file, url1)

#Año 2022 segunda revisión
url2 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/refs/heads/main/Oscar/info_sistematizada/Info_2022_2p.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url2, write_disk(temp_file, overwrite = TRUE))
info2022_2p <- read_excel(temp_file, sheet = "Consolidado")  
rm(temp_file, url2)

#Año 2023 primera revisión
url3 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/refs/heads/main/Oscar/info_sistematizada/Info_2023_1p.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url3, write_disk(temp_file, overwrite = TRUE))
info2023_1p <- read_excel(temp_file, sheet = "Componentes")  
rm(temp_file, url3)

#Año 2023 segunda revisión
url4 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/refs/heads/main/Oscar/info_sistematizada/Info_2023_2p.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url4, write_disk(temp_file, overwrite = TRUE))
info2023_2p <- read_excel(temp_file, sheet = "Consolidado")  
rm(temp_file, url4)

#Año 2024 primera revisión
url5 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/refs/heads/main/Oscar/info_sistematizada/Info_2024_1p.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url5, write_disk(temp_file, overwrite = TRUE))
info2024_1p <- read_excel(temp_file, sheet = "Componentes")  
rm(temp_file, url5)

#Año 2024 segunda revisión
url6 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/refs/heads/main/Oscar/info_sistematizada/Info_2024_2p.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url6, write_disk(temp_file, overwrite = TRUE))
info2024_2p <- read_excel(temp_file, sheet = "Consolidado")  
rm(temp_file, url6)


######    Limpieza de información   ########


#PRIMERA REVISIÓN

#Primera revisión
info2022_1p_filter <- info2022_1p %>%
  filter(!Hecho_imputado %in% c("Reconsideración", "Multa coercitiva", "Sin especificar"))

info2023_1p_filter<- info2023_1p %>%
  filter(!Hecho_imputado %in% c("Reconsideración", "multa coercitiva", "Multa coercitiva"))

info2024_1p_filter<- info2024_1p %>%
  filter(!Hecho_imputado %in% c("Multa coercitiva", "Reconsideración", 
                                "Medida correctiva", "Informe de enmienda"))


#Verificamos el nombre de las varaibles
names(info2022_1p_filter)
names(info2023_1p_filter)
names(info2024_1p_filter)

#Seleccionamos las variables que consideramos relevantes
info2022_1p_filter <- info2022_1p_filter[c("Informes", "Expedientes", "Hecho_imputado", "Num_Imputacion", "Sub_extremo", "Monto",
                                           "COS_anual", "T_meses", "Costo_evitado", "Unidad_monetaria", "Tipo_de_cambio",
                                           "Beneficio_ilícito", "Prob_Detección","Multa", "Multa_Final", "Sancion_total",
                                           "Colapsar", "Observaciones")]

info2023_1p_filter <- info2023_1p_filter[c("Informes", "Expedientes", "Hecho_imputado", "Num_Imputacion", "Sub_extremo", "Monto",
                                           "COS_anual", "T_meses", "Costo_evitado", "Unidad_monetaria", "Tipo_de_cambio",
                                           "Beneficio_ilícito", "Prob_Detección","Multa", "Multa_Final", "Sancion_total",
                                           "Colapsar", "Observaciones")]

info2024_1p_filter <- info2024_1p_filter[c("Informes", "Expedientes", "Hecho_imputado", "Num_Imputacion", "Sub_extremo", "Monto",
                                           "COS_anual", "T_meses", "Costo_evitado", "Unidad_monetaria", "Tipo_de_cambio",
                                           "Beneficio_ilícito", "Prob_Detección","Multa", "Multa_Final", "Sancion_total",
                                           "Colapsar", "Observaciones")]
                                           

# Unimos los data frames
dfunido1 = rbind(info2022_1p_filter, info2023_1p_filter, info2024_1p_filter)

#Modificamos el nombre de la variable
dfunido1 <- dfunido1 %>% rename(extremo = Sub_extremo)

#Completamos la columna
dfunido1$extremo = ifelse(is.na(dfunido1$extremo), 0, dfunido1$extremo)

#Generamos códigos para la base de datos
dfunido1$codigo_impu = paste(dfunido1$Informes, dfunido1$Num_Imputacion, sep = "-")
dfunido1$codigo_extremo = paste(dfunido1$Informes, dfunido1$Num_Imputacion, dfunido1$extremo, sep = "-")


#Creamos un identificador para la fase en la que revisó la información


resumen1 <- table(dfunido1$codigo_impu)






