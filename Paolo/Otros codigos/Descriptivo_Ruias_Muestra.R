

rm(list = ls())

######################
####### RUIAS ########
######################

# Carga de información de RUIAS
url3 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/RUIAS-CSEP.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url3, write_disk(temp_file, overwrite = TRUE))
RUIAS <- read_excel(temp_file, sheet = "RUIAS")  
rm(temp_file, url3)

colnames(RUIAS)[colnames(RUIAS) == "Multa por infracción final Incumplimiento" ] <- "Multa"
colnames(RUIAS)[colnames(RUIAS) == "Fecha de la Resolución...38" ] <- "Fecha_PAS"
RUIAS$Año <- format(RUIAS$Fecha_PAS, "%Y")

table(RUIAS$Año)
Analisis <- RUIAS %>%
  filter(Año>2021)
table(Analisis$Año)

library(moments)

Filtro <- Analisis %>% filter(Multa > 0)

Ruias_Año <- Filtro %>%
  group_by(Año) %>%
  summarise(
    Promedio = mean(Multa, na.rm = TRUE),
    Mediana = median(Multa, na.rm = TRUE),
    Desviacion = sd(Multa, na.rm = TRUE),
    Asimetria = skewness(Multa, na.rm = TRUE),
    Curtosis = kurtosis(Multa, na.rm = TRUE),
    Maximo = max(Multa, na.rm = TRUE),
    Minimo = min(Multa, na.rm = TRUE),
    Percentil_25 = quantile(Multa, 0.25, na.rm = TRUE),
    Percentil_75 = quantile(Multa, 0.75, na.rm = TRUE))

Ruias_General <- Filtro %>%
  summarise(
    Promedio = mean(Multa, na.rm = TRUE),
    Mediana = median(Multa, na.rm = TRUE),
    Desviacion = sd(Multa, na.rm = TRUE),
    Asimetria = skewness(Multa, na.rm = TRUE),
    Curtosis = kurtosis(Multa, na.rm = TRUE),
    Maximo = max(Multa, na.rm = TRUE),
    Minimo = min(Multa, na.rm = TRUE),
    Percentil_25 = quantile(Multa, 0.25, na.rm = TRUE),
    Percentil_75 = quantile(Multa, 0.75, na.rm = TRUE))


#######################
####### Muestra #######
#######################

rm(list = ls())

# Carga de información del Consolidado 1
url1 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/dfunido.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url1, write_disk(temp_file, overwrite = TRUE))
Consolidado <- read_excel(temp_file, sheet = "Sheet 1")  
rm(temp_file, url1)

# Carga de información del Consolidado 2
url2 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/dfunido2.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url2, write_disk(temp_file, overwrite = TRUE))
Consolidado2 <- read_excel(temp_file, sheet = "Sheet 1")  
rm(temp_file, url2)

# Quitando las observaciones que no usaremos del consolidado

CFinal1 <- Consolidado %>%
  filter(!Propuesta_Multa %in% c("si"),!Observaciones%in% c("Se debe eliminar por criterio del informe"))

colnames(CFinal1)[colnames(CFinal1) == "ID2"] <- "Index"
CFinal1$Index <- as.character(CFinal1$Index)

colnames(Consolidado2)[colnames(Consolidado2) == "ID RUIAS"] <- "Index"
colnames(Consolidado2)[colnames(Consolidado2) == "Sub extremo"] <- "Sub_extremo"
colnames(Consolidado2)[colnames(Consolidado2) == "Expedientes"] <- "Expediente"
Consolidado2$Index <- as.character(Consolidado2$Index)


CFinal1 <- CFinal1 %>% dplyr::select("ID", "Expediente", "Informes", "Hecho_imputado", 
                                     "Num_Imputacion","Sub_extremo", "Monto", "Index",
                                     "COS_anual", "T_meses",  "Costo_evitado", "Unidad_monetaria",
                                     "Tipo_de_cambio", "Beneficio_ilícito", "Prob_Detección",
                                     "Multa", "Multa_Final", "Sancion_total",  "Colapsar", "year")

CFinal2 <- Consolidado2 %>% dplyr::select("ID", "Expediente", "Informes", "Hecho_imputado", 
                                          "Num_Imputacion","Sub_extremo", "Monto", "Index",
                                          "COS_anual", "T_meses",  "Costo_evitado", "Unidad_monetaria",
                                          "Tipo_de_cambio", "Beneficio_ilícito", "Prob_Detección",
                                          "Multa", "Multa_Final", "Sancion_total",  "Colapsar", "year")

CFinal1$Datos <- "Primera fase"
CFinal2$Datos <- "Segunda fase"

CFinal <- rbind(CFinal1, CFinal2)
rm(CFinal1, CFinal2, Consolidado, Consolidado2)
CFinal  <- CFinal  %>% filter(Multa_Final != 0)

CFinal$Multa_Final <- as.numeric(CFinal$Multa_Final)

Muestra_Año <- CFinal %>%
  group_by(year) %>%
  summarise(
    Promedio = mean(Multa_Final, na.rm = TRUE),
    Mediana = median(Multa_Final, na.rm = TRUE),
    Desviacion = sd(Multa_Final, na.rm = TRUE),
    Asimetria = skewness(Multa_Final, na.rm = TRUE),
    Curtosis = kurtosis(Multa_Final, na.rm = TRUE),
    Maximo = max(Multa_Final, na.rm = TRUE),
    Minimo = min(Multa_Final, na.rm = TRUE),
    Percentil_25 = quantile(Multa_Final, 0.25, na.rm = TRUE),
    Percentil_75 = quantile(Multa_Final, 0.75, na.rm = TRUE))

Muestra_General <- CFinal %>%
  summarise(
    Promedio = mean(Multa_Final, na.rm = TRUE),
    Mediana = median(Multa_Final, na.rm = TRUE),
    Desviacion = sd(Multa_Final, na.rm = TRUE),
    Asimetria = skewness(Multa_Final, na.rm = TRUE),
    Curtosis = kurtosis(Multa_Final, na.rm = TRUE),
    Maximo = max(Multa_Final, na.rm = TRUE),
    Minimo = min(Multa_Final, na.rm = TRUE),
    Percentil_25 = quantile(Multa_Final, 0.25, na.rm = TRUE),
    Percentil_75 = quantile(Multa_Final, 0.75, na.rm = TRUE))
