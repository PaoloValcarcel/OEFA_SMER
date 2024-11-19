
require(pacman)
p_load(foreign, tidyverse, rio, here, dplyr, viridis, readxl, stringr, RColorBrewer, ggcorrplot,  
        flextable, officer, classInt, foreign, stargazer, sf, mapview, leaflet, writexl, lmtest,
       tseries, car, haven, officer, xlsx, openxlsx, httr)

rm(list = ls())

###################################
###### Informes de sanción ########
###################################

# Carga de información del Consolidado 1
url1 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/dfunido.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url1, write_disk(temp_file, overwrite = TRUE))
Consolidado <- read_excel(temp_file, sheet = "Sheet 1")  
rm(temp_file, url1)
#Consolidado <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/dfunido.xlsx", sheet = "Sheet 1")

# Carga de información del Consolidado 2
url2 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/dfunido2.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url2, write_disk(temp_file, overwrite = TRUE))
Consolidado2 <- read_excel(temp_file, sheet = "Sheet 1")  
rm(temp_file, url2)

# Carga de información de RUIAS
url3 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/RUIAS-CSEP.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url3, write_disk(temp_file, overwrite = TRUE))
RUIAS <- read_excel(temp_file, sheet = "RUIAS")  
rm(temp_file, url3)
#RUIAS <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/RUIAS-CSEP.xlsx", sheet = "RUIAS")

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

CFinal <- rbind(CFinal1, CFinal2)
rm(CFinal1, CFinal2, Consolidado, Consolidado2)

# Seleccionando las variables a usar

RFinal <- RUIAS %>% dplyr::select("ID", "Administrado", "RUC", "Sector económico", "Departamento",
                                  "Provincia", "Distrito", "Infracción cometida sancionada (Clasificación de 11)",
                                  "Infracción cometida sancionada (Clasificación de 19)", 
                                  "Inicio de supervisión", "Fin de supervisión", "Fecha de notificación...23",
                                  "Documento de inicio", "Fecha de emisión",
                                  "N° de Resolución de Responsabilidad Administrativa", "Fecha de la Resolución...38", 
                                  "Fecha de notificación...39")

colnames(RFinal)[colnames(RFinal) == "ID"] <- "Index"
colnames(RFinal)[colnames(RFinal) == "Sector económico"] <- "SectorEco"
colnames(RFinal)[colnames(RFinal) == "Inicio de supervisión"] <- "InicioSup"
colnames(RFinal)[colnames(RFinal) == "Fin de supervisión"] <- "FinSup"
colnames(RFinal)[colnames(RFinal) == "Fecha de notificación...23"] <- "InicioPAS"
colnames(RFinal)[colnames(RFinal) == "Infracción cometida sancionada (Clasificación de 11)"] <- "Incumplimiento_11"
colnames(RFinal)[colnames(RFinal) == "Infracción cometida sancionada (Clasificación de 19)"] <- "Incumplimiento_19"
colnames(RFinal)[colnames(RFinal) == "Fecha de notificación...23"] <- "Fecha_Notificacion_23"
colnames(RFinal)[colnames(RFinal) == "Fecha de notificación...39"] <- "Fecha_Notificacion_39"
colnames(RFinal)[colnames(RFinal) == "Fecha de la Resolución...38"] <- "Fecha_Resolucion"
colnames(RFinal)[colnames(RFinal) == "N° de Resolución de Responsabilidad Administrativa"] <- "Nro_Resolucion"
colnames(RFinal)[colnames(RFinal) == "Documento de inicio"] <- "Documento_Inicio"
colnames(RFinal)[colnames(RFinal) == "Fecha de emisión"] <- "Fecha_Emision"


RFinal$Index <- as.character(RFinal$Index)

# Fusionando ambas bases
FINAL <-left_join(x = CFinal, y = RFinal, by="Index")
FINAL <- FINAL %>%
  mutate(Merge = if_else(!is.na(Departamento), 1, 0)) 

# Eliminando los objetos que no necesitamos
rm(CFinal, RFinal, RUIAS)

# Arreglando las etiquetas de los sectores

FINAL$Sancion_total <- as.numeric(FINAL$Sancion_total)
FINAL$Multa_Final <- as.numeric(FINAL$Multa_Final)
FINAL$Prob_Detección <- as.numeric(FINAL$Prob_Detección)
FINAL$Beneficio_ilícito <- as.numeric(FINAL$Beneficio_ilícito)
FINAL$T_meses <- as.numeric(FINAL$T_meses)
FINAL <- FINAL %>%
  mutate(SectorEco = tolower(SectorEco))

FINAL$SectorEco <- sapply(FINAL$SectorEco, function(x) {
  paste(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))), sep = "")
})

FINAL <- FINAL %>% filter(Multa_Final != 0)

# Quedándome solo con los que fusionaron perfecto con el RUIAS

table(FINAL$Merge)
FINAL <- FINAL %>%
  filter(Merge == 1)

#Admins <- FINAL %>% dplyr::select("Administrado", "RUC")
#Administrados <- Admins %>%
#  distinct(Administrado, RUC)
#write_xlsx(Administrados, path = "D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/Administrados.xlsx")
#rm(Admins, Administrados)

# Index 7020

# Carga de información de tamaño de empresas
url4 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/Tamaño_Empresas.xls"
temp_file <- tempfile(fileext = ".xls")
GET(url4, write_disk(temp_file, overwrite = TRUE))
Tamaño <- read_excel(temp_file, sheet = "Sheet1")  
rm(temp_file, url4)

Tamaño$RUC <- as.character(Tamaño$RUC)

# Fusionando con Tamaño
FINAL <-left_join(x = FINAL, y = Tamaño, by="RUC")
rm(Tamaño)

FINAL$ciiu <- ifelse(FINAL$Index == "7020", "1320", FINAL$ciiu)
FINAL$descciiu <- ifelse(FINAL$Index == "7020", "EXT. DE MIN. METALIFEROS NO FERROSOS.", FINAL$descciiu)
FINAL$trabajadores <- ifelse(FINAL$Index == "7020", 288, FINAL$trabajadores)
FINAL$Contribuyente <- ifelse(FINAL$Index == "7020", "SOCIEDAD ANONIMA CERRADA", FINAL$Contribuyente)
FINAL$Regimen <- ifelse(FINAL$Index == "7020", "REG", FINAL$Regimen)
FINAL$Tamaño_emp <- ifelse(FINAL$Index == "7020", "Gran empresa", FINAL$Tamaño_emp)

### ----- Otros ajustes ----- ###

### Obteniendo el total de hechos imputados, extremos y sub extremos ###

FINAL$Colapsar <- ifelse(FINAL$Colapsar == "Máximo", "Maximo", FINAL$Colapsar)
table(FINAL$Colapsar)

# Seleccionando variables a emplear
Extremos <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar)

#  245 registros con extremos y sub extremos (Anterior)
#  350 registros con extremos y sub extremos (Actual)
Revision <- Extremos %>% 
            filter(Colapsar!="NA")

#  64 hechos imputados con extremos y sub extremos (Anterior)
#  94 hechos imputados con extremos y sub extremos (Actual)
Revision2 <- Revision %>%
  distinct(Informes, Num_Imputacion)

#  37 informes con hechos imputados con extremos y sub extremos (Anterior)
#  57 informes con hechos imputados con extremos y sub extremos (Actual)
Revision3 <- Revision2 %>%
  distinct(Informes)

rm(Extremos, Revision, Revision2, Revision3)

# 797 - 245 = 552 Hechos imputados
# 552 + 64 = 616 Hechos imputados

###################################
###### Fechas de informes #########
###################################

# Bucle para descargar y leer los archivos por año
years <- c(2022, 2023, 2024)
for (year in years) {
  url <- paste0("https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Fechas/Fecha_", year, ".xlsx")
  temp_file <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  assign(paste0("F", year), read_excel(temp_file, sheet = as.character(year)))
  rm(temp_file, url)
}
rm(year, years)

Fechas1 <- rbind(F2022, F2023, F2024)
rm(F2022, F2023, F2024)

Fechas1 <- Fechas1 %>% dplyr::select(Informes, Fecha_Informe)

years <- c(2022, 2023, 2024)
for (year in years) {
  url <- paste0("https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Fechas/Fecha_", year, "b.xlsx")
  temp_file <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  assign(paste0("F", year), read_excel(temp_file, sheet = as.character(year)))
  rm(temp_file, url)
}
rm(year, years)

Fechas2 <- rbind(F2022, F2023, F2024)
colnames(Fechas2)[colnames(Fechas2) == "Fecha"] <- "Fecha_Informe"
Fechas2 <- Fechas2 %>% dplyr::select(Informes, Fecha_Informe)
rm(F2022, F2023, F2024)

Fechas <- rbind(Fechas1, Fechas2)
rm(Fechas1, Fechas2)

FINAL <-left_join(x = FINAL, y = Fechas, by="Informes")
rm(Fechas)

#table(is.na(FINAL$Fecha_Informe))
#View(FINAL[is.na(FINAL$Fecha_Informe), ])

###################################
##### Factores de graduación ######
###################################

# Bucle para leer los archivos por año
years <- c(2022, 2023, 2024)
Factores <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Factores/Copia%20de%20Informes_"
for (year in years) {
  url <- paste0(Factores, year, ".xlsx")
  temp_file <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  assign(paste0("G", year), read_excel(temp_file, sheet = "Graduacion"))
  rm(temp_file, url)
}
rm(Factores, year, years)

#G2022 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2022.xlsx", sheet = "Graduacion")
#G2023 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2023.xlsx", sheet = "Graduacion")
#G2024 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2024.xlsx", sheet = "Graduacion")

# Quitando de las bases las categorías a no emplear
G2022F<- G2022 %>%
  filter(!Categoria_FA %in% c("Reconsideración", "Multa coercitiva", "Sin factor"))

G2022F<- G2022F %>%
  filter(!Observaciones %in% c("Eliminado de la conclusió del informe de sanción por motivo específico"))

G2023F<- G2023 %>%
  filter(!Categoria_FA %in% c("Reconsideración", "multa coercitiva", "multas coercitivas"))

G2024F<- G2024 %>%
  filter(!Categoria_FA %in% c("Multa coercitiva", "Reconsideración", "Enmienda multa coercitiva",
                              "Medida correctiva", "Informe de enmienda"))
G2024F <- G2024F %>%
  filter(!(ID == 167 & Imputacion == 4), !(ID == 179 & Imputacion == 3))

G2024F <- G2024F %>%
  filter(!ID == 71)

# "Se debe eliminar por criterio del informe"

# Selecionando las variables a emplear
G2022F <- G2022F %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA", "Imputacion")
G2023F <- G2023F %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA", "Imputacion")
G2024F <- G2024F %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA", "Imputacion")

# Haciendo un append
Aglomerado <- rbind(G2022F, G2023F, G2024F)
rm(G2022,G2022F, G2023, G2023F, G2024,G2024F)

# Cargando la base de DFUNIDO
#Consolidado <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/dfunido.xlsx", sheet = "Sheet 1")
url1 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/dfunido.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url1, write_disk(temp_file, overwrite = TRUE))
Consolidado <- read_excel(temp_file, sheet = "Sheet 1")  
rm(temp_file, url1)

# Seleccionando las variables
Consolidado <- Consolidado %>% dplyr::select("Informes", "Propuesta_Multa")
Consolidado <- Consolidado %>%
  filter(Propuesta_Multa == "si")

#Quitando duplicados
Unicos <- Consolidado[!duplicated(Consolidado$Informes), ]

# Fusionando bases
Fusion <-left_join(x = Aglomerado, y = Unicos, by="Informes")
rm(Consolidado, Aglomerado, Unicos)

# Exportando
#write_xlsx(Fusion, path = "D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Factores.xlsx")

table(Fusion$Propuesta_Multa, useNA = "ifany")

Fusion <- Fusion %>%
  filter(is.na(Propuesta_Multa))

Aglomerado1 <- Fusion %>%
  filter(!(Informes == "00575-2023-OEFA/DFAI-SSAG" & Imputacion == 1))
rm(Fusion)

Aglomerado1 <- Aglomerado1 %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")
table(Aglomerado1$Factores_agravantes, useNA = "ifany")

# Importando Los nuevos Factores de la segunda tanda de informes
years <- c(2022, 2023, 2024)
Factores <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Factores/Info_"
for (year in years) {
  url <- paste0(Factores, year, ".xlsx")
  temp_file <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  assign(paste0("G", year), read_excel(temp_file, sheet = "Graduación"))
  rm(temp_file, url)
}
rm(Factores, year, years)

G2022F <- G2022 %>%
  filter(!Observaciones %in% c("Multa Coercitiva", "Propuesta de calculo de multa"))

G2023F <- G2023 %>%
  filter(!Observaciones %in% c("Multa coercitiva", "Propuesta Calculo de Multa"))

G2024F <- G2024 %>%
  filter(!Observaciones %in% c("Multas Coercitivas", "Propuesta Calculo de Multa", "Recurso de Reconsideracion"))

Aglomerado2 <- rbind(G2022F, G2023F, G2024F)
rm(G2022,G2022F, G2023, G2023F, G2024,G2024F)
colnames(Aglomerado2)[colnames(Aglomerado2) == "Correlativo"] <- "ID"
Aglomerado2 <- Aglomerado2 %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")

# Combinando los factores
FACTORES <- rbind(Aglomerado1, Aglomerado2)
rm(Aglomerado1, Aglomerado2)

# Gráfico de barras de factores
Filtro <- FACTORES[FACTORES$Factores_agravantes %in% c("F1", "F2", "F3", "F5", "F6"), ]
table(Filtro$Factores_agravantes, useNA = "ifany")

ggplot(Filtro, aes(x = Factores_agravantes)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  labs(x = " ",
       y = "Factores atenuantes y agravantes") +
  theme_minimal()

rm(Filtro)
#write.xlsx(Filtro,"D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/INFORMES_GRADUACION.xlsx")

###################################
###### Exportando las bases #######
###################################


FINAL1 <- FINAL %>% dplyr::select(-c("Hecho_imputado"))


wb <- createWorkbook()

# Añadiendo la primera hoja con el primer dataframe
addWorksheet(wb, "Informes")
writeData(wb, "Informes", FINAL1, colNames = TRUE)

# Añadiendo la segunda hoja con el segundo dataframe
addWorksheet(wb, "Factores")
writeData(wb, "Factores", FACTORES, colNames = TRUE)

# Guardardando el archivo Excel
saveWorkbook(wb, "D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/INFORMES_GRADUACION.xlsx", overwrite = TRUE)


###################################
#### Estadísticas Descriptivas ####
###################################

### --- Total de informes de cálculo de multa y hechos imputados --- ###

Unique <- FINAL %>%
  distinct(year, Informes)

# Conteo de observaciones por año en el objeto FINAL
cFINAL <- FINAL %>%
  group_by(year) %>%
  summarise(Hechos_Imputados = n())

# Conteo de informes por año en el objeto Unique
cUnique <- Unique %>%
  group_by(year) %>%
  summarise(Informes = n_distinct(Informes))

# Fusionamos ambos conteos en un solo data frame por año
Grafico <- full_join(cFINAL, cUnique, by = "year")

# Reorganizamos los datos en un formato long
Glong <- Grafico %>%
  pivot_longer(cols = c(Informes, Hechos_Imputados),
               names_to = "Origen",
               values_to = "Conteo")

ggplot(Glong, aes(x = factor(year), y = Conteo, fill = Origen)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +  
  geom_text(aes(label = Conteo), position = position_dodge2(width = 0.9, reverse = TRUE), vjust = -0.5) +
  labs(x = "Año", y = "Información cruzada con el RUIIAS", fill = NULL) +
  scale_fill_manual(values = c("Hechos_Imputados" = "indianred2", "Informes" = "#7AC5CD"),
                    labels = c("Hechos Imputados", "Informes")) +
  theme_minimal() +
  theme(legend.position = "bottom")

rm(cFINAL, Grafico, Glong, Unique)

### ------- Solo hechos imputados fusionado con RUIIAS ------- ###

Extremos <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar, year)

Revision1 <- Extremos %>% 
  filter(Colapsar!="NA")

Rev1 <- Revision1 %>%
  distinct(Informes, Num_Imputacion)

Revision2 <- Extremos %>% 
  filter(is.na(Colapsar))

Rev2 <- Revision2 %>%
  distinct(Informes, Num_Imputacion)

# Ahora ya cuadra Revision2 con Rev2 al tener los mismos datos únicos

Revs <- rbind(Rev1, Rev2)
Revs <- Revs %>% dplyr::select(Informes)
Revs$year <- sub(".*-(\\d{4}).*", "\\1", Revs$Informes)

cRevs <- Revs %>%
  group_by(year) %>%
  summarise(Hechos_Imputados = n())

# Fusionamos ambos conteos en un solo data frame por año
cUnique$year <- as.character(cUnique$year)
Grafico <- full_join(cRevs, cUnique, by = "year")

# Reorganizamos los datos en un formato long
Glong <- Grafico %>%
  pivot_longer(cols = c(Informes, Hechos_Imputados),
               names_to = "Origen",
               values_to = "Conteo")

ggplot(Glong, aes(x = factor(year), y = Conteo, fill = Origen)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +  
  geom_text(aes(label = Conteo), position = position_dodge2(width = 0.9, reverse = TRUE), vjust = -0.5) +
  labs(x = "Año", y = "Información cruzada con el RUIIAS", fill = NULL) +
  scale_fill_manual(values = c("Hechos_Imputados" = "indianred2", "Informes" = "#7AC5CD"),
                    labels = c("Hechos Imputados", "Informes")) +
  theme_minimal() +
  theme(legend.position = "bottom")

rm(Grafico, Glong, Rev1, Rev2, Revision1, Revision2, Revs, Extremos, cRevs, cUnique)


### --- Infracciones analizadas por sectores para el período de 2022 a 2024 --- ###

Extremos <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar, SectorEco)

Hechos <- Extremos %>% 
  filter(is.na(Colapsar))
Hechos <- Hechos %>% dplyr::select(Informes, Num_Imputacion, SectorEco)

Revision <- Extremos %>% 
  filter(Colapsar!="NA")

Revision2 <- Revision %>%
  distinct(Informes, Num_Imputacion, SectorEco)

Imputaciones <- rbind(Hechos, Revision2)
Imputaciones$year <- sub(".*-(\\d{4})-.*", "\\1", Imputaciones$Informes)

rm(Extremos, Hechos, Revision, Revision2)
Imputaciones <- Imputaciones %>% dplyr::select(SectorEco, year)

# Calcular las frecuencias y los porcentajes
data_pie <- Imputaciones %>%
  filter(SectorEco != "No aplica") %>%  
  group_by(SectorEco) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Gráficos de pie
ggplot(data_pie, aes(x = "", y = count, fill = SectorEco)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +  
  geom_text(aes(label = sprintf("%.1f%%", percentage)),  
            position = position_stack(vjust = 0.5), 
            size = 4,  
            color = "white",  
            fontface = "bold") +  
  scale_fill_viridis_d() +  
  labs(fill = NULL) +  
  theme(legend.position = "bottom")

rm(data_pie, Imputaciones)

### --- Gráfico de cajas de la sanciones impuestas por imputación (con y sin valores atípicos) --- ###

General <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar, year, Multa_Final)
Hechos <- General %>% filter(is.na(Colapsar))
Hechos <- Hechos %>% dplyr::select(Informes, Num_Imputacion, year, Multa_Final)
Extremos <- General %>%  filter(Colapsar!="NA")

Ext_colaps <- Extremos %>%
  group_by(Informes, Num_Imputacion, year) %>%
  summarize(
    Multa_Final = ifelse(first(Colapsar) == "Suma",
                         sum(Multa_Final, na.rm = TRUE),
                         max(Multa_Final, na.rm = TRUE)), .groups = 'drop')

Hechos_Multas <- rbind(Hechos, Ext_colaps)
rm(Ext_colaps, Hechos, General, Extremos)

Est1 <- Hechos_Multas %>%
  group_by(year) %>%
  summarise(
    minimo = round(min(Multa_Final, na.rm = TRUE), 3),
    Q1 = round(quantile(Multa_Final, 0.25, na.rm = TRUE), 2),
    mediana = round(median(Multa_Final, na.rm = TRUE), 2),
    promedio = round(mean(Multa_Final, na.rm = TRUE), 2),
    Q3 = round(quantile(Multa_Final, 0.75, na.rm = TRUE), 2),
    maximo = round(max(Multa_Final, na.rm = TRUE), 0),
    .groups = 'drop')

#write_xlsx(Est1, "D:/NUEVO D/LOCACION OEFA/Informes/Tercera OS/Informe 5/Graficos/Tabla_Año.xlsx")

# Ahora sin considerar outliers
Filtro <- Hechos_Multas %>%
  group_by(year) %>%
  filter(Multa_Final <= quantile(Multa_Final, 0.90, na.rm = TRUE)) %>%
  ungroup()  

Est2 <- Filtro %>%
  group_by(year) %>%
  summarise(
    mediana = median(Multa_Final, na.rm = TRUE),
    Q1 = quantile(Multa_Final, 0.25, na.rm = TRUE),
    Q3 = quantile(Multa_Final, 0.75, na.rm = TRUE),
    promedio = mean(Multa_Final, na.rm = TRUE),
    .groups = 'drop')

Est3 <- Filtro %>%
  summarise(
    mediana = median(Multa_Final, na.rm = TRUE),
    Q1 = quantile(Multa_Final, 0.25, na.rm = TRUE),
    Q3 = quantile(Multa_Final, 0.75, na.rm = TRUE),
    promedio = mean(Multa_Final, na.rm = TRUE),  
    .groups = 'drop')


# Configurando la ventana gráfica para mostrar 2 gráficos en 1 fila
par(mfrow = c(1, 2))

# Gráfico 1: Distribución de las multas con outliers
boxplot(Hechos_Multas$Multa_Final ~ Hechos_Multas$year,
        main = "Con outliers",
        ylab = "Multa final (en UIT)",
        xlab = " ",
        col = "darkolivegreen4",
        outline = TRUE)

# Gráfico 2: Distribución de las multas sin outliers
boxplot(Filtro$Multa_Final ~ Filtro$year,
        main = "Sin outliers", 
        ylab = "Multa final (en UIT)",
        xlab = " ",
        col = "darkgoldenrod2",
        outline = TRUE)

# Restaurar la configuración de la ventana gráfica a su estado original
par(mfrow = c(1, 1))

######################################################################################
### Aquí va el gráfico de cajas con distribuciones del script "Cajas_Distribucion" ###
######################################################################################

rm(Est1, Est2, Est3, Filtro, Hechos_Multas)

### ---- Distribución de multas por sectores ---- ###
General <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar, year, Multa_Final, SectorEco)
Hechos <- General %>% filter(is.na(Colapsar))
Hechos <- Hechos %>% dplyr::select(Informes, Num_Imputacion, year, Multa_Final,  SectorEco)
Extremos <- General %>% filter(Colapsar!="NA")

Ext_colaps <- Extremos %>%
  group_by(Informes, Num_Imputacion, year, SectorEco) %>%
  summarize(Multa_Final = ifelse(first(Colapsar) == "Suma",
                         sum(Multa_Final, na.rm = TRUE),
                         max(Multa_Final, na.rm = TRUE)), .groups = 'drop')

Hechos_Multas <- rbind(Hechos, Ext_colaps)

rm(Hechos, General, Extremos, Ext_colaps)
Hechos_Multas <- Hechos_Multas %>% dplyr::select(year, Multa_Final, SectorEco)

Est4 <- Hechos_Multas %>%
  group_by(SectorEco) %>%
  summarise(
    Sanciones = sum(!is.na(Multa_Final)),
    minimo = round(min(Multa_Final, na.rm = TRUE), 3),
    Q1 = round(quantile(Multa_Final, 0.25, na.rm = TRUE), 2),
    mediana = round(median(Multa_Final, na.rm = TRUE), 2),
    promedio = round(mean(Multa_Final, na.rm = TRUE), 2),
    Q3 = round(quantile(Multa_Final, 0.75, na.rm = TRUE), 2),
    maximo = round(max(Multa_Final, na.rm = TRUE), 0),
    .groups = 'drop')

#write_xlsx(Est4, "D:/NUEVO D/LOCACION OEFA/Informes/Tercera OS/Informe 5/Graficos/Tabla_Sectores.xlsx")

# Ahora sin considerar outliers
Filtro2 <- Hechos_Multas %>%
           group_by(SectorEco) %>%
           filter(Multa_Final <= quantile(Multa_Final, 0.90, na.rm = TRUE)) %>%
           ungroup() 


# Configurando la ventana gráfica para mostrar 2 gráficos en 1 fila
par(mfrow = c(2, 1))

# Gráfico 1: Distribución de las multas por sectores con outliers
boxplot(Hechos_Multas$Multa_Final ~ Hechos_Multas$SectorEco,
        xlab = "Sector Económico",
        ylab = "Multa final (en UIT)",
        col = "darkolivegreen4",
        main = "Con outliers",
        outline = TRUE,
        cex.axis = 0.65)

# Gráfico 2: Distribución de las multas por sectores sin outliers
boxplot(Filtro2$Multa_Final ~ Filtro2$SectorEco,
        xlab = "Sector Económico",
        ylab = "Multa final (en UIT)",
        col = "darkgoldenrod2",
        main = "Sin outliers",
        outline = TRUE,
        cex.axis = 0.65)

# Restaurar la configuración de la ventana gráfica a su estado original
par(mfrow = c(1, 1))

rm(Est4, Hechos_Multas, Filtro2)

### --- Tabla estadística del Beneficio Ilícito por sector --- ###

General <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar, year, 
                                   Beneficio_ilícito, Multa_Final, SectorEco)
Hechos <- General %>% filter(is.na(Colapsar))
Hechos <- Hechos %>% dplyr::select(Informes, Num_Imputacion, year, Beneficio_ilícito,  SectorEco)
Extremos <- General %>% filter(Colapsar!="NA")

Ext_colaps <- Extremos %>%
  group_by(Informes, Num_Imputacion, year, SectorEco) %>%
  summarize(Beneficio_ilícito = ifelse(first(Colapsar) == "Suma",
                                 sum(Beneficio_ilícito, na.rm = TRUE),
                                 max(Beneficio_ilícito, na.rm = TRUE)), .groups = 'drop')

Hechos_Ilicitos <- rbind(Hechos, Ext_colaps)

Est5 <- Hechos_Ilicitos %>%
  group_by(SectorEco) %>%
  summarise(Min = min(Beneficio_ilícito, na.rm = TRUE),
    Q1 = quantile(Beneficio_ilícito, 0.25, na.rm = TRUE),
    Mediana = median(Beneficio_ilícito, na.rm = TRUE),
    Media = mean(Beneficio_ilícito, na.rm = TRUE),
    Q3 = quantile(Beneficio_ilícito, 0.75, na.rm = TRUE),
    Max = max(Beneficio_ilícito, na.rm = TRUE))

# Ver la tabla generada
#write_xlsx(Est5, "D:/NUEVO D/LOCACION OEFA/Informes/Tercera OS/Informe 5/Graficos/Tabla_beneficio.xlsx")

# Grafico de cajas del beneficio ilicito por sector

BI_Sectores <- Hechos_Ilicitos %>%
  distinct(SectorEco, Beneficio_ilícito)

Filtro3 <- BI_Sectores %>%
  group_by(SectorEco) %>%
  filter(Beneficio_ilícito <= quantile(Beneficio_ilícito, 0.90, na.rm = TRUE)) %>%
  ungroup() 

# Ajusta los márgenes (c(bottom, left, top, right))
par(mfrow = c(2, 1), mar = c(3, 4, 2, 1)) 

# Gráfico de cajas con outliers por sectores
boxplot(Beneficio_ilícito ~ SectorEco, data = BI_Sectores,
        main = "Con outliers",
        xlab = "Sector Económico", ylab = "Beneficio ilícito (en UIT)",
        col = "lightblue", border = "black", outlier.colour = "black",
        cex.axis = 0.55)  

# Gráfico de cajas sin outliers por sectores
boxplot(Beneficio_ilícito ~ SectorEco, data = Filtro3,
        main = "Sin outliers",
        xlab = "Sector Económico", ylab = "Beneficio ilícito (en UIT)",
        col = "lightblue", border = "black", outlier.colour = "black",
        cex.axis = 0.55)  

# Restablece la configuración de gráficos
par(mfrow = c(1, 1))

rm(BI_Sectores, Est5, Ext_colaps, Extremos, Filtro3, General, Hechos, Hechos_Ilicitos)

### --- Gráfico de Tiempo Promedio --- ###

# Convertir las variables de fechas al formato Date (si no lo están ya)

Fechas <- FINAL %>% dplyr::select("SectorEco", "FinSup", "InicioPAS", "Fecha_Informe", "year",
                                  "Informes", "Num_Imputacion", "Colapsar")

Fechas$FinSup <- as.numeric(Fechas$FinSup)
Fechas$FinSup <- as.Date(Fechas$FinSup, origin = "1899-12-30")
FINAL$InicioPAS <- as.Date(FINAL$InicioPAS, format="%Y-%m-%d")
FINAL$Fecha_Informe <- as.Date(FINAL$Fecha_Informe, format="%Y-%m-%d")


Hechos <- Fechas %>% filter(is.na(Colapsar))
Extremos <- Fechas %>% filter(Colapsar!="NA")

Colaps <- Extremos %>%
  group_by(Num_Imputacion, Informes) %>%
  slice(1) %>%  
  ungroup()

Fechas_Hechos <- rbind(Hechos, Colaps)
rm(Colaps, Hechos, Extremos, Fechas)


# Calcular las diferencias en meses entre las fechas

# Periodo de incumplimiento: Del fin de la supervisión (FinSup) al Inicio de PAS (InicioPAS)
# Determinación de la multa: De Inicio de PAS (InicioPAS) a Informe de Sanción (Fecha_Informe)

# De FinSup a InicioPAS
Fechas_Hechos$Meses_FinSup_InicioPAS <- as.numeric(difftime(Fechas_Hechos$InicioPAS, 
                                                            Fechas_Hechos$FinSup, units = "days")) / 30.44

# De InicioPAS a Fecha_Informe
Fechas_Hechos$Meses_InicioPAS_FechaInforme <- as.numeric(difftime(Fechas_Hechos$Fecha_Informe, 
                                                           Fechas_Hechos$InicioPAS, units = "days")) / 30.44

# Calcular los promedios de cada transición
mediana_FinSup_InicioPAS <- median(Fechas_Hechos$Meses_FinSup_InicioPAS, na.rm = TRUE)
promedio_FinSup_InicioPAS <- mean(Fechas_Hechos$Meses_FinSup_InicioPAS, na.rm = TRUE)
mediana_InicioPAS_FechaInforme <- median(Fechas_Hechos$Meses_InicioPAS_FechaInforme, na.rm = TRUE)
promedio_InicioPAS_FechaInforme <- mean(Fechas_Hechos$Meses_InicioPAS_FechaInforme, na.rm = TRUE)


# percentil 90

# Calcular el percentil 90 para ambos periodos
percentil_90_FinSup_InicioPAS <- quantile(Fechas_Hechos$Meses_FinSup_InicioPAS, 0.90, na.rm = TRUE)
percentil_90_InicioPAS_FechaInforme <- quantile(Fechas_Hechos$Meses_InicioPAS_FechaInforme, 0.90, na.rm = TRUE)

# Filtrar los datos excluyendo los outliers por encima del percentil 90
Fechas_Filtrado <- Fechas_Hechos %>%
  filter(Meses_FinSup_InicioPAS <= percentil_90_FinSup_InicioPAS,
         Meses_InicioPAS_FechaInforme <= percentil_90_InicioPAS_FechaInforme)

# Configurar la ventana gráfica para mostrar 2 gráficos en 1 fila
par(mfrow = c(1, 2))

# Gráfico 1: Periodo de Incumplimiento (FinSup a InicioPAS)
boxplot(Fechas_Hechos$Meses_FinSup_InicioPAS,
        main = "Periodo 1",
        ylab = "Meses",
        col = "lightblue")

# Gráfico 2: Determinación de la multa (InicioPAS a Informe de Sanción)
boxplot(Fechas_Hechos$Meses_InicioPAS_FechaInforme,
        main = "Periodo 2",
        ylab = "Meses",
        col = "lightgreen")

# Restaurar la configuración de la ventana gráfica a su estado original
par(mfrow = c(1, 1))

# Crear un data frame con los resultados
promedios <- data.frame(
  Transicion = c("Periodo 1", "Periodo 2"),
  Meses_Promedio = c(promedio_FinSup_InicioPAS, promedio_InicioPAS_FechaInforme))

promedios$ID <- seq(1, nrow(promedios))

# Tiempo promedio entre fechas en meses
promedios$Transicion <- factor(promedios$Transicion, levels = promedios$Transicion[order(promedios$ID)])

# Gráfico con el nuevo orden
ggplot(promedios, aes(x = Transicion, y = Meses_Promedio, fill = Transicion)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  labs(x = "Transición", y = "Meses promedio") +
  theme_minimal() +
  scale_fill_manual(values = c("lightcoral", "skyblue")) +
  geom_text(aes(label = round(Meses_Promedio, 0)), vjust = -0.5) +
  theme(legend.position = "none")

# Calcular los máximo de cada transición
max_FinSup_InicioPAS <- max(Fechas_Hechos$Meses_FinSup_InicioPAS, na.rm = TRUE)
max_InicioPAS_FechaInforme <- max(Fechas_Hechos$Meses_InicioPAS_FechaInforme, na.rm = TRUE)

# Crear un data frame con los resultados
maximos <- data.frame(
  Transicion = c("Periodo de incumplimiento", "Determinación de multa"),
  Meses_max = c(max_FinSup_InicioPAS, max_InicioPAS_FechaInforme))

maximos$ID <- seq(1, nrow(maximos))

# Tiempo promedio entre fechas en meses
maximos$Transicion <- factor(maximos$Transicion, levels = maximos$Transicion[order(maximos$ID)])

# Gráfico con el nuevo orden
ggplot(maximos, aes(x = Transicion, y = Meses_max, fill = Transicion)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  labs(x = "Transición", y = "Meses máximos") +
  theme_minimal() +
  scale_fill_manual(
    values = c("lightcoral", "skyblue")) +
  geom_text(aes(label = round(Meses_max, 0)), vjust = -0.5) +
  theme(
    legend.position = "bottom",           
    legend.title = element_blank()) +
  guides(fill = guide_legend(title = NULL))

# A nivel de sector económico

# Calculando los promedios de cada transición por sector económico
promedios_sector <- Fechas %>%
  group_by(SectorEco) %>%
  summarise(
    Promedio_Sup_PAS = mean(Meses_FinSup_InicioPAS, na.rm = TRUE),
    Promedio_PAS_Inf = mean(Meses_InicioPAS_FechaInforme, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("Promedio"),
    names_to = "Transicion",
    values_to = "Meses_Promedio"
  )

ggplot(promedios_sector, aes(x = SectorEco, y = Meses_Promedio, fill = Transicion)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6, color = "black") +
  labs(x = "Sector Económico", y = "Meses promedio") +
  theme_minimal() +
  scale_fill_manual(
    values = c("skyblue", "lightcoral"),
    labels = c("Determinación de multa", "Periodo de incumplimiento")) +  
  geom_text(aes(label = round(Meses_Promedio, 0)), vjust = -0.5, position = position_dodge(0.6)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()) +
  guides(fill = guide_legend(title = NULL))

# Calculando los máximos de cada transición por sector económico
Max_sector <- Fechas %>%
  group_by(SectorEco) %>%
  summarise(
    Max_Sup_PAS = max(Meses_FinSup_InicioPAS, na.rm = TRUE),
    Max_PAS_Inf = max(Meses_InicioPAS_FechaInforme, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with("Max"),
    names_to = "Transicion",
    values_to = "Meses_max"
  )

ggplot(Max_sector, aes(x = SectorEco, y = Meses_max, fill = Transicion)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6, color = "black") +
  labs(x = "Sector Económico", y = "Meses máximos") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightcoral"),
  labels = c("Determinación de multa", "Periodo de incumplimiento")) +  
  geom_text(aes(label = round(Meses_max, 0)), vjust = -0.5, position = position_dodge(0.6)) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(title = NULL))


### --- Probabilidad de detección por más frecuente --- ###

Deteccion <- FINAL %>%
  count(Prob_Detección)

ggplot(Deteccion, aes(x = Prob_Detección, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = n), vjust = -0.5) +  
  labs(x = "Probabilidad de Detección",
       y = "Frecuencia") +
  theme_minimal()

Detec_Sector <- FINAL %>%
  count(SectorEco, Prob_Detección)

ggplot(Detec_Sector, aes(x = factor(Prob_Detección), y = n, fill = SectorEco)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Probabilidad de Detección",
       y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

### --- Informes excluidos --- ###

rm(list = ls())

years <- c(2022, 2023, 2024)
Factores <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Factores/Copia%20de%20Informes_"
for (year in years) {
  url <- paste0(Factores, year, ".xlsx")
  temp_file <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  assign(paste0("G", year), read_excel(temp_file, sheet = "Componentes"))
  rm(temp_file, url)
}
rm(Factores, year, years)

#G2022 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2022.xlsx", sheet = "Componentes")
#G2023 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2023.xlsx", sheet = "Componentes")
#G2024 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2024.xlsx", sheet = "Componentes")

G2022 <- G2022 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")
G2023 <- G2023 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")
G2024 <- G2024 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")

G2022$Año <- 2022
G2023$Año <- 2023
G2024$Año <- 2024

# Compilando las bases 
FINAL <- rbind(G2022, G2023, G2024)

# Trayendo el DFUNIDO
#Consolidado <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/dfunido.xlsx", sheet = "Sheet 1")
url1 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/dfunido.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url1, write_disk(temp_file, overwrite = TRUE))
Consolidado <- read_excel(temp_file, sheet = "Sheet 1")  
rm(temp_file, url1)

Consolidado <- Consolidado %>% dplyr::select("Informes", "Propuesta_Multa")
Consolidado <- Consolidado %>%
  filter(Propuesta_Multa == "si")

#Quitando duplicados
Unicos <- Consolidado[!duplicated(Consolidado$Informes), ]

# Fusionando bases
Fusion <-left_join(x = FINAL, y = Unicos, by="Informes")

# Filtrando
Excluidos <- Fusion %>%
  filter(Hecho_imputado %in% c("Reconsideración", "Multa coercitiva", 
                               "multas coercitivas", "Enmienda multa coercitiva",
                               "Medida correctiva", "Informe de enmienda") |
           Propuesta_Multa %in% c("si"))


Unicos <- Excluidos[!duplicated(Excluidos$Informes), ]

Completo <- Unicos %>%
  mutate(Hecho_imputado = case_when(
    Propuesta_Multa == "si" ~ "Propuesta de cálculo de multa",
    TRUE ~ Hecho_imputado))

rm(Unicos, Excluidos, FINAL, Fusion, G2022, G2023, G2024, Consolidado)

# Seleccionado variables
Completo <- Completo %>% dplyr::select("Año","Hecho_imputado")

Completo$Unos <- 1

names(Completo)

Colapsado <- Completo %>%
  group_by(Año, Hecho_imputado) %>%
  summarise(Total = sum(Unos, na.rm = TRUE)) %>%
  ungroup()

rm(Completo)

# Gráfico de excluidos
ggplot(Colapsado, aes(x = Año, y = Total, fill = Hecho_imputado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Total), vjust = -0.5, position = position_dodge(0.9), size = 3.5) +
  scale_fill_viridis_d() +  
  labs(x = "Año",
       y = "Total") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

# Tabla resumen

knitr::kable(head(Colapsado))

