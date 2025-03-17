
require(pacman)
p_load(foreign, tidyverse, rio, here, dplyr, viridis, readxl, stringr, RColorBrewer, ggcorrplot,  
        flextable, officer, classInt, foreign, stargazer, sf, mapview, leaflet, writexl, lmtest,
       tseries, car, haven, officer, xlsx, openxlsx, httr)

rm(list = ls())

###################################
###### Informes de sanción ########
###################################

# Carga de las bases antiguas
url1 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20Finales/Informes_2022.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url1, write_disk(temp_file, overwrite = TRUE))
Consolidado1 <- read_excel(temp_file, sheet = "Componentes")  
Consolidado1$Año <- 2022
rm(temp_file, url1)

url2 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20Finales/Informes_2023.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url2, write_disk(temp_file, overwrite = TRUE))
Consolidado2 <- read_excel(temp_file, sheet = "Componentes")  
Consolidado2$Año <- 2023
rm(temp_file, url2)

url3 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20Finales/Informes_2024.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url3, write_disk(temp_file, overwrite = TRUE))
Consolidado3 <- read_excel(temp_file, sheet = "Componentes")  
Consolidado3$Año <- 2024
rm(temp_file, url3)

# Carga de las bases nuevas
url4 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20Finales/Info_2022.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url4, write_disk(temp_file, overwrite = TRUE))
Consolidado4 <- read_excel(temp_file, sheet = "Consolidado")  
Consolidado4$Año <- 2022
rm(temp_file, url4)

url5 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20Finales/Info_2023.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url5, write_disk(temp_file, overwrite = TRUE))
Consolidado5 <- read_excel(temp_file, sheet = "Consolidado")  
Consolidado5$Año <- 2023
rm(temp_file, url5)

url6 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20Finales/Info_2024.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url6, write_disk(temp_file, overwrite = TRUE))
Consolidado6 <- read_excel(temp_file, sheet = "Consolidado")  
Consolidado6$Año <- 2024
rm(temp_file, url6)

# Carga de información de RUIAS
url7 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20sustento/RUIAS-CSEP.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url7, write_disk(temp_file, overwrite = TRUE))
RUIAS <- read_excel(temp_file, sheet = "RUIAS")  
rm(temp_file, url7)


# Consolidando la información
Antigua <- rbind(Consolidado1, Consolidado2, Consolidado3)
rm(Consolidado1, Consolidado2, Consolidado3)

Nueva <- rbind(Consolidado4, Consolidado5, Consolidado6)
rm(Consolidado4, Consolidado5, Consolidado6)

# Filtrando la información necesaria
Antigua <- Antigua %>% 
  filter(Detalle != "Eliminar" | is.na(Detalle))

Antigua <- Antigua %>% 
  filter(Filtro=="Cálculo de multa")

Nueva <- Nueva %>% 
  filter(Filtro=="Cálculo de multa")

# Edición de base Antigua
colnames(Antigua)[colnames(Antigua) == "Sub_extremo"] <- "Extremo"
Antigua <- Antigua %>% select(-Detalle, -Observaciones)
Antigua$Datos <- "Primera fase"

# Edición de base Nueva
Nueva <- Nueva %>% select(-"Sub extremo")
Nueva$Datos <- "Segunda fase"

# Append principal
CFinal <- rbind(Antigua, Nueva)

# Ediciones extra
colnames(CFinal)[colnames(CFinal) == "ID"] <- "Index"
CFinal$Index <- as.character(CFinal$Index)
colnames(CFinal)[colnames(CFinal) == "Expedientes"] <- "Expediente"

rm(Antigua, Nueva)



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
CFinal$Index[is.na(CFinal$Index)] <- 0
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

# Fusionando el Tipo de Empresa (restante: Index 7020)
url8 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Bases/Exceles/Tipo_Empresas.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url8, write_disk(temp_file, overwrite = TRUE))
Tamaño <- read_excel(temp_file, sheet = "Final")  
rm(temp_file, url8)
FINAL <-left_join(x = FINAL, y = Tamaño, by="Administrado")
rm(Tamaño)
#View(FINAL[is.na(FINAL$tipo_actividad), ])
FINAL$tipo_actividad <- ifelse(FINAL$Index == "7020", "actividad empresarial", FINAL$tipo_actividad)
FINAL$tipo_persona <- ifelse(FINAL$Index == "7020", "persona jurídica", FINAL$tipo_persona)
table(FINAL$tipo_actividad)
FINAL$Colapsar <- ifelse(FINAL$Colapsar == "Máximo", "Maximo", FINAL$Colapsar)
table(FINAL$Colapsar)

###################################
###### Fechas de informes #########
###################################

# Bucle para descargar y leer los archivos por año
years <- c(2022, 2023, 2024)
for (year in years) {
  url <- paste0("https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Fechas/Fecha_", year, ".xlsx")
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
  url <- paste0("https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Fechas/Fecha_", year, "b.xlsx")
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

years <- c(2022, 2023, 2024)
Factores <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20Finales/Informes_"
for (year in years) {
  url <- paste0(Factores, year, ".xlsx")
  temp_file <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  assign(paste0("G", year), read_excel(temp_file, sheet = "Graduacion"))
  rm(temp_file, url)
}

rm(Factores, year, years)

# Quitando de las bases las categorías a no emplear

G2022$Observaciones <- NULL 
G2024$Observaciones <- NULL 

# Haciendo un append
Aglomerado1 <- rbind(G2022, G2023, G2024)
rm(G2022, G2023, G2024)


table(Aglomerado1$Detalle)
Aglomerado1 <- Aglomerado1 %>% 
  filter(Detalle != "Eliminar")


table(Aglomerado1$Filtro)
Aglomerado1 <- Aglomerado1 %>% 
  filter(Filtro=="Cálculo de multa")

# Selecionando las variables a emplear
Aglomerado1 <- Aglomerado1 %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")
table(Aglomerado1$Factores_agravantes, useNA = "ifany")


# Importando Los nuevos Factores de la segunda tanda de informes
years <- c(2022, 2023, 2024)
Factores <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/C%C3%B3digo%20Final/Bases%20Finales/Info_"
for (year in years) {
  url <- paste0(Factores, year, ".xlsx")
  temp_file <- tempfile(fileext = ".xlsx")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  assign(paste0("G", year), read_excel(temp_file, sheet = "Graduación"))
  rm(temp_file, url)
}
rm(Factores, year, years)

Aglomerado2 <- rbind(G2022, G2023, G2024)
rm(G2022, G2023, G2024)

table(Aglomerado2$Filtro)
Aglomerado2 <- Aglomerado2 %>% 
  filter(Filtro=="Cálculo de multa")

colnames(Aglomerado2)[colnames(Aglomerado2) == "Correlativo"] <- "ID"
Aglomerado2 <- Aglomerado2 %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")

Aglomerado1$Datos <- "Primera fase"
Aglomerado2$Datos <- "Segunda fase"

# Combinando los factores
FACTORES <- rbind(Aglomerado1, Aglomerado2)
rm(Aglomerado1, Aglomerado2)

FACTORES <- FACTORES %>%
  mutate(`% FA` = ifelse(is.na(Categoria_FA), 0, `% FA`))

###################################
###### Exportando las bases #######
###################################

FINAL <- FINAL %>% dplyr::select(-c("Hecho_imputado"))

wb <- createWorkbook()

# Añadiendo la primera hoja con el primer dataframe
addWorksheet(wb, "Informes")
writeData(wb, "Informes", FINAL, colNames = TRUE)

# Añadiendo la segunda hoja con el segundo dataframe
addWorksheet(wb, "Factores")
writeData(wb, "Factores", FACTORES, colNames = TRUE)

# Guardardando el archivo Excel
saveWorkbook(wb, "D:/LAPTOP ACER/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Código Final/Bases Finales/INFORMES_GRADUACION.xlsx", overwrite = TRUE)

#Hasta acá revisar

###################################
#### Estadísticas Descriptivas ####
###################################

NOMERGE <- FINAL %>%
  filter(Merge == 0)

MERGE <- FINAL %>%
  filter(Merge == 1)

# Colores institucionales
#col = c("#144AA7", "#1d85bf", "#0BC7E0", "#44bfb5", "#8CCD3A", "#FFB500","#696a6a")

### --- Gráfico de barras de factores --- ###
Filtro <- FACTORES[FACTORES$Factores_agravantes %in% c("F1", "F2", "F3", "F5", "F6"), ]
table(Filtro$Factores_agravantes, useNA = "ifany")

ggplot(Filtro, aes(x = Factores_agravantes)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  labs(x = " ",
       y = "Factores atenuantes y agravantes") +
  theme_minimal()

rm(Filtro)

### --- Total de informes de cálculo de multa y hechos imputados (con RUIAS) --- ###
Extremos <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar, year)

Revision1 <- Extremos %>% 
  filter(Colapsar!="NA")

Rev1 <- Revision1 %>%
  distinct(Informes, Num_Imputacion)

Revision2 <- Extremos %>% 
  filter(is.na(Colapsar))

Rev2 <- Revision2 %>%
  distinct(Informes, Num_Imputacion)

Revs <- rbind(Rev1, Rev2)
Revs <- Revs %>% dplyr::select(Informes)
Revs$year <- sub(".*-(\\d{4}).*", "\\1", Revs$Informes)

cRevs <- Revs %>%
  group_by(year) %>%
  summarise(Hechos_Imputados = n())

Unique <- FINAL %>%
  distinct(year, Informes)

# Conteo de informes por año en el objeto Unique
cUnique <- Unique %>%
  group_by(year) %>%
  summarise(Informes = n_distinct(Informes))

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
  scale_fill_manual(values = c("Hechos_Imputados" = "#144AA7", "Informes" = "#8CCD3A"),
                    labels = c("Hechos Imputados", "Informes")) +
  theme_minimal() +
  theme(legend.position = "bottom")

rm(Grafico, Glong, Rev1, Rev2, Revision1, Revision2, Revs, Extremos, cRevs, cUnique, Unique)

### --- Infracciones analizadas por sectores para el período de 2022 a 2024 --- ###

Extremos <- MERGE %>% dplyr::select(Informes, Num_Imputacion, Colapsar, SectorEco)

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

#col = c("#144AA7", "#1d85bf", "#0BC7E0", "#44bfb5", "#8CCD3A", "#FFB500","#696a6a")

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

ggplot(data_pie, aes(x = "", y = count, fill = SectorEco)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +  
  geom_text(aes(label = sprintf("%.1f%%", percentage), 
                x = 1.6),  
            position = position_stack(vjust = 0.5), 
            size = 4,  
            color = "black",  
            fontface = "bold") +  
  scale_fill_manual(values = c("#144AA7", "#4D4D4D", "#0BC7E0", "#44bfb5", "#8CCD3A", "#FFB500", "#696a6a", "#1d85bf")) +  
  labs(fill = NULL) +  
  theme(legend.position = "bottom")



rm(data_pie, Imputaciones)

### --- Gráfico de cajas de la sanciones impuestas por imputación (con y sin valores atípicos) --- ###

General <- MERGE %>% dplyr::select(Informes, Num_Imputacion, Colapsar, year, Multa_Final)
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

#col = c("#144AA7", "#1d85bf", "#0BC7E0", "#44bfb5", "#8CCD3A", "#FFB500","#696a6a")
# Configurando la ventana gráfica para mostrar 2 gráficos en 1 fila
par(mfrow = c(1, 2))

# Gráfico 1: Distribución de las multas con outliers
boxplot(Hechos_Multas$Multa_Final ~ Hechos_Multas$year,
        main = "Con outliers",
        ylab = "Multa final (en UIT)",
        xlab = " ",
        col = "#8CCD3A",
        outline = TRUE)

# Gráfico 2: Distribución de las multas sin outliers
boxplot(Filtro$Multa_Final ~ Filtro$year,
        main = "Sin outliers", 
        ylab = "Multa final (en UIT)",
        xlab = " ",
        col = "#FFB500",
        outline = TRUE)

# Restaurar la configuración de la ventana gráfica a su estado original
par(mfrow = c(1, 1))

######################################################################################
### Aquí va el gráfico de cajas con distribuciones del script "Cajas_Distribucion" ###
######################################################################################

rm(Est1, Est2, Est3, Filtro, Hechos_Multas)

### ---- Distribución de multas por sectores ---- ###
General <- MERGE %>% dplyr::select(Informes, Num_Imputacion, Colapsar, year, Multa_Final, SectorEco)
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

General <- MERGE %>% dplyr::select(Informes, Num_Imputacion, Colapsar, year, 
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

Fechas <- MERGE %>% dplyr::select("SectorEco", "FinSup", "InicioPAS", "Fecha_Informe", "year",
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
    values_to = "Meses_max")

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

# General
Deteccion <- FINAL %>%
  count(Prob_Detección) %>%
  mutate(Prob_Detección = factor(Prob_Detección, levels = c(0.1, 0.5, 0.75, 1)))

ggplot(Deteccion, aes(x = Prob_Detección, y = n)) +
  geom_bar(stat = "identity", fill = "#0BC7E0", color = "black") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(x = "Probabilidad de Detección",
       y = "Frecuencia") +
  theme_minimal()

# Por año
Detec_año <- MERGE %>%
  count(year, Prob_Detección) %>%
  mutate(Prob_Detección = factor(Prob_Detección, levels = c(0.1, 0.5, 0.75, 1)))

ggplot(Detec_año, aes(x = Prob_Detección, y = n, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = n), position = position_dodge(0.9), vjust = -0.5) +
  labs(x = "Probabilidad de Detección",
       y = "Frecuencia",
       fill = "Año") +
  scale_fill_manual(values = c("2022" = "#a6cee3", 
                               "2023" = "#1f78b4", 
                               "2024" = "#08306b")) + 
  theme_minimal() +
  theme(legend.position = "bottom")

Detec_nivel <- FINAL %>%
  count(Prob_Detección, year) %>%
  mutate(Prob_Detección = factor(Prob_Detección, levels = c(0.5, 0.75, 1))) 

Detec_nivel <- Detec_nivel %>% 
  filter(!is.na(Prob_Detección))

#col = c("#144AA7", "#1d85bf", "#0BC7E0", "#44bfb5", "#8CCD3A", "#FFB500","#696a6a")

ggplot(Detec_nivel, aes(x = factor(year), y = n, fill = Prob_Detección)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = n), position = position_dodge(0.9), vjust = -0.5) +
  labs(x = "Año",
       y = "Frecuencia",
       fill = NULL) +
  scale_fill_manual(values = c("0.5" = "#8CCD3A", 
                               "0.75" = "#0BC7E0", 
                               "1" = "#144AA7")) + 
  theme_minimal() +
  theme(legend.position = "bottom")

# Por sector económico
Detec_Sector <- MERGE %>%
  count(SectorEco, Prob_Detección)


sector_colors <- c("Agricultura" = "white", "Electricidad" = "#a6cee3","Industria" = "#6baed6",  
                   "Pesquería" = "#3182bd", "Consultoras ambientales" = "#1f78b4", 
                   "Hidrocarburos" = "#4a6491", "Minería" = "#08519c", "Residuos sólidos" = "#08306b") 

ggplot(Detec_Sector, aes(x = factor(Prob_Detección), y = n, fill = SectorEco)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Probabilidad de Detección",
       y = "Frecuencia",
       fill = "Sector Económico") +
  scale_fill_manual(values = sector_colors) + 
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

G2022 <- G2022 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")
G2023 <- G2023 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")
G2024 <- G2024 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")

G2022$Año <- 2022
G2023$Año <- 2023
G2024$Año <- 2024

# Compilando las bases 
FINAL <- rbind(G2022, G2023, G2024)

# Trayendo el DFUNIDO
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


#Exportando las categorías de la nueva fase
url1 <- "https://raw.githubusercontent.com/PaoloValcarcel/OEFA_SMER/main/Paolo/Scripts/Bases/Informes_2daFase.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url1, write_disk(temp_file, overwrite = TRUE))
Fase_2 <- read_excel(temp_file, sheet = "2da_Fase")  
rm(temp_file, url1)

Fase_2 <- Fase_2 %>%
  filter(Observaciones != "Calculo de Multa")
colnames(Fase_2)[colnames(Fase_2) == "Observaciones"] <- "Hecho_imputado"

# Seleccionado variables
Completo <- Completo %>% dplyr::select("Año","Hecho_imputado")

# Juntando ambas fases
Fases <- rbind(Completo, Fase_2)
Fases$Hecho_imputado <- gsub("Propuesta de cálculo de multa", "Propuesta cálculo de multa", Fases$Hecho_imputado)
Fases$Unos <- 1

Fases2 <- Fases %>%
  group_by(Año, Hecho_imputado) %>%
  summarise(Total = sum(Unos, na.rm = TRUE)) %>%
  ungroup()

rm(Completo, Fase_2)

#col = c("#144AA7", "#1d85bf", "#0BC7E0", "#44bfb5", "#8CCD3A", "#FFB500","#696a6a")
# Gráfico de excluidos
ggplot(Fases2, aes(x = Año, y = Total, fill = Hecho_imputado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Total), vjust = -0.5, position = position_dodge(0.9), size = 3.5) +
  scale_fill_manual(values = c("#FFB500", "#8CCD3A", "#44bfb5", "#1d85bf", "#144AA7")) +  
  labs(x = "Año",
       y = "Total") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

# Tabla resumen
Fases3 <- Fases %>%
  group_by(Hecho_imputado) %>%
  summarise(Total = sum(Unos, na.rm = TRUE)) %>%
  ungroup()

knitr::kable((Fases3))


### ----- Gráfico de incumplimientos por informes ----- ### 

cFINAL <- MERGE %>%
  mutate(Incumplimientos = case_when(
    Incumplimiento_11 == 'Incumplimiento de Límites Máximos Permisibles en efluentes' ~ 'Incumplimiento LMP',
    Incumplimiento_11 == 'Incumplimiento de Límites Máximos Permisibles en emisiones' ~ 'Incumplimiento LMP',
    Incumplimiento_11 == 'Incumplimiento del Instrumento de Gestión Ambiental' ~ 'Incumplimiento IGA',
    Incumplimiento_11 == 'No contar con Instrumentos de Gestión Ambiental' ~ 'Incumplimiento IGA',
    Incumplimiento_11 == 'Incumplimiento de medidas administrativas (medidas cautelares, medidas correctivas y preventivas)' ~ 'Incumplimiento de med. administrativas',
    Incumplimiento_11 == 'No brindar información, presentar información inexacta o fuera de plazo' ~ 'No presentó información',
    Incumplimiento_11 == 'No efectuar monitoreos (en el plazo, alcance y/o frecuencia)' ~ 'No efectuar monitoreos',
    Incumplimiento_11 == 'Obstaculizar o impedir labores de supervisión y/o fiscalización' ~ 'Obstaculizar o impedir labores',
    Incumplimiento_11 == 'Incumplimiento de normas de residuos sólidos' ~ 'Incumplimiento de residuos sólidos',
    TRUE ~ Incumplimiento_11))

table(cFINAL$Incumplimientos)

Inc <- cFINAL %>% dplyr::select("Informes", "Num_Imputacion", "Incumplimientos")

Inc <- Inc %>% 
  filter(Incumplimientos!="NA")

Unicos <- Inc %>%
  distinct(Informes, Num_Imputacion, Incumplimientos)

Informes <- Unicos %>%
  group_by(Incumplimientos) %>%
  summarise(Frecuencia = n(), .groups = "drop")

ggplot(Informes, aes(x = Incumplimientos, y = Frecuencia, fill = as.factor(Incumplimientos))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Frecuencia), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  labs(x = "Incumplimientos", 
       y = "Frecuencia", 
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", 
    axis.text.x = element_blank() ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
  scale_fill_viridis_d(option = "viridis")

rm(Inc, Unicos, Informes)

###----- Gráfico de incumplimientos por factores de graduación -----###

# Para las categorías NA
SFac <- FACTORES %>% 
  filter(is.na(Categoria_FA))

# Para los factores del F1 al F6
SFac <- FACTORES %>%
  filter(Factores_agravantes %in% c("F1", "F2", "F3", "F4", "F5", "F6"))
colnames(SFac)[colnames(SFac) == "Imputacion"] <- "Num_Imputacion"

#duplicados <- SFac %>%
#  group_by(Informes, Num_Imputacion, Factores_agravantes) %>%
#  filter(n() > 1) %>%
#  ungroup()

SFac <- SFac %>% dplyr::select("Informes", "Num_Imputacion", "Factores_agravantes")

SFac <- SFac %>%
  distinct(Informes, Num_Imputacion, Factores_agravantes)

CRuias <- cFINAL %>% dplyr::select("Informes", "Num_Imputacion", "Incumplimientos")

Unicos <- CRuias %>%
  distinct(Informes, Num_Imputacion, Incumplimientos)

Fusion <- left_join(x = SFac, y = Unicos, by = c("Informes", "Num_Imputacion"))

Fusion <- Fusion %>%
  distinct(Informes, Num_Imputacion, Factores_agravantes, .keep_all = TRUE)

duplicados <- Fusion %>%
  group_by(Informes, Num_Imputacion, Factores_agravantes) %>%
  filter(n() > 1) %>%
  ungroup()

Fusion <- Fusion %>% 
  filter(!is.na(Incumplimientos))

Factores <- Fusion %>%
  group_by(Incumplimientos) %>%
  summarise(Frecuencia = n(), .groups = "drop")

ggplot(Factores, aes(x = Incumplimientos, y = Frecuencia, fill = as.factor(Incumplimientos))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Frecuencia), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  labs(x = "Incumplimientos", 
       y = "Frecuencia", 
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_blank() ) +
  guides(fill = guide_legend(nrow = 4, byrow = TRUE)) + 
  scale_fill_viridis_d(option = "viridis")

rm(SFac, CRuias, Unicos, Fusion, Factores)


###---- Gráfico del impacto de los factores acumulado -----###

Eliminar <- c("F.1.1", "F.1.2", "F.1.3", "F.1.4", "F.1.7",
              "F1 1.1", "F1 1.2", "F1 1.3", "F1 1.4", "F1 1.5",
              "F1 1.6", "F1 1.7")

# Filtrando solo los F1 al F7
Facts <- FACTORES %>%
  filter(!is.na(Factores_agravantes), 
         !Factores_agravantes %in% Eliminar)

table(Facts$Factores_agravantes)
colnames(Facts)[colnames(Facts) == "Imputacion"] <- "Num_Imputacion"

# Colapsando la suma de los factores por Informes y número de imputación
Colapsado <- Facts %>%
  group_by(Informes, Num_Imputacion) %>%
  summarise(`% FA` = sum(`% FA`, na.rm = TRUE), .groups = "drop")


# Sacando la info de la base de informes (FINAL)
Extremos <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar,
                                    Beneficio_ilícito, Prob_Detección, Datos)

Revision1 <- Extremos %>% 
  filter(Colapsar!="NA")

Revision2 <- Extremos %>% 
  filter(is.na(Colapsar))

Rev1 <- Revision1 %>%
  group_by(Informes, Num_Imputacion) %>%  
  summarise(
    Beneficio_ilícito = max(Beneficio_ilícito, na.rm = TRUE),  
    Prob_Detección = max(Prob_Detección, na.rm = TRUE)) %>%
  ungroup()

Rev2 <- Revision2 %>% dplyr::select(Informes, Num_Imputacion, Beneficio_ilícito, Prob_Detección)

Revs <- rbind(Rev1, Rev2)
rm(Rev1, Rev2, Revision1, Revision2, Extremos, Facts)

Grafico <- left_join(x = Colapsado, y = Revs, by = c("Informes", "Num_Imputacion"))
rm(Colapsado, Revs)

Grafico$Efecto_Factores <- Grafico$Beneficio_ilícito*Grafico$`% FA`
Grafico$Multa <- round(Grafico$Beneficio_ilícito / Grafico$Prob_Detección,1) * (1 + Grafico$`% FA`)
Grafico$Efecto_Prob <- Grafico$Multa*(1-Grafico$Prob_Detección)
Grafico$Comrpobacion<- Grafico$Beneficio_ilícito + Grafico$Efecto_Factores + Grafico$Efecto_Prob 

# Quitando outliers
P90 <- Grafico %>%
  summarise(p90_Beneficio_ilícito = quantile(Beneficio_ilícito, 0.9, na.rm = TRUE),
            p90_Efecto_Prob = quantile(Efecto_Prob, 0.9, na.rm = TRUE),
            p90_Efecto_Factores = quantile(Efecto_Factores, 0.9, na.rm = TRUE))

# Eliminando los outliers (valores por encima del percentil 90)
Filtro <- Grafico %>%
  filter(Beneficio_ilícito <= P90$p90_Beneficio_ilícito,
         Efecto_Prob <= P90$p90_Efecto_Prob,
         Efecto_Factores <= P90$p90_Efecto_Factores)

# Cuartiles de cada variable
cuartiles <- Filtro %>%
  summarise(
    q25_Beneficio_ilícito = quantile(Beneficio_ilícito, 0.25, na.rm = TRUE),
    q50_Beneficio_ilícito = quantile(Beneficio_ilícito, 0.5, na.rm = TRUE),
    q75_Beneficio_ilícito = quantile(Beneficio_ilícito, 0.75, na.rm = TRUE),
    
    q25_Efecto_Prob = quantile(Efecto_Prob, 0.25, na.rm = TRUE),
    q50_Efecto_Prob = quantile(Efecto_Prob, 0.5, na.rm = TRUE),
    q75_Efecto_Prob = quantile(Efecto_Prob, 0.75, na.rm = TRUE),
    
    q25_Efecto_Factores = quantile(Efecto_Factores, 0.25, na.rm = TRUE),
    q50_Efecto_Factores = quantile(Efecto_Factores, 0.5, na.rm = TRUE),
    q75_Efecto_Factores = quantile(Efecto_Factores, 0.75, na.rm = TRUE))

# Filtrando por los cuartiles 
# Primer cuartil (Q1) - Menor o igual al 25%
Cuartil1 <- Filtro %>%
  filter(
    Beneficio_ilícito <= cuartiles$q25_Beneficio_ilícito,
    Efecto_Prob <= cuartiles$q25_Efecto_Prob,
    Efecto_Factores <= cuartiles$q25_Efecto_Factores) %>%
  select(Beneficio_ilícito, Efecto_Prob, Efecto_Factores, Num_Imputacion)

# Segundo cuartil (Q2) - Entre el 25% y el 50%
Cuartil2 <- Filtro %>%
  filter(
    Beneficio_ilícito > cuartiles$q25_Beneficio_ilícito & Beneficio_ilícito <= cuartiles$q50_Beneficio_ilícito,
    Efecto_Prob > cuartiles$q25_Efecto_Prob & Efecto_Prob <= cuartiles$q50_Efecto_Prob,
    Efecto_Factores > cuartiles$q25_Efecto_Factores & Efecto_Factores <= cuartiles$q50_Efecto_Factores) %>%
  select(Beneficio_ilícito, Efecto_Prob, Efecto_Factores, Num_Imputacion)

# Tercer cuartil (Q3) - Entre el 50% y el 75%
Cuartil3 <- Filtro %>%
  filter(
    Beneficio_ilícito > cuartiles$q50_Beneficio_ilícito & Beneficio_ilícito <= cuartiles$q75_Beneficio_ilícito,
    Efecto_Prob > cuartiles$q50_Efecto_Prob & Efecto_Prob <= cuartiles$q75_Efecto_Prob,
    Efecto_Factores > cuartiles$q50_Efecto_Factores & Efecto_Factores <= cuartiles$q75_Efecto_Factores) %>%
  select(Beneficio_ilícito, Efecto_Prob, Efecto_Factores, Num_Imputacion)

# Cuarto cuartil (Q4) - Mayor al 75%
Cuartil4 <- Filtro %>%
  filter(
    Beneficio_ilícito > cuartiles$q75_Beneficio_ilícito,
    Efecto_Prob > cuartiles$q75_Efecto_Prob,
    Efecto_Factores > cuartiles$q75_Efecto_Factores) %>%
  select(Beneficio_ilícito, Efecto_Prob, Efecto_Factores, Num_Imputacion)


# Agregarlo al data frame (si Cuartil1 es un data frame)

# Estableciendo los cuadrantes

Cuartil1$Correlativo <- seq_len(nrow(Cuartil1))
#X1 <- Cuartil1$Num_Imputacion
X1 <- Cuartil1$Correlativo
Y1 <- Cuartil1[, c(1, 3, 2)]

Cuartil2$Correlativo <- seq_len(nrow(Cuartil2))
#X2 <- Cuartil2$Num_Imputacion
X2 <- Cuartil2$Correlativo
Y2 <- Cuartil2[, c(1, 3, 2)]

Cuartil3$Correlativo <- seq_len(nrow(Cuartil3))
#X3 <- Cuartil3$Num_Imputacion
X3 <- Cuartil3$Correlativo
Y3 <- Cuartil3[, c(1, 3, 2)]

Cuartil4$Correlativo <- seq_len(nrow(Cuartil4))
#X4 <- Cuartil4$Num_Imputacion
X4 <- Cuartil4$Correlativo
Y4 <- Cuartil4[, c(1, 3, 2)]

library(areaplot)
#"#144AA7", "#1d85bf", "#0BC7E0", "#44bfb5", "#8CCD3A", "#FFB500","#696a6a"
#cols <- hcl.colors(6, palette = "viridis", alpha = 0.8)
cols <- c("#44bfb5", "#8CCD3A", "#144AA7")

par(mfrow = c(2, 2))
areaplot(X1, Y1,col = cols, legend = TRUE, xlab = " ", ylab = "Multa", 
         args.legend = list(x = "topleft", cex = 0.65), main="Primer cuartil", xaxt = "n")
legend("topleft", legend = c("Beneficio ilícito", "Efecto factores", "Efecto Probabilidad"),
       fill = cols, cex = 0.65)

areaplot(X2, Y2,col = cols, xlab = " ", ylab = "Multa",
         args.legend = list(x = "topleft", cex = 0.65), main="Segundo cuartil", xaxt = "n")
#legend("topleft", legend = c("Beneficio ilícito", "Efecto factores", "Efecto Probabilidad"), fill = cols, cex = 0.65)

areaplot(X3, Y3,col = cols, xlab = " ", ylab = "Multa",
         args.legend = list(x = "topleft", cex = 0.65), main="Tercer cuartil", xaxt = "n")
#legend("topleft", legend = c("Beneficio ilícito", "Efecto factores", "Efecto Probabilidad"), fill = cols, cex = 0.65)

areaplot(X4, Y4,col = cols, xlab = " ", ylab = "Multa",
         args.legend = list(x = "topleft", cex = 0.65), main="Cuarto cuartil", xaxt = "n")
#legend("topleft", legend = c("Beneficio ilícito", "Efecto factores", "Efecto Probabilidad"), fill = cols, cex = 0.65)

rm(Cuartil1, Cuartil2, Cuartil3, Cuartil4, cuartiles, Filtro, Grafico, P90, Y1, Y2, Y3, Y4, X1,
   X2, X3, X4, Eliminar, cols)


### ---------- Gráfico de incumplimientos RUIAS ------------ ###

RFinal <- RUIAS %>% dplyr::select("ID", "Administrado", "RUC", "Sector económico", "Departamento",
                                  "Provincia", "Distrito", "Inicio de supervisión", "Fin de supervisión", 
                                  "Documento de inicio", "Fecha de emisión",
                                  "Infracción cometida sancionada (Clasificación de 11)",
                                  "Fecha de notificación...23")

colnames(RFinal)[colnames(RFinal) == "ID"] <- "Index"
colnames(RFinal)[colnames(RFinal) == "Sector económico"] <- "SectorEco"
colnames(RFinal)[colnames(RFinal) == "Inicio de supervisión"] <- "InicioSup"
colnames(RFinal)[colnames(RFinal) == "Fin de supervisión"] <- "FinSup"
colnames(RFinal)[colnames(RFinal) == "Infracción cometida sancionada (Clasificación de 11)"] <- "Incumplimiento_11"
colnames(RFinal)[colnames(RFinal) == "Fecha de notificación...23"] <- "InicioPAS"

Incumplimientos <- RFinal %>%
  mutate(Incumplimientos = case_when(
    Incumplimiento_11 == 'Incumplimiento de Límites Máximos Permisibles en efluentes' ~ 'Incumplimiento LMP',
    Incumplimiento_11 == 'Incumplimiento de Límites Máximos Permisibles en emisiones' ~ 'Incumplimiento LMP',
    Incumplimiento_11 == 'Incumplimiento del Instrumento de Gestión Ambiental' ~ 'Incumplimiento IGA',
    Incumplimiento_11 == 'No contar con Instrumentos de Gestión Ambiental' ~ 'Incumplimiento IGA',
    Incumplimiento_11 == 'Incumplimiento de medidas administrativas (medidas cautelares, medidas correctivas y preventivas)' ~ 'Incumplimiento de med. administrativas',
    Incumplimiento_11 == 'No brindar información, presentar información inexacta o fuera de plazo' ~ 'No presentó información',
    Incumplimiento_11 == 'No efectuar monitoreos (en el plazo, alcance y/o frecuencia)' ~ 'No efectuar monitoreos',
    Incumplimiento_11 == 'Obstaculizar o impedir labores de supervisión y/o fiscalización' ~ 'Obstaculizar o impedir labores',
    Incumplimiento_11 == 'Incumplimiento de normas de residuos sólidos' ~ 'Incumplimiento de residuos sólidos',
    Incumplimiento_11 == 'Incumplimiento de recomendación, mandato o disposición administrativa' ~ 'Incumplimiento de disposición admin.',
    TRUE ~ Incumplimiento_11))

rm(RFinal)

Incumplimientos$InicioPAS <- as.Date(Incumplimientos$InicioPAS, origin = "1899-12-30")
Incumplimientos$Año_InicioPAS <- format(Incumplimientos$InicioPAS, "%Y")

Pedido <- Incumplimientos %>%
          filter(Año_InicioPAS>=2021)

table(Pedido$Año_InicioPAS)

Pedido <- Pedido %>% 
  filter(Incumplimientos!="NA")

Resumen <- Pedido %>%
  group_by(Incumplimientos) %>%
  summarise(Frecuencia = n(), .groups = "drop")

Resumen2 <- Pedido %>%
  group_by(Incumplimientos, Año_InicioPAS) %>%
  summarise(Frecuencia = n(), .groups = "drop")

# Gráfico general
ggplot(Resumen, aes(x = Incumplimientos, y = Frecuencia, fill = as.factor(Incumplimientos))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Frecuencia), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  labs(x = "Incumplimientos", 
       y = "Frecuencia", 
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_blank() ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
  scale_fill_viridis_d(option = "viridis")

# Gráfico por año
ggplot(Resumen2, aes(x = Año_InicioPAS, y = Frecuencia, fill = as.factor(Incumplimientos))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = Frecuencia), 
            vjust = -0.5, 
            position = position_dodge(width = 0.9), 
            size = 3) +
  labs(x = NULL, 
       y = "Frecuencia", 
       fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE)) + 
        scale_fill_viridis_d(option = "viridis")

rm(Incumplimientos, Pedido, Resumen, Resumen2)
