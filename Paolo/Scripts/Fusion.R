
require(pacman)
p_load(foreign, tidyverse, rio, here, dplyr, viridis, readxl, stringr, RColorBrewer, ggcorrplot,  
        flextable, officer, classInt, foreign, stargazer, sf, mapview, leaflet, writexl, lmtest,
       tseries, car, haven, officer, xlsx, openxlsx)

rm(list = ls())

###################################
###### Informes de sanción ########
###################################

# Carga de información

Consolidado <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/dfunido.xlsx", sheet = "Sheet 1")
RUIAS <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/RUIAS-CSEP.xlsx", sheet = "RUIAS")

# Quitando las observaciones que no usaremos del consolidado

CFinal <- Consolidado %>%
  filter(!Propuesta_Multa %in% c("si"),!Observaciones%in% c("Se debe eliminar por criterio del informe"))

colnames(CFinal)[colnames(CFinal) == "ID2"] <- "Index"
CFinal$Index <- as.character(CFinal$Index)

# Seleccionando las variables a usar

RFinal <- RUIAS %>% dplyr::select("ID", "Administrado", "RUC", "Sector económico", "Departamento",
                                  "Provincia", "Distrito", "Infracción cometida sancionada (Clasificación de 11)",
                                  "Infracción cometida sancionada (Clasificación de 19)", 
                                  "¿Tiene resolución de reconsideración?...81", "¿Tiene resolución de apelación?...88",
                                  "Inicio de supervisión", "Fin de supervisión", "Fecha de notificación...23",
                                  "¿Tiene resolución de apelación?...88", "Documento de inicio", "Fecha de emisión",
                                  "N° de Resolución de Responsabilidad Administrativa", "Fecha de la Resolución...38", 
                                  "Fecha de notificación...39")

colnames(RFinal)[colnames(RFinal) == "ID"] <- "Index"
colnames(RFinal)[colnames(RFinal) == "Sector económico"] <- "SectorEco"
colnames(RFinal)[colnames(RFinal) == "Inicio de supervisión"] <- "InicioSup"
colnames(RFinal)[colnames(RFinal) == "Fin de supervisión"] <- "FinSup"
colnames(RFinal)[colnames(RFinal) == "Fecha de notificación...23"] <- "InicioPAS"
colnames(RFinal)[colnames(RFinal) == "Infracción cometida sancionada (Clasificación de 11)"] <- "Incumplimiento1"
colnames(RFinal)[colnames(RFinal) == "¿Tiene resolución de apelación?...88"] <- "Apelacion"

RFinal$Index <- as.character(RFinal$Index)

# Fusionando ambas bases
FINAL <-left_join(x = CFinal, y = RFinal, by="Index")
FINAL <- FINAL %>%
  mutate(Merge = if_else(!is.na(Departamento), 1, 0)) 

# Eliminando los objetos que no necesitamos
rm(CFinal, RFinal, Consolidado, RUIAS)

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

# Otros ajustes

FINAL <- FINAL %>%
  select(-Administrado.x) %>%  
  rename(Administrado = Administrado.y)  

### Obteniendo el total de hechos imputados, extremos y sub extremos ###

FINAL$Colapsar <- ifelse(FINAL$Colapsar == "Máximo", "Maximo", FINAL$Colapsar)
table(FINAL$Colapsar)

# Seleccionando variables a emplear
Extremos <- FINAL %>% dplyr::select(Informes, Num_Imputacion, Colapsar)

#  245 registros con extremos y sub extremos
Revision <- Extremos %>% 
            filter(Colapsar!="NA")

#  64 hechos imputados con extremos y sub extremos
Revision2 <- Revision %>%
  distinct(Informes, Num_Imputacion)

#  37 informes con hechos imputados con extremos y sub extremos
Revision3 <- Revision2 %>%
  distinct(Informes)

rm(Extremos, Revision, Revision2, Revision3)

# 798 - 245 = 553 Hechos imputados
# 553 + 64 = 617 Hechos imputados

###################################
###### Fechas de informes ########
###################################

F2022 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Fechas/Fecha_2022.xlsx", sheet = "2022")
F2023 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Fechas/Fecha_2023.xlsx", sheet = "2023")
F2024 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Fechas/Fecha_2024.xlsx", sheet = "2024")

Fechas <- rbind(F2022, F2023, F2024)

Fechas <- Fechas %>% dplyr::select(Informes, Fecha_Informe, Obs_Fecha)
rm(F2022, F2023, F2024)

FINAL <-left_join(x = FINAL, y = Fechas, by="Informes")
rm(Fechas)

###################################
##### Factores de graduación ######
###################################

G2022 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2022.xlsx", 
                   sheet = "Graduacion")
G2023 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2023.xlsx", 
                   sheet = "Graduacion")
G2024 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2024.xlsx", 
                   sheet = "Graduacion")

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
Consolidado <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/dfunido.xlsx", sheet = "Sheet 1")

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
#write_xlsx(Fusion, path = "D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Factores.xlsvx")

Fusion <- Fusion %>%
  filter(is.na(Propuesta_Multa))

FACTORES <- Fusion %>%
  filter(!(Informes == "00575-2023-OEFA/DFAI-SSAG" & Imputacion == 1))

FACTORES <- FACTORES %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")

rm(Fusion)
#write.xlsx(Fusion,"D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/INFORMES_GRADUACION.xlsx", sheet = "Factores")

###################################
###### Exportando las bases #######
###################################

wb <- createWorkbook()

# Añadir la primera hoja con el primer dataframe
addWorksheet(wb, "Informes")
writeData(wb, "Informes", FINAL)

# Añadir la segunda hoja con el segundo dataframe
addWorksheet(wb, "Factores")
writeData(wb, "Factores", Fusion)

# Guardar el archivo Excel
saveWorkbook(wb, "D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/INFORMES_GRADUACION.xlsx", overwrite = TRUE)


###################################
#### Estadísticas Descriptivas ####
###################################


### --- Total de informes de cálculo de multa y hechos imputados --- ###
### --------- analizados para el período de 2022 a 2024 -----------  ###


### ------- Para el total fusionando con RUIIAS ------- ###

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

# Gráfico de cajas con outliers

Multa1 <-  ggplot(Hechos_Multas, aes(x = factor(year), y = Multa_Final)) +
         geom_boxplot(fill = "darkolivegreen4", color = "black", outlier.colour = "black") +
         labs(x = "Año", y = "Multa final (en UIT)") +
         theme_minimal()

# Ahora sin considerar outliers
Filtro <- Hechos_Multas %>%
  group_by(year) %>%
  filter(Multa_Final <= quantile(Multa_Final, 0.70, na.rm = TRUE)) %>%
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

# Gráfico de cajas sin outliers
Multa2 <- ggplot(Filtro, aes(x = factor(year), y = Multa_Final)) +
          geom_boxplot(fill = "darkgoldenrod2", color = "black", outlier.colour = "black") +  
          labs(x = "Año", y = "Multa final (en UIT)") +
          theme_minimal()

library(patchwork)
MultaF <- Multa1 / Multa2 
MultaF

rm(Est1, Est2, Est3, Filtro, Multa1, Multa2, MultaF, Hechos_Multas)

### ---- Por sectores ---- ###
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
    minimo = round(min(Multa_Final, na.rm = TRUE), 3),
    Q1 = round(quantile(Multa_Final, 0.25, na.rm = TRUE), 2),
    mediana = round(median(Multa_Final, na.rm = TRUE), 2),
    promedio = round(mean(Multa_Final, na.rm = TRUE), 2),
    Q3 = round(quantile(Multa_Final, 0.75, na.rm = TRUE), 2),
    maximo = round(max(Multa_Final, na.rm = TRUE), 0),
    .groups = 'drop')

write_xlsx(Est4, "D:/NUEVO D/LOCACION OEFA/Informes/Tercera OS/Informe 5/Graficos/Tabla_Sectores.xlsx")

# Gráfico de cajas con outliers por sectores
Multa3 <- ggplot(Hechos_Multas, aes(x = factor(SectorEco), y = Multa_Final)) +
          geom_boxplot(fill = "darkolivegreen4", color = "black", outlier.colour = "black") +
          labs(x = "Sector Económico", y = "Multa final (en UIT)") +
          theme_minimal()

# Ahora sin considerar outliers
Filtro2 <- Hechos_Multas %>%
           group_by(SectorEco) %>%
           filter(Multa_Final <= quantile(Multa_Final, 0.70, na.rm = TRUE)) %>%
           ungroup() 

Multa4 <-   ggplot(Filtro2, aes(x = factor(SectorEco), y = Multa_Final)) +
            geom_boxplot(fill = "darkgoldenrod2", color = "black", outlier.colour = "black") +  
            labs(x = "Año", y = "Multa final (en UIT)") +
            theme_minimal()


MultaF2 <- Multa3 / Multa4 
MultaF2

rm(Est4, Multa3, Multa4, Hechos_Multas, Filtro2, MultaF2)

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
write_xlsx(Est5, "D:/NUEVO D/LOCACION OEFA/Informes/Tercera OS/Informe 5/Graficos/Tabla_beneficio.xlsx")

# Grafico de cajas del beneficio ilicito por sector

BI_Sectores <- Hechos_Ilicitos %>%
  distinct(SectorEco, Beneficio_ilícito)

Filtro3 <- BI_Sectores %>%
  group_by(SectorEco) %>%
  filter(Beneficio_ilícito <= quantile(Beneficio_ilícito, 0.70, na.rm = TRUE)) %>%
  ungroup() 

# Ajusta los márgenes (c(bottom, left, top, right))
par(mfrow = c(2, 1), mar = c(3, 4, 2, 1)) 

# Gráfico de cajas con outliers por sectores
boxplot(Beneficio_ilícito ~ SectorEco, data = BI_Sectores,
        xlab = "Sector Económico", ylab = "Beneficio ilícito (en UIT)",
        col = "lightblue", border = "black", outlier.colour = "black",
        cex.axis = 0.55)  

# Gráfico de cajas sin outliers por sectores
boxplot(Beneficio_ilícito ~ SectorEco, data = Filtro3,
        xlab = "Sector Económico", ylab = "Beneficio ilícito (en UIT)",
        col = "lightblue", border = "black", outlier.colour = "black",
        cex.axis = 0.55)  

# Restablece la configuración de gráficos
par(mfrow = c(1, 1))


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
boxplot(Fechas_Filtrado$Meses_FinSup_InicioPAS,
        main = "Periodo 1",
        ylab = "Meses",
        col = "lightblue")

# Gráfico 2: Determinación de la multa (InicioPAS a Informe de Sanción)
boxplot(Fechas_Filtrado$Meses_InicioPAS_FechaInforme,
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


# Calcular los promedios de cada transición por sector económico
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


# Calcular los máximos de cada transición por sector económico
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



### --- Informes excluidos --- ###

rm(list = ls())

G2022 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2022.xlsx", 
                   sheet = "Componentes")
G2023 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2023.xlsx", 
                   sheet = "Componentes")
G2024 <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Factores/Copia de Informes_2024.xlsx", 
                   sheet = "Componentes")

G2022 <- G2022 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")
G2023 <- G2023 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")
G2024 <- G2024 %>% dplyr::select("ID","Informes", "Hecho_imputado", "Num_Imputacion")

G2022$Año <- 2022
G2023$Año <- 2023
G2024$Año <- 2024

# Compilando las bases 
FINAL <- rbind(G2022, G2023, G2024)

# Trayendo el DFUNIDO
Consolidado <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/dfunido.xlsx", sheet = "Sheet 1")

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

