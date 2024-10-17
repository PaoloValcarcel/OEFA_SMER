rm(list = ls())

require(pacman)
p_load(foreign, tidyverse, rio, here, dplyr, viridis, readxl, stringr, RColorBrewer, ggcorrplot,  
        flextable, officer, classInt, foreign, stargazer, sf, mapview, leaflet, writexl, lmtest,
       tseries, car, haven, officer, xlsx, openxlsx)

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

#Quitar las sanciones preliminares


# Seleccionando las variables a usar

RFinal <- RUIAS %>% dplyr::select("ID", "Administrado", "RUC", "Sector económico", "Departamento",
                                  "Provincia", "Distrito", "Infracción cometida sancionada (Clasificación de 11)",
                                  "Infracción cometida sancionada (Clasificación de 19)", 
                                  "¿Tiene resolución de reconsideración?...81", "¿Tiene resolución de apelación?...88",
                                  "Inicio de supervisión", "Fin de supervisión",
                                  "¿Tiene resolución de apelación?...88")

colnames(RFinal)[colnames(RFinal) == "ID"] <- "Index"
colnames(RFinal)[colnames(RFinal) == "Sector económico"] <- "SectorEco"
colnames(RFinal)[colnames(RFinal) == "Inicio de supervisión"] <- "InicioSup"
colnames(RFinal)[colnames(RFinal) == "Fin de supervisión"] <- "FinSup"
colnames(RFinal)[colnames(RFinal) == "Infracción cometida sancionada (Clasificación de 11)"] <- "Incumplimiento1"
colnames(RFinal)[colnames(RFinal) == "¿Tiene resolución de apelación?...88"] <- "Apelacion"

RFinal$Index <- as.character(RFinal$Index)

# Fusionando ambas bases
FINAL <-left_join(x = CFinal, y = RFinal, by="Index")

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

#write.xlsx(FINAL ,"D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/INFORMES_GRADUACION.xlsx", sheet = "Informes")

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
G2022F <- G2022F %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")
G2023F <- G2023F %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")
G2024F <- G2024F %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")

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

Fusion <- Fusion %>% dplyr::select("ID","Informes", "Imputacion", "Factores_agravantes", "Categoria_FA", "% FA")

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

#INFORMES <-read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Paolo/Scripts/Bases/Consolidado_Informes.xlsx", sheet = "CONSOLIDADO")

## Creando la variable de ocurrencia
Unique <- FINAL %>%
  distinct(year, Expediente, RUC)

# Contando la recurrencia de cada RUC (administrado) por año
Recurrencia <- Unique %>%
  group_by(year, RUC) %>%
  summarise(Recurrencia = n()) %>%
  ungroup()

# Uniendo el conteo con los expedientes únicos por año
Colapsado <- Recurrencia %>%
  left_join(Unique, by = c("year", "RUC")) %>%
  dplyr::select(year, Expediente, Recurrencia)

#library(writexl)
#write_xlsx(FINAL, "D:/NUEVO D/LOCACION OEFA/Bases/FINAL.xlsx")

FINAL$Index <- paste(FINAL$year, FINAL$Expediente, sep = "_")
Colapsado$Index <- paste(Colapsado$year, Colapsado$Expediente, sep = "_")
Filtro <- Colapsado[, c("Index", "Recurrencia")]
FINAL <- merge(FINAL, Filtro, by = "Index", all.x = TRUE)
rm(Colapsado, Filtro, Recurrencia, Unique)

### ------- Estadísticas descriptivas ------- ###

# Administrados únicos por año

Admin_Año <- FINAL %>%
  group_by(year) %>%
  summarise(count = n_distinct(RUC))

ggplot(Admin_Año, aes(x = factor(year), y = count, fill = factor(year))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, size = 5) +
  scale_fill_brewer(palette = "Set3") +  
  labs(x = "Año", 
       y = "Número de Administrados") +
  theme_minimal(base_size = 15) +  
  theme(legend.position = "none") 

rm(Admin_Año)

# Sanciones por sector económico

ggplot(FINAL, aes(x = SectorEco, y = Sancion_total, fill = SectorEco)) +
  geom_boxplot() +
  labs(x = "Sector",
       y = "Sanción total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Sanciones por sector económico y año
ggplot(FINAL, aes(x = SectorEco, y = Sancion_total, fill = factor(year))) +
  geom_boxplot() +
  labs(x = "Sector",
       y = "Sanción total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(), 
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set1") 


# Histograma por sector 
ggplot(data = FINAL, aes(x = Prob_Detección, fill = SectorEco)) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())

# Histograma de probabilidad de detección para Agricultura

Agricultura <- subset(FINAL, sector == "Agricultura")

ggplot(data = Agricultura, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())

# Histograma de probabilidad de detección para Minería

Mineria <- subset(FINAL, sector == "Minería")

ggplot(data = Mineria, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())

# Histograma de probabilidad de detección para Electricidad

Electricidad <- subset(FINAL, sector == "Electricidad")

ggplot(data = Electricidad, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())

# Histograma de probabilidad de detección para Industria

Industria <- subset(FINAL, sector == "Industria")

ggplot(data = Industria, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())

# Histograma de probabilidad de detección para Pesquería

Pesqueria <- subset(FINAL, sector == "Pesquería")

ggplot(data = Pesqueria, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())

# Histograma de probabilidad de detección para hidrocarburos

Hidrocarburos <- subset(FINAL, sector == "Hidrocarburos")

ggplot(data = Hidrocarburos, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())

# Histograma de probabilidad de detección para Residuos Sólidos

Residuos <- subset(FINAL, sector == "Residuos Sólidos")

ggplot(data = Residuos, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())


# Duración promedio de la supervisión por sector y año

FINAL$InicioSup <- as.numeric(FINAL$InicioSup)
FINAL$FinSup <- as.numeric(FINAL$FinSup)

Supervisiones <- FINAL %>%
  mutate(InicioSup = as.Date(InicioSup, format = "%Y-%m-%d"), 
         FinSup = as.Date(FinSup, format = "%Y-%m-%d")) %>%
  mutate(Duracion = as.numeric(difftime(FinSup, InicioSup, units = "days"))) %>%
  group_by(SectorEco, year) %>%
  summarize(DuracionPromedio = round(mean(Duracion, na.rm = TRUE), 1))

colnames(Supervisiones)[colnames(Supervisiones) == "DuracionPromedio"] <- "Duracion"

# Tiempo promedio de las supervisiones por sector y año
ggplot(Supervisiones, aes(x = SectorEco, y = Duracion, fill = SectorEco)) +  
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), aes(group = factor(year))) +  
  coord_flip() +  
  geom_text(aes(label = sprintf("%.1f", Duracion), group = factor(year)),  
            position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 3) +  
  labs(x = "Sector",
       y = "Duración Promedio (días)",
       fill = "Sector") +  
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal() +
  theme(legend.position = "none")

# Tiempo promedio de las supervisiones por sector y año en escala

colores <- c("2022" = "#FFCCCC",  # Naranja pastel
                    "2023" = "#FF6666",  # Azul pastel
                    "2024" = "#800000")  # Rojo pastel

ggplot(Supervisiones, aes(x = SectorEco, y = Duracion, fill = factor(year))) +  
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), aes(group = factor(year))) +  
  coord_flip() +  
  geom_text(aes(label = sprintf("%.1f", Duracion), group = factor(year)),  
            position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 3) +  
  labs(x = "Sector",
       y = "Duración Promedio (días)",
       fill = "Año") +  
  scale_fill_manual(values = colores) + 
  theme_minimal() +
  theme(legend.position = "right") 

# Incumplimientos por sectores

Incumplimientos <- FINAL %>%
  group_by(SectorEco, year) %>%
  summarise(Conteo = n()) %>%
  ungroup()

ggplot(Incumplimientos, aes(x = SectorEco, y = Conteo, fill = SectorEco)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Conteo), vjust = -0.5, color = "black") +
  labs(x = "Sector Económico",
       y = "Conteo de Incumplimientos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  scale_fill_brewer(palette = "Pastel1") +
  facet_wrap(~ year)

rm(Incumplimientos)


# Probabilidad promedio por departamento

FINAL <- FINAL %>%
  mutate(Departamento = ifelse(Departamento == "Lima / Lima", "Lima", Departamento))

FINAL <- FINAL %>%
  mutate(Departamento = ifelse(Departamento == "Loreto Loreto", "Loreto", Departamento))

FINAL <- FINAL %>%
  mutate(Departamento = ifelse(Departamento == "Piura / Piura", "Piura", Departamento))

FINAL <- FINAL %>%
  mutate(Departamento = iconv(Departamento, from = "UTF-8", to = "ASCII//TRANSLIT"))

Prob_mean <- FINAL %>%
  group_by(Departamento) %>%
  summarise(Prob_Promedio = mean(Prob_Detección, na.rm = TRUE))

Prob_mean$Long <- nchar(Prob_mean$Departamento)

Prob_mean <- Prob_mean %>% filter(Long<15)
Prob_mean <- Prob_mean %>% filter(Long!=14)

Peru <- st_read(dsn = "D:/NUEVO D/UNIANDES/ECONOMIA URBANA/CBD Peru/Shape/PER_adm1.shp")

colnames(Peru)[colnames(Peru) == "NAME_1"] <- "Departamento"

Peru <- Peru %>%
  mutate(Departamento = iconv(Departamento, from = "UTF-8", to = "ASCII//TRANSLIT"))

Mapa <- Peru %>%
  left_join(Prob_mean, by = "Departamento")

Mapa <- Mapa %>%
  mutate(Prob_Promedio = ifelse(Departamento == "Lima Province", 0.7174107, Prob_Promedio))

mapview(Mapa, zcol = "Prob_Promedio", legend = TRUE,
            layer.name = c("Prob. Detección")) 


# Correlación entre Probabilidad de detección y Multas

cor(FINAL$Prob_Detección, FINAL$Sancion_total, use = "complete.obs")
cor(FINAL$Prob_Detección, FINAL$Multa_Final, use = "complete.obs")

Num1 <- FINAL[, c("Prob_Detección", "Sancion_total")]
Num2 <- FINAL[, c("Prob_Detección", "Multa_Final")]

Matriz1 <- cor(Num1, use = "complete.obs")
Matriz2 <- cor(Num2, use = "complete.obs")

print(Matriz1)
print(Matriz2)

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


### ------ Modelo econométrico ------ ###
FINAL$InicioSup <- as.numeric(FINAL$InicioSup)
FINAL$FinSup <- as.numeric(FINAL$FinSup)
FINAL$InicioSup <- as.Date(FINAL$InicioSup, origin = "1899-12-30")
FINAL$FinSup <- as.Date(FINAL$FinSup, origin = "1899-12-30")
FINAL$DuracionSup <- as.numeric(FINAL$FinSup - FINAL$InicioSup)


FINAL <- FINAL %>%
  mutate(Incumplimiento1 = case_when(
    Incumplimiento1 == 'Incumplimiento de Límites Máximos Permisibles en efluentes' ~ 'Incumplimiento LMP',
    Incumplimiento1 == 'Incumplimiento de Límites Máximos Permisibles en emisiones' ~ 'Incumplimiento LMP',
    Incumplimiento1 == 'Incumplimiento del Instrumento de Gestión Ambiental' ~ 'Incumplimiento IGA',
    Incumplimiento1 == 'No contar con Instrumentos de Gestión Ambiental' ~ 'Incumplimiento IGA',
    Incumplimiento1 == 'Incumplimiento de medidas administrativas (medidas cautelares, medidas correctivas y preventivas)' ~ 'Incumplimiento de medidas administrativas',
    Incumplimiento1 == 'No brindar información, presentar información inexacta o fuera de plazo' ~ 'No presentó información',
    Incumplimiento1 == 'No efectuar monitoreos (en el plazo, alcance y/o frecuencia)' ~ 'No efectuar monitoreos',
    Incumplimiento1 == 'Obstaculizar o impedir labores de supervisión y/o fiscalización' ~ 'Obstaculizar o impedir labores',
    TRUE ~ Incumplimiento1))


table(FINAL$Incumplimiento1)

BD <- read_dta("C:/Users/Paolo/Desktop/Sunat/S192021.dta")
colnames(FINAL)[colnames(FINAL) == "RUC"] <- "ruc"
BD$ruc <- as.character(BD$ruc)
FINAL <- left_join(x = FINAL, y = BD, by="ruc")

# OLS de corte transversal (2024)
A2022 <- FINAL %>% filter(year==2022)
A2023 <- FINAL %>% filter(year==2023)
A2024 <- FINAL %>% filter(year==2024)

#Modelo22 <- lm(log(Sancion_total) ~ log(Costo_evitado) + Prob_Detección + factor(sector)
#               + T_meses + DuracionSup + Recurrencia + factor(descripcion_contri) + 
#                 factor(tamano_ven), data = A2022)

Modelo22 <- lm(log(Sancion_total) ~ log(Costo_evitado) + Prob_Detección + factor(sector)
               + T_meses + DuracionSup + Recurrencia  + factor(tamano_ven) + 
                 factor(Incumplimiento1), data = A2022)

# H0: No hay error de especificación
resettest(Modelo22)
# QQplot: Desviación de los residuos
qqPlot(Modelo22$residuals)
# H0: distribuyen normal los residuos
jarque.bera.test(Modelo22$residuals)
# H0: Homocedasticidad
bptest(Modelo22)

#Modelo23 <- lm(Sancion_total ~ Costo_evitado + Prob_Detección + factor(sector)
#               + T_meses + DuracionSup + Recurrencia + factor(descripcion_contri) + 
#                 factor(tamano_ven), data = A2023)

Modelo23 <- lm(log(Sancion_total) ~ log(Costo_evitado) + Prob_Detección + factor(sector)
               + T_meses + DuracionSup + Recurrencia  + factor(tamano_ven) + 
                 factor(Incumplimiento1), data = A2023)

# H0: No hay error de especificación
resettest(Modelo23)
# QQplot: Desviación de los residuos
qqPlot(Modelo23$residuals)
# H0: distribuyen normal los residuos
jarque.bera.test(Modelo23$residuals)
# H0: Homocedasticidad
bptest(Modelo23)


#Modelo24 <- lm(Sancion_total ~ Costo_evitado + Prob_Detección + factor(sector)
#               + T_meses + DuracionSup + Recurrencia + factor(descripcion_contri) + 
#                 factor(tamano_ven), data = A2024)

Modelo24 <- lm(log(Sancion_total) ~ log(Costo_evitado) + Prob_Detección + factor(sector)
               + T_meses + DuracionSup + Recurrencia  + factor(tamano_ven) + 
                 factor(Incumplimiento1), data = A2024)

# H0: No hay error de especificación
resettest(Modelo24)
# QQplot: Desviación de los residuos
qqPlot(Modelo24$residuals)
# H0: distribuyen normal los residuos
jarque.bera.test(Modelo24$residuals)
# H0: Homocedasticidad
bptest(Modelo24)

stargazer(Modelo22, Modelo23, Modelo24, type = "text",  out="D:/NUEVO D/LOCACION OEFA/Informes/Segunda OS/Informe 01/Resultados/Modelo.tex")

# Elección discreta
table(A2022$Prob_Detección)
Logit <- glm(Prob_Detección ~ factor(sector) + DuracionSup, data = A2022, family = binomial(link = "logit"))
stargazer(Logit, type = "text")

# Panel data

#library(plm)

#Panel <- pdata.frame(FINAL, index = c("Num_Imputacion","year"))

# Eliminar filas con NA en las variables de índice
#Panel <- Panel[complete.cases(Panel$Num_Imputacion, Panel$year), ]

# Eliminar duplicados
#Panel <- Panel[!duplicated(Panel[, c("Num_Imputacion", "year")]), ]

#Pooled <- plm(Sancion_total ~ Costo_evitado + Prob_Detección + factor(sector), data = Panel, model = "pooling")
#FE <- plm(Sancion_total ~ Costo_evitado + Prob_Detección + factor(sector), data = Panel, model = "within")

#summary(Pooled)
#summary(FE)

# Pooled vs Efectos Fijos
#pFtest(FE, Pooled)