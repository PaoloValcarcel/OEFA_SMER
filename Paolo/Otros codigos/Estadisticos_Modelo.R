

### --- Otros estadísticos --- ###

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
