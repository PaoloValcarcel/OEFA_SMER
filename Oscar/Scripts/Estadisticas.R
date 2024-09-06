rm(list = ls())
getwd()

#Asignamos la carpeta donde trabajaremos
#setwd("D:/OEFA/Metodología de multas OEFA/analisis/outputs")

# Carga de información


library(readxl)
#M2022 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2022.xlsx", sheet = "Componentes")
#M2023 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2023.xlsx", sheet = "Componentes")
#M2024 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2024.xlsx", sheet = "Componentes")

M2022 <-read_excel("D:/NUEVO D/LOCACION OEFA/Busqueda/Informes_2022.xlsx", sheet = "Componentes")
M2023 <-read_excel("D:/NUEVO D/LOCACION OEFA/Busqueda/Informes_2023.xlsx", sheet = "Componentes")
M2024 <-read_excel("D:/NUEVO D/LOCACION OEFA/Busqueda/Informes_2024.xlsx", sheet = "Componentes")

# Tratamiento de información

M2022F<- M2022 %>%
  filter(!Hecho_imputado %in% c("Reconsideración", "Multa coercitiva", "Sin especificar"))

M2023F<- M2023 %>%
  filter(!Hecho_imputado %in% c("Reconsideración", "multa coercitiva"))

M2024F<- M2024 %>%
  filter(!Hecho_imputado %in% c("Multa coercitiva", "Reconsideración", 
                                "Medida correctiva", "Informe de enmienda"))


#####Estadística de las infracciones 
####################################

#Creamos los años en los que se generaron la información
M2022F <- M2022F %>% mutate( year = 2022)
M2023F <- M2023F %>% mutate( year = 2023)
M2024F <- M2024F %>% mutate( year = 2024)

#Unimos las bases de datos
df_unido <- rbind(M2022F,M2023F,M2024F)

#Generamos una variable con números consecutivos usando el operador :
df_unido$id2 <- 1:nrow(df_unido)

#Generamos un código para las imputaciones analizadas
names(df_unido)
df_unido$imp <- paste(as.character(df_unido$ID), as.character(df_unido$Num_Imputacion),as.character(df_unido$year), sep = "-")
df_unido <- df_unido %>% rename(Expediente = Expedientes)

#Importamos RUIAS
#ruias <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/RUIAS-CSEP.xlsx")
ruias <- read_excel("D:/NUEVO D/REPOSITORIO_GITHUB/OEFA_SMER/Oscar/Scripts/RUIAS-CSEP.xlsx")
names(ruias)

#Generamos la base que requerimos
sectores <- ruias[, c("Expediente", "Sector económico", "Administrado")]
sectores <- sectores %>% rename(sector =`Sector económico`)
sectores <- distinct(sectores)

sectores <- sectores %>% mutate(sector = ifelse(sector == "RESIDUOS SÓLIDOS" ,"Residuos Sólidos", sector))
table(sectores$sector)

df_unido2 <- merge(df_unido, sectores, by="Expediente", all.x = TRUE)

#Quitamos los duplicados en caso los haya
df_unido2 <- distinct(df_unido2)
########################################################################
General <- subset(df_unido2, !is.na(sector))

# Histograma
ggplot(data = General, aes(x = Prob_Detección, fill = sector)) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())


General_Hidrocarburos <- subset(General, sector == "Hidrocarburos")

# Gráfico hidrocarburos
ggplot(data = General_Hidrocarburos, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())


General_Residuos <- subset(General, sector == "Residuos Sólidos")

# Gráfico de residuos sólidos
ggplot(data = General_Residuos, aes(x = Prob_Detección, fill = factor(year))) +
  geom_histogram(position = "dodge", binwidth = 0.12, color = "black") +  
  labs(x = "Probabilidad de detección",             
       y = "Frecuencia", 
       fill = "Año") +                            
  theme_minimal() +
  theme(legend.position = "bottom",          
        legend.title = element_blank())


library(writexl) 
write_xlsx(df_unido2, path = "D:/NUEVO D/LOCACION OEFA/Bases/Base_Conjunta.xlsx")
########################################################################

########################################
#Generamos las estadísticas descriptivas
########################################

#Sanción por imputación

#Convertimos la variable Multa en numérica
class(df_unido2$Multa)
df_unido2$Multa <- as.numeric(df_unido2$Multa)

sanciones <- df_unido2 %>% group_by(imp) %>% summarise(monto = min(Multa))
class(sanciones$monto)

#Unimos con las información deseada y nos quedamos solo con la información deseada
sanciones1 <- merge(sanciones[, c("imp", "monto")], df_unido2[, c("imp", "sector")], by = "imp", all.x = TRUE)
sanciones2 <- sanciones1 %>% distinct(imp, monto, .keep_all = TRUE)

#Resumen de información para una variable
summary(sanciones2$monto)
names(sanciones2)

#Resumen por sectores
resumen <- sanciones2 %>%
  group_by(sector) %>%
  summarise(
    Min = min(monto),
    Q1 = quantile(monto, 0.25),
    Mediana = median(monto),
    Media = mean(monto),
    Q3 = quantile(monto, 0.75),
    Max = max(monto)
  )

print(resumen)

#Segunda opción
#install.packages("psych")
library(psych)

describeBy(sanciones$monto, group = sanciones$sector)

#Realizamos un gráfico de cajas
boxplot(sanciones2$monto ~ sanciones2$sector, main = "Sanciónes impuestas por imputación",
        ylab = "Soles", xlab="Costo Evitado", col = "#FBB500", horizontal = FALSE,
        outline = FALSE)


#Realizamos un histograma
hist(sanciones2$monto, freq= TRUE, breaks = 50,
     xlab = "Monto de Sancion (UIT)", ylab = "Frecuencia", col= "#8CCD3A")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)


#Para aquellos datos mayor al percentil 90
percentil_90 <- quantile(sanciones2$monto, 0.90)
datos_filtrados <- sanciones2$monto[sanciones2$monto < percentil_90]

#Realizamos un histograma
hist(datos_filtrados, freq= TRUE, breaks = 50,
     xlab = "Monto de Sancion (UIT)", ylab = "Frecuencia", col= "#8CCD3A")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)

#Para aquellos datos mayor al percentil 80
percentil_80 <- quantile(sanciones2$monto, 0.80)
datos_filtrados2 <- sanciones2$monto[sanciones2$monto < percentil_80]

#Histograma
hist(datos_filtrados2, freq= TRUE, breaks = 50,
     main = "Histograma de sanciones impuestas por imputación", xlab = "Monto de Sancion (UIT)", 
     ylab = "Frecuencia", col= "#8CCD3A")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)

#Probabilidad de detección

boxplot(df_unido2$Prob_Detección ~ df_unido2$sector)

