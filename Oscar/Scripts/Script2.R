
rm(list = ls())

getwd()

#Asignamos la carpeta donde trabajaremos
setwd("D:/OEFA/Metodología de multas OEFA/analisis/outputs")

# Carga de información


library(dplyr)
library(readxl)

M2022 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2022.xlsx", sheet = "Componentes")
M2023 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2023.xlsx", sheet = "Componentes")
M2024 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2024.xlsx", sheet = "Componentes")


#Juntamos las bases de datos
df_general <- rbind(M2022, M2023, M2024)

#Eliminamos las imputaciones que no deben considerarse

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
df_unido$numeracion <- 1:nrow(df_unido)

#Generamos un código para las imputaciones analizadas
names(df_unido)
df_unido$imp <- paste(as.character(df_unido$ID), as.character(df_unido$Num_Imputacion),as.character(df_unido$year), sep = "-")
df_unido <- df_unido %>% rename(Expediente = Expedientes)

#Importamos RUIAS
ruias <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/RUIAS-CSEP.xlsx")
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

#Generamos una variable de identificación
names(df_unido2)
df_unido2$identificador <- paste(as.character(df_unido$Informes), as.character(df_unido$Num_Imputacion), sep = "-")


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
        ylab = "UIT", xlab="Costo Evitado", col = "#FBB500", horizontal = FALSE,
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

#Realizamos un histograma
hist(datos_filtrados2, freq= TRUE, breaks = 50,
     main = "Histograma de sanciones impuestas por imputación", xlab = "Monto de Sancion (UIT)", 
     ylab = "Frecuencia", col= "#8CCD3A")
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)


#Costos evitados
################

names(df_unido2)
costo_evi <- df_unido2 %>% group_by(imp) %>% summarise(monto = min(Beneficio_ilícito))

#Unimos con las información deseada y nos quedamos solo con la información deseada
costo_evi <- merge(costo_evi[, c("imp", "monto")], df_unido2[, c("imp", "sector")], by = "imp", all.x = TRUE)
costo_evi <- costo_evi  %>% distinct(imp, monto, .keep_all = TRUE)

#Realizamos un gráfico de cajas
class(costo_evi$monto)
costo_evi$monto <- as.numeric(costo_evi$monto)

boxplot(costo_evi$monto ~ costo_evi$sector, main = "Beneficio ilícito por imputación",
        ylab = "UIT", xlab="Costo Evitado", col = "#0BC7E0", horizontal = FALSE,
        outline = FALSE)



#Probabilidad de detección

boxplot(df_unido2$Prob_Detección ~ df_unido2$sector)



#Factores de graduación de sanciones
###################################

#Importamos la información

library(readxl)
F2022 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2022.xlsx", sheet = "Graduacion")
F2023 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2023.xlsx", sheet = "Graduacion")
F2024 <- read_excel("D:/OEFA/Metodología de multas OEFA/analisis/Informes_2024.xlsx", sheet = "Graduacion")

#Unimos las bases de datos
df_factores <- rbind(F2022,F2023,F2024)


#Revisamos las imputaciones que no deben ser consideradas y verificamos que cuenten con únicamente un infracción

M2022Eli<- M2022 %>%
  filter(Hecho_imputado %in% c("Reconsideración", "Multa coercitiva", "Sin especificar"))

M2023Eli<- M2023 %>%
  filter(Hecho_imputado %in% c("Reconsideración", "multa coercitiva"))

M2024Eli<- M2024 %>%
  filter(Hecho_imputado %in% c("Multa coercitiva", "Reconsideración", 
                                "Medida correctiva", "Informe de enmienda"))


#Unimos las bases de datos
imput_eliminar <- rbind(M2022Eli, M2023Eli, M2024Eli)

#Verificamos que en la base general no encontramos alguna tipificación relacionada con algún informe de sanción



duplicados <- imput_eliminar %>% distinct(Informes, .keep_all = TRUE)
duplicados <- imput_eliminar %>% distinct(Informes, .keep_all = TRUE)
  
  
  datos[!duplicated(datos), ]


#Creamos una variable llave
names(df_factores)
df_factores$identificador <- paste(as.character(df_factores$Informes), as.character(df_factores$Imputacion), sep = "-")





#Unimos las bases de datos con las base anteriormente trabajada
df_factores_vf <- merge(df_factores, df_unido2, by="Expediente" )





df_unido2 <- merge(df_unido, sectores, by="Expediente", all.x = TRUE)


# Tratamiento de información

M2022F<- M2022 %>%
  filter(!Hecho_imputado %in% c("Reconsideración", "Multa coercitiva", "Sin especificar"))

M2023F<- M2023 %>%
  filter(!Hecho_imputado %in% c("Reconsideración", "multa coercitiva"))

M2024F<- M2024 %>%
  filter(!Hecho_imputado %in% c("Multa coercitiva", "Reconsideración", 
                                "Medida correctiva", "Informe de enmienda"))




#Ejemplo de Gráfico

install.packages("fmsb")
library(fmsb)

# Crear un marco de datos con los valores máximos, mínimos y los datos a graficar
data <- data.frame(
  Var1 = c(10, 0, 8),
  Var2 = c(10, 0, 3),
  Var3 = c(10, 0, 6),
  Var4 = c(10, 0, 7),
  Var5 = c(10, 0, 4)
)

radarchart(data)

radarchart(data, axistype = 1,
           pcol = "blue", pfcol = rgb(0.2, 0.5, 0.5, 0.5), plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.8,
           vlcex = 0.8)










#Exportamos la base de datos
library(openxlsx)
write.xlsx(df_general, file = "base5.xlsx")


#Otros líneas de código auxiliares
#df_unido_vf <- merge(sanciones[, c("imp", "multa")], df_unido[, c("imp", "Expediente")], by = "imp")



