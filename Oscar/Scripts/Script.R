

rm(list = ls())

require(pacman)
p_load(tidyverse, rio, here, dplyr, viridis, readxl, stringr, 
       RColorBrewer, flextable, officer, classInt)


# Carga de información

M2022 <-read_excel("D:/OEFA/Metodología de multas OEFA/Informes_2022.xlsx", sheet = "Componentes")
M2023 <-read_excel("D:/OEFA/Metodología de multas OEFA/Informes_2023.xlsx", sheet = "Componentes")
M2024 <-read_excel("D:/OEFA/Metodología de multas OEFA/Informes_2024.xlsx", sheet = "Componentes")

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

#Unimos las bases de datos
df_unido <- rbind(M2022F,M2023F,M2024F)

#Generamos una variable con números consecutivos usando el operador :
df_unido$id2 <- 1:nrow(df_unido)

#Generamos un código para las imputaciones analizadas
df_unido$imp <- paste(as.character(df_unido$ID), as.character(df_unido$Num_Imputacion), sep = "-")
df_unido <- df_unido %>% rename(Expediente = Expedientes)

#Análisis de Costo Evitado
##########################

costo_evitado <- df_unido %>% group_by(imp) %>% summarize(costo=max(Sancion_total))
costo_evitado$costo <- as.numeric(costo_evitado$costo)

#Importamos RUIAS
ruias <- read_excel("D:/OEFA/Metodología de multas OEFA/RUIAS - CSEP.xlsx")

names(costo_evitado)
names(df_unido)
names(ruias)

base <- merge(costo_evitado[, c("imp", "costo")], df_unido[, c("imp", "Expediente")], by = "imp")
base2 <- merge(base[, c("imp", "costo", "Expediente")], ruias[, c("Expediente", "Sector económico")], by = "Expediente")


base2 <- base2 %>% group_by(imp) %>% summarize(costo=max(costo))

boxplot(base2$costo ~ base2$`Sector económico`, main = "Costo Evitado (S/.)",
        ylab = "Soles", xlab="Costo Evitado", col = "#FBB500", horizontal = FALSE,
        outline = TRUE)


