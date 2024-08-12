
rm(list = ls())

require(pacman)
p_load(tidyverse, rio, here, dplyr, viridis, readxl, stringr, 
       RColorBrewer, flextable, officer, classInt)


# Carga de información

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

#########################
### Sanciones totales ###
#########################

Sancion22  <- M2022F  %>%
  group_by(Informes) %>%
  summarize(suma_valor = max(Sancion_total))

Sancion23  <- M2023F  %>%
  group_by(Informes) %>%
  summarize(suma_valor = max(Sancion_total))

Sancion24  <- M2024F  %>%
  group_by(Informes) %>%
  summarize(suma_valor = max(Sancion_total))

Sancion22$suma_valor <- as.numeric(as.character(Sancion22$suma_valor))
Sancion23$suma_valor <- as.numeric(as.character(Sancion23$suma_valor))
Sancion24$suma_valor <- as.numeric(as.character(Sancion24$suma_valor))

Sancion22$round <- round(Sancion22$suma_valor, 0)
Sancion23$round <- round(Sancion23$suma_valor, 0)
Sancion24$round <- round(Sancion24$suma_valor, 0)

# Gráfico general 2022

outliers22 <- boxplot(Sancion22$round,
                    main = "Boxplot de las sanciones en UIT",
                    ylab= "Año 2022",
                    col = "lightblue")

outliers22 <- outliers22$out
print(outliers22)
outliers22 <- outliers22[outliers22 >= 719]
outliers22_format <- format(outliers22, big.mark = ",", scientific = FALSE)

text(x = rep(1, length(outliers22)), 
     y = outliers22, 
     labels = outliers22_format, 
     pos = 4, 
     col = "red",
     cex = 0.9) 


# Gráfico general 2023

outliers23 <- boxplot(Sancion23$round,
                    main = "Boxplot de las sanciones en UIT",
                    ylab= "Año 2023",
                    col = "lightblue")

outliers23 <- outliers23$out
print(outliers23)
outliers23 <- outliers23[outliers23 >= 871]
outliers23_format <- format(outliers23, big.mark = ",", scientific = FALSE)

text(x = rep(1, length(outliers23)), 
     y = outliers23, 
     labels = outliers23_format, 
     pos = 4, 
     col = "red",
     cex = 0.9) 

# Gráfico general 2024

outliers24 <- boxplot(Sancion24$round,
                    main = "Boxplot de las sanciones en UIT",
                    ylab= "Año 2024",
                    col = "lightblue")

outliers24 <- outliers24$out
print(outliers24)
outliers24 <- outliers24[outliers24 >= 1799.48]
outliers24_format <- format(outliers24, big.mark = ",", scientific = FALSE)

text(x = rep(1, length(outliers24)), 
     y = outliers24, 
     labels = outliers24_format, 
     pos = 4, 
     col = "red",
     cex = 0.9) 


# De manera agregada para todos los años

sanciones_general <- list("2022" = Sancion22$suma_valor, 
                       "2023" = Sancion23$suma_valor, 
                       "2024" = Sancion24$suma_valor)

boxplot(sanciones_general,
        main = "Boxplot de las sanciones totales",
        ylab = "Valor en UIT",
        col = c("lightblue", "lightgreen", "lightcoral"),  
        pch = 19)  


# Gráfico para el percentil 80 de las sanciones 2022

Sancion22$round2 <- round(Sancion22$suma_valor, 2)

quantile(Sancion22$suma_valor, probs = c(0.80, 0.90))

Sancion22_SO <- Sancion22 %>%
  filter(round2 < 81.11)

outliers22_2 <- boxplot(Sancion22_SO$round2,
                     main = "Boxplot de las sanciones por debajo del percentil 80",
                     ylab = "Año 2022",
                     col = "lightblue")

outliers22_2 <- outliers22_2$out
print(outliers22_2)
outliers_format22_2 <- format(outliers22_2, big.mark = ",", scientific = FALSE)

text(x = rep(1, length(outliers22_2)), 
     y = outliers22_2, 
     labels = outliers_format22_2, 
     pos = 4, 
     col = "red",
     cex = 0.9) 


# Gráfico para el percentil 80 de las sanciones 2023

Sancion23$round2 <- round(Sancion23$suma_valor, 2)

quantile(Sancion23$suma_valor, probs = c(0.80, 0.90))

Sancion23_SO <- Sancion23 %>%
  filter(round2 < 80.23)

outliers23_2 <- boxplot(Sancion23_SO$round2,
                     main = "Boxplot de las sanciones por debajo del percentil 80",
                     ylab = "Año 2023",
                     col = "lightblue")

outliers23_2 <- outliers23_2$out
print(outliers23_2)
outliers_format23_2 <- format(outliers23_2, big.mark = ",", scientific = FALSE)

text(x = rep(1, length(outliers23_2)), 
     y = outliers23_2, 
     labels = outliers_format23_2, 
     pos = 4, 
     col = "red",
     cex = 0.9) 


# Gráfico para el percentil 80 de las sanciones 2024

Sancion24$round2 <- round(Sancion24$suma_valor, 2)

quantile(Sancion24$suma_valor, probs = c(0.80, 0.90))

Sancion24_SO <- Sancion24 %>%
  filter(round2 < 62.58)

outliers24_2 <- boxplot(Sancion24_SO$round2,
             main = "Boxplot de las sanciones por debajo del percentil 80",
             ylab = "Año 2024",
             col = "lightblue")

outliers24_2 <- outliers24_2$out
print(outliers24_2)
outliers_format24_2 <- format(outliers24_2, big.mark = ",", scientific = FALSE)

text(x = rep(1, length(outliers24_2)), 
     y = outliers24_2, 
     labels = outliers_format24_2, 
     pos = 4, 
     col = "red",
     cex = 0.9) 

# De manera agregada para todos los años

sanciones_p80 <- list("2022" = Sancion22_SO$round2, 
                       "2023" = Sancion23_SO$round2, 
                       "2024" = Sancion24_SO$round2)

boxplot(sanciones_p80,
        main = "Boxplot de las sanciones por debajo del percentil 80",
        ylab = "Valor en UIT",
        col = c("lightblue", "lightgreen", "lightcoral"),  
        pch = 19)  


###################################
#### Probabilidad de detección ####
###################################

D2022 <- as.data.frame(M2022F$Prob_Detección)
colnames(D2022) <- "Prob_Detección"

D2023 <- as.data.frame(M2023F$Prob_Detección)
colnames(D2023) <- "Prob_Detección"

D2024 <- as.data.frame(M2024F$Prob_Detección)
colnames(D2024) <- "Prob_Detección"

datos_combinados <- bind_rows(
  mutate(D2022, Año = "2022"),
  mutate(D2023, Año = "2023"),
  mutate(D2024, Año = "2024")
)

# Creando gráfico tipo violín
ggplot(datos_combinados, aes(x = Año, y = Prob_Detección, fill = Año)) +
  geom_violin(trim = FALSE,
              draw_quantiles = c(0.5, 0.80, 0.90)) +
  labs(title = "Distribución de probabilidades de detección por Año",
       x = "Año",
       y = "Mediana, P80 y P90") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 

