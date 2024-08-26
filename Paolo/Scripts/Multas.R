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

Datos <- bind_rows(
  mutate(D2022, Año = "2022"),
  mutate(D2023, Año = "2023"),
  mutate(D2024, Año = "2024")
)

# Creando gráfico tipo violín
ggplot(Datos, aes(x = Año, y = Prob_Detección, fill = Año)) +
  geom_violin(trim = FALSE,
              draw_quantiles = c(0.5, 0.80, 0.90)) +
  labs(title = "Distribución de probabilidades de detección por Año",
       x = "Año",
       y = "Mediana, P80 y P90") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())

# Creando histograma
ggplot(Datos, aes(x = Prob_Detección, fill = Año)) +
  geom_histogram(binwidth = 0.05, color = "black", alpha = 0.7, position = "dodge") +
  labs(title = "Distribución de Probabilidad de Detección por Año",
       x = "Probabilidad de Detección",
       y = "Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())


###########################
#### Beneficio ilícito ####
###########################

B2022 <- M2022F %>% dplyr::select(Informes, Beneficio_ilícito, Colapsar)

Max22 <- B2022 %>%
  filter(Colapsar == "Máximo") %>%
  group_by(Informes) %>%
  summarize(Beneficio_ilícito = max(Beneficio_ilícito)) %>%
  ungroup()

Sum22 <- B2022 %>%
  filter(Colapsar == "Suma") %>%
  group_by(Informes) %>%
  summarize(Beneficio_ilícito = sum(Beneficio_ilícito)) %>%
  ungroup()

B2022SN <- B2022 %>%
  filter(!(Colapsar %in% c("Máximo", "Suma")))

B2022SN$Colapsar <- NULL
B2022F <- bind_rows(Max22, Sum22, B2022SN)


B2023 <- M2023F %>% dplyr::select(Informes, Beneficio_ilícito, Colapsar)
B2023$Beneficio_ilícito <- as.numeric(B2023$Beneficio_ilícito)

Max23 <- B2023 %>%
  filter(Colapsar == "Máximo") %>%
  group_by(Informes) %>%
  summarize(Beneficio_ilícito = max(Beneficio_ilícito)) %>%
  ungroup()

Sum23 <- B2023 %>%
  filter(Colapsar == "Suma") %>%
  group_by(Informes) %>%
  summarize(Beneficio_ilícito = sum(Beneficio_ilícito)) %>%
  ungroup()

B2023SN <- B2023 %>%
  filter(!(Colapsar %in% c("Máximo", "Suma")))

B2023SN$Colapsar <- NULL
B2023F <- bind_rows(Max23, Sum23, B2023SN)


B2024 <- M2024F %>% dplyr::select(Informes, Beneficio_ilícito, Colapsar)

Max24 <- B2024 %>%
  filter(Colapsar == "Máximo") %>%
  group_by(Informes) %>%
  summarize(Beneficio_ilícito = max(Beneficio_ilícito)) %>%
  ungroup()

Sum24 <- B2024 %>%
  filter(Colapsar == "Suma") %>%
  group_by(Informes) %>%
  summarize(Beneficio_ilícito = sum(Beneficio_ilícito)) %>%
  ungroup()

B2024SN <- B2024 %>%
  filter(!(Colapsar %in% c("Máximo", "Suma")))

B2024SN$Colapsar <- NULL
B2024F <- bind_rows(Max24, Sum24, B2024SN)

rm(Max22, Sum22, B2022SN, Max23, Sum23, B2023SN, Max24, Sum24, B2024SN)


# Gráfico de cajas agrupado general
Benef_ili <- list("2022" = B2022F$Beneficio_ilícito, 
                  "2023" = B2023F$Beneficio_ilícito, 
                  "2024" = B2024F$Beneficio_ilícito)

boxplot(Benef_ili ,
        main = "Boxplot del beneficio ilicito por años",
        ylab = "",
        col = c("lightblue", "lightgreen", "lightcoral"),  
        pch = 19)  


quantile(B2022F$Beneficio_ilícito, probs = c(0.80))
quantile(B2023F$Beneficio_ilícito, probs = c(0.80))
quantile(B2024F$Beneficio_ilícito, probs = c(0.80))

B2022FO <- B2022F %>% filter(Beneficio_ilícito < 15.1474)
B2023FO <- B2023F %>% filter(Beneficio_ilícito < 12.193)
B2024FO <- B2024F %>% filter(Beneficio_ilícito < 4.985)


# Gráfico de cajas para el percentil 80
Benef_ili2 <- list("2022" = B2022FO$Beneficio_ilícito, 
                  "2023" = B2023FO$Beneficio_ilícito, 
                  "2024" = B2024FO$Beneficio_ilícito)

boxplot(Benef_ili2 ,
        main = "Boxplot del beneficio ilicito debajo del percentil 80",
        ylab = "Por años",
        col = c("lightblue", "lightgreen", "lightcoral"),  
        pch = 19)  


#########################
#### Tiempo en meses ####
#########################

T2022 <- as.data.frame(M2022F$T_meses)
colnames(T2022) <- "T_meses"
T2022$T_meses <- as.numeric(T2022$T_meses)

T2023 <- as.data.frame(M2023F$T_meses)
colnames(T2023) <- "T_meses"
T2023$T_meses <- as.numeric(T2023$T_meses)

T2024 <- as.data.frame(M2024F$T_meses)
colnames(T2024) <- "T_meses"
T2024$T_meses <- as.numeric(T2024$T_meses)


quantile(T2022$T_meses, probs = c(0.80))
quantile(T2023$T_meses, probs = c(0.80))
quantile(T2024$T_meses, probs = c(0.80))

T2022F <- T2022 %>% filter(T_meses < 42.74)
T2023F <- T2023 %>% filter(T_meses < 43.8374)
T2024F <- T2024 %>% filter(T_meses < 44.1998)


Tiempos <- bind_rows(
  mutate(T2022F, Año = "2022"),
  mutate(T2023F, Año = "2023"),
  mutate(T2024F, Año = "2024"))


# Creando gráfico tipo violín
ggplot(Tiempos, aes(x = Año, y = T_meses, fill = Año)) +
  geom_violin(trim = FALSE,
              draw_quantiles = c(0.5, 0.80, 0.90)) +
  labs(title = "Distribución del tiempo en meses por Año",
       x = "Año",
       y = "Mediana, P80 y P90") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) 

# Gráfico de cajas para el percentil 80
T_Meses <- list("2022" = T2022F$T_meses, 
                "2023" = T2023F$T_meses, 
                "2024" = T2024F$T_meses)

boxplot(T_Meses,
        main = "Boxplot del tiempo en meses debajo del percentil 80",
        ylab = "Por años",
        col = c("lightblue", "lightgreen", "lightcoral"),  
        pch = 19)  
