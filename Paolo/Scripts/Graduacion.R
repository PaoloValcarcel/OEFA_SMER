rm(list = ls())

require(pacman)
p_load(readxl, tidyverse, rio, here, dplyr, writexl)


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


##########################################################################################################

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

