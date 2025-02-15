require(pacman)
p_load(pdftools,
       stringr,
       wordcloud,
       tesseract,
       data.table,
       dplyr,
       tabulapdf,
       tidyverse,
       ggplot2,
       openxlsx)

rm(list=ls())

################################
######## PARA LOS NUEVOS #######
################################

### ---- Programando para el ASUNTO del 2024 -----###

C2024 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2024/Nuevos"
PDF_2024 <- list.files(C2024, full.names = TRUE)
PDF_2024 <- as.data.frame(PDF_2024)
Drop2024 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2024/Nuevos/"
F2024 <- sub(Drop2024, "", PDF_2024$PDF_2024) 
F2024 <- as.data.frame(F2024)
rm(PDF_2024, C2024, Drop2024)
F2024$id <- 1:nrow(F2024)

Ruta_2024 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2024/Nuevos/"

leer_pdf <- function(Nombre) {
  ruta_completa <- paste0(Ruta_2024, Nombre)
  pdf_text(ruta_completa)
}

contenido_pdfs <- lapply(F2024$F2024, leer_pdf)

for (i in 1:length(contenido_pdfs)) {
  assign(paste0("Informe_", i), contenido_pdfs[[i]])
}

### ---- Asunto 2024 -----###

Texto <- list()
Asunto <- lapply(1:97, function(i) get(paste0("Informe_", i)))

for (i in 1:97) {
  Asunto_actual <- Asunto[[i]]
  Asunto_actual <- str_replace_all(Asunto_actual, "\\s+", " ")
  ASNT <- str_sub(str_extract(Asunto_actual, "(?<=ASUNTO : ).*"), 1, 45)
  ASNT <- unlist(ASNT)
  ASNT <- ASNT[!is.na(ASNT)]
  
  if (length(ASNT) > 0) {
    Texto[[i]] <- data.table(Correlativo = rep(i, length(ASNT)), Oracion = ASNT)
  }
}

Asunto_2024N <- rbindlist(Texto)
rm(Asunto_actual, ASNT, Texto, Asunto, F2024, contenido_pdfs)


### ---- Informes 2024 -----###

resultados <- list()
informes <- lapply(1:97, function(i) get(paste0("Informe_", i)))
for (i in 1:97) {
  informe_actual <- informes[[i]]
  informe_actual <- str_replace_all(informe_actual, "\\s+", " ")
  informe <- str_extract(informe_actual, "(?<=INFORME N°\\s).*?(?=\\sA\\s:)")
  informe <- informe[!is.na(informe)]
  resultados[[i]] <- informe
}

Informes_2024N <- data.table(
  Correlativo = seq_along(resultados),
  Informes = unlist(lapply(resultados, function(x) ifelse(is.null(x), NA, x))))

rm(informes, resultados,contenido_pdfs)

Final24N <- left_join(Informes_2024N, Asunto_2024N, by = "Correlativo")
rm(Informes_2024N, Asunto_2024N)

################################################################################

### ---- Programando para el ASUNTO del 2023 -----###

C2023 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2023/Nuevos"
PDF_2023 <- list.files(C2023, full.names = TRUE)
PDF_2023 <- as.data.frame(PDF_2023)
Drop2023 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2023/Nuevos/"
F2023 <- sub(Drop2023, "", PDF_2023$PDF_2023) 
F2023 <- as.data.frame(F2023)
rm(PDF_2023, C2023, Drop2023)
F2023$id <- 1:nrow(F2023)

Ruta_2023 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2023/Nuevos/"

leer_pdf <- function(Nombre) {
  ruta_completa <- paste0(Ruta_2023, Nombre)
  pdf_text(ruta_completa)
}

contenido_pdfs <- lapply(F2023$F2023, leer_pdf)

for (i in 1:length(contenido_pdfs)) {
  assign(paste0("Informe_", i), contenido_pdfs[[i]])
}

### ---- Asunto 2023 -----###

Texto <- list()
Asunto <- lapply(1:48, function(i) get(paste0("Informe_", i)))

for (i in 1:48) {
  Asunto_actual <- Asunto[[i]]
  Asunto_actual <- str_replace_all(Asunto_actual, "\\s+", " ")
  ASNT <- str_sub(str_extract(Asunto_actual, "(?<=ASUNTO : ).*"), 1, 45)
  ASNT <- unlist(ASNT)
  ASNT <- ASNT[!is.na(ASNT)]
  
  if (length(ASNT) > 0) {
    Texto[[i]] <- data.table(Correlativo = rep(i, length(ASNT)), Oracion = ASNT)
  }
}

Asunto_2023N <- rbindlist(Texto)
rm(Asunto_actual, ASNT, Texto, Asunto, F2023, contenido_pdfs)

### ---- Informes 2023 -----###

resultados <- list()
informes <- lapply(1:48, function(i) get(paste0("Informe_", i)))
for (i in 1:48) {
  informe_actual <- informes[[i]]
  informe_actual <- str_replace_all(informe_actual, "\\s+", " ")
  informe <- str_extract(informe_actual, "(?<=INFORME N°\\s).*?(?=\\sA\\s:)")
  informe <- informe[!is.na(informe)]
  resultados[[i]] <- informe
}

Informes_2023N <- data.table(
  Correlativo = seq_along(resultados),
  Informes = unlist(lapply(resultados, function(x) ifelse(is.null(x), NA, x))))

rm(informes, resultados)

Final23N <- left_join(Informes_2023N, Asunto_2023N, by = "Correlativo")
rm(Informes_2023N, Asunto_2023N)

Final23N <- Final23N %>%
  mutate(Informes = ifelse(Correlativo == 8, "00433-2023-OEFA/DFAI-SSAG", Informes))

Final23N <- Final23N %>%
  mutate(Informes = ifelse(Correlativo == 35, "04604-2023-OEFA/DFAI-SSAG", Informes))

################################################################################

### ---- Programando para el ASUNTO del 2022 -----###

C2022 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2022/Nuevos"
PDF_2022 <- list.files(C2022, full.names = TRUE)
PDF_2022 <- as.data.frame(PDF_2022)
Drop2022 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2022/Nuevos/"
F2022 <- sub(Drop2022, "", PDF_2022$PDF_2022) 
F2022 <- as.data.frame(F2022)
rm(PDF_2022, C2022, Drop2022)
F2022$id <- 1:nrow(F2022)

Ruta_2022 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2022/Nuevos/"

leer_pdf <- function(Nombre) {
  ruta_completa <- paste0(Ruta_2022, Nombre)
  pdf_text(ruta_completa)
}

contenido_pdfs <- lapply(F2022$F2022, leer_pdf)

for (i in 1:length(contenido_pdfs)) {
  assign(paste0("Informe_", i), contenido_pdfs[[i]])
}

### ---- Asunto 2022 -----###

Texto <- list()
Asunto <- lapply(1:48, function(i) get(paste0("Informe_", i)))

for (i in 1:48) {
  Asunto_actual <- Asunto[[i]]
  Asunto_actual <- str_replace_all(Asunto_actual, "\\s+", " ")
  ASNT <- str_sub(str_extract(Asunto_actual, "(?<=ASUNTO : ).*"), 1, 45)
  ASNT <- unlist(ASNT)
  ASNT <- ASNT[!is.na(ASNT)]
  
  if (length(ASNT) > 0) {
    Texto[[i]] <- data.table(Correlativo = rep(i, length(ASNT)), Oracion = ASNT)
  }
}

Asunto_2022N <- rbindlist(Texto)
rm(Asunto_actual, ASNT, Texto, Asunto, F2022, contenido_pdfs)

### ---- Informes 2022 -----###

resultados <- list()
informes <- lapply(1:48, function(i) get(paste0("Informe_", i)))
for (i in 1:48) {
  informe_actual <- informes[[i]]
  informe_actual <- str_replace_all(informe_actual, "\\s+", " ")
  informe <- str_extract(informe_actual, "(?<=INFORME N°\\s).*?(?=\\sA\\s:)")
  informe <- informe[!is.na(informe)]
  resultados[[i]] <- informe
}

Informes_2022N <- data.table(
  Correlativo = seq_along(resultados),
  Informes = unlist(lapply(resultados, function(x) ifelse(is.null(x), NA, x))))

rm(informes, resultados)

Final22N <- left_join(Informes_2022N, Asunto_2022N, by = "Correlativo")
rm(Informes_2022N, Asunto_2022N)


Final22N <- Final22N %>%
  mutate(Informes = ifelse(Correlativo == 5, "00147-2022-OEFA/DFAI-SSAG", Informes))

Final22N <- Final22N %>%
  mutate(Informes = ifelse(Correlativo == 9, "00490-2022-OEFA/DFAI-SSAG", Informes))

Final22N <- Final22N %>%
  mutate(Informes = ifelse(Correlativo == 32, "01518-2022-OEFA/DFAI-SSAG", Informes))

# Exportando las bases 

wb <- createWorkbook()

# Añadiendo la primera hoja con el primer dataframe
addWorksheet(wb, "Final22N")
writeData(wb, "Final22N", Final22N, colNames = TRUE)

# Añadiendo la segunda hoja con el segundo dataframe
addWorksheet(wb, "Final23N")
writeData(wb, "Final23N", Final23N, colNames = TRUE)

# Añadiendo la segunda hoja con el segundo dataframe
addWorksheet(wb, "Final24N")
writeData(wb, "Final24N", Final24N, colNames = TRUE)

# Guardardando el archivo Excel
saveWorkbook(wb, "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Asuntos_1.xlsx", overwrite = TRUE)


##################################
######## PARA LOS ANTIGUOS #######
##################################

rm(list=ls())

C2022 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2022"
C2023 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2023"
C2024 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2024"

PDF_2022 <- list.files(C2022, full.names = TRUE)
PDF_2023 <- list.files(C2023, full.names = TRUE)
PDF_2024 <- list.files(C2024, full.names = TRUE)

PDF_2022 <- as.data.frame(PDF_2022)
PDF_2023 <- as.data.frame(PDF_2023)
PDF_2024 <- as.data.frame(PDF_2024)

Drop2022 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2022/"
Drop2023 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2023/"
Drop2024 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2024/"

F2022 <- sub(Drop2022, "", PDF_2022$PDF_2022)
F2023 <- sub(Drop2023, "", PDF_2023$PDF_2023)
F2024 <- sub(Drop2024, "", PDF_2024$PDF_2024)

F2022 <- as.data.frame(F2022)
F2023 <- as.data.frame(F2023)
F2024 <- as.data.frame(F2024)

rm(PDF_2022, C2022, PDF_2023, C2023, PDF_2024, C2024, Drop2022, Drop2023, Drop2024)

F2022 <- F2022[F2022$F2022 != "Nuevos", , drop = FALSE]
F2023 <- F2023[F2023$F2023 != "Nuevos", , drop = FALSE]
F2024 <- F2024[F2024$F2024 != "Nuevos", , drop = FALSE]

# Generando la base para el 2024

Ruta_2024 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2024/"

leer_pdf <- function(Nombre) {
  ruta_completa <- paste0(Ruta_2024, Nombre)
  pdf_text(ruta_completa)
}

contenido_pdfs <- lapply(F2024$F2024, leer_pdf)

for (i in 1:length(contenido_pdfs)) {
  assign(paste0("Informe_", i), contenido_pdfs[[i]])
}

### ---- Asunto 2024 -----###

Texto <- list()
Asunto <- lapply(1:184, function(i) get(paste0("Informe_", i)))

for (i in 1:184) {
  Asunto_actual <- Asunto[[i]]
  Asunto_actual <- str_replace_all(Asunto_actual, "\\s+", " ")
  ASNT <- str_sub(str_extract(Asunto_actual, "(?<=ASUNTO : ).*"), 1, 45)
  ASNT <- unlist(ASNT)
  ASNT <- ASNT[!is.na(ASNT)]
  
  if (length(ASNT) > 0) {
    Texto[[i]] <- data.table(Correlativo = rep(i, length(ASNT)), Oracion = ASNT)
  }
}

Asunto_2024 <- rbindlist(Texto)
rm(Asunto_actual, ASNT, Texto, Asunto, F2024, contenido_pdfs)


### ---- Informes 2024 -----###

resultados <- list()
informes <- lapply(1:184, function(i) get(paste0("Informe_", i)))
for (i in 1:184) {
  informe_actual <- informes[[i]]
  informe_actual <- str_replace_all(informe_actual, "\\s+", " ")
  informe <- str_extract(informe_actual, "(?<=INFORME N°\\s).*?(?=\\sA\\s:)")
  informe <- informe[!is.na(informe)]
  resultados[[i]] <- informe
}

Informes_2024 <- data.table(
  Correlativo = seq_along(resultados),
  Informes = unlist(lapply(resultados, function(x) ifelse(is.null(x), NA, x))))

rm(informes, resultados)

Final24 <- left_join(Informes_2024, Asunto_2024, by = "Correlativo")
rm(Informes_2024, Asunto_2024)

Final24 <- Final24 %>%
  mutate(Informes = ifelse(Correlativo == 72, "00474-2024-OEFA/DFAI-SSAG", Informes))

Final24 <- Final24 %>%
  mutate(Informes = ifelse(Correlativo == 143, "01033-2024-OEFA/DFAI-SSAG", Informes))

################################################################################

# Generando la base para el 2023

Ruta_2023 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2023/"

leer_pdf <- function(Nombre) {
  ruta_completa <- paste0(Ruta_2023, Nombre)
  pdf_text(ruta_completa)
}

contenido_pdfs <- lapply(F2023$F2023, leer_pdf)

for (i in 1:length(contenido_pdfs)) {
  assign(paste0("Informe_", i), contenido_pdfs[[i]])
}

### ---- Asunto 2023 -----###

Texto <- list()
Asunto <- lapply(1:92, function(i) get(paste0("Informe_", i)))

for (i in 1:92) {
  Asunto_actual <- Asunto[[i]]
  Asunto_actual <- str_replace_all(Asunto_actual, "\\s+", " ")
  ASNT <- str_sub(str_extract(Asunto_actual, "(?<=ASUNTO : ).*"), 1, 45)
  ASNT <- unlist(ASNT)
  ASNT <- ASNT[!is.na(ASNT)]
  
  if (length(ASNT) > 0) {
    Texto[[i]] <- data.table(Correlativo = rep(i, length(ASNT)), Oracion = ASNT)
  }
}

Asunto_2023 <- rbindlist(Texto)
rm(Asunto_actual, ASNT, Texto, Asunto, F2023, contenido_pdfs)

### ---- Informes 2023 -----###

resultados <- list()
informes <- lapply(1:92, function(i) get(paste0("Informe_", i)))
for (i in 1:92) {
  informe_actual <- informes[[i]]
  informe_actual <- str_replace_all(informe_actual, "\\s+", " ")
  informe <- str_extract(informe_actual, "(?<=INFORME N°\\s).*?(?=\\sA\\s:)")
  informe <- informe[!is.na(informe)]
  resultados[[i]] <- informe
}

Informes_2023 <- data.table(
  Correlativo = seq_along(resultados),
  Informes = unlist(lapply(resultados, function(x) ifelse(is.null(x), NA, x))))

rm(informes, resultados)

Final23 <- left_join(Informes_2023, Asunto_2023, by = "Correlativo")
rm(Informes_2023, Asunto_2023)

Final23 <- Final23 %>%
  mutate(Informes = ifelse(Correlativo == 30, "01806-2023-OEFA/DFAI-SSAG", Informes))

Final23 <- Final23 %>%
  mutate(Informes = ifelse(Correlativo == 53, "04001-2023-OEFA/DFAI-SSAG", Informes))

################################################################################

# Generando la base para el 2022

Ruta_2022 <- "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Informes 2022/"

leer_pdf <- function(Nombre) {
  ruta_completa <- paste0(Ruta_2022, Nombre)
  pdf_text(ruta_completa)
}

contenido_pdfs <- lapply(F2022$F2022, leer_pdf)

for (i in 1:length(contenido_pdfs)) {
  assign(paste0("Informe_", i), contenido_pdfs[[i]])
}


### ---- Asunto 2022 -----###

Texto <- list()
Asunto <- lapply(1:92, function(i) get(paste0("Informe_", i)))

for (i in 1:92) {
  Asunto_actual <- Asunto[[i]]
  Asunto_actual <- str_replace_all(Asunto_actual, "\\s+", " ")
  ASNT <- str_sub(str_extract(Asunto_actual, "(?<=ASUNTO : ).*"), 1, 45)
  ASNT <- unlist(ASNT)
  ASNT <- ASNT[!is.na(ASNT)]
  
  if (length(ASNT) > 0) {
    Texto[[i]] <- data.table(Correlativo = rep(i, length(ASNT)), Oracion = ASNT)
  }
}

Asunto_2022 <- rbindlist(Texto)
rm(Asunto_actual, ASNT, Texto, Asunto, F2022, contenido_pdfs)

### ---- Informes 2022 -----###

resultados <- list()
informes <- lapply(1:92, function(i) get(paste0("Informe_", i)))
for (i in 1:92) {
  informe_actual <- informes[[i]]
  informe_actual <- str_replace_all(informe_actual, "\\s+", " ")
  informe <- str_extract(informe_actual, "(?<=INFORME N°\\s).*?(?=\\sA\\s:)")
  informe <- informe[!is.na(informe)]
  resultados[[i]] <- informe
}

Informes_2022 <- data.table(
  Correlativo = seq_along(resultados),
  Informes = unlist(lapply(resultados, function(x) ifelse(is.null(x), NA, x))))

rm(informes, resultados)

Final22 <- left_join(Informes_2022, Asunto_2022, by = "Correlativo")
rm(Informes_2022, Asunto_2022)

Final22 <- Final22 %>%
  mutate(Informes = ifelse(Correlativo == 16, "02324-2022-OEFA/DFAI-SSAG", Informes))

Final22 <- Final22 %>%
  mutate(Informes = ifelse(Correlativo == 20, "00834-2022-OEFA/DFAI-SSAG", Informes))

Final22 <- Final22 %>%
  mutate(Informes = ifelse(Correlativo == 21, "01097-2022-OEFA/DFAI-SSAG", Informes))

Final22 <- Final22 %>%
  mutate(Informes = ifelse(Correlativo == 29, "00327-2022-OEFA/DFAI-SSAG", Informes))

Final22 <- Final22 %>%
  mutate(Informes = ifelse(Correlativo == 89, "02991-2022-OEFA/DFAI-SSAG", Informes))

# Exportando las bases 

wb <- createWorkbook()

# Añadiendo la primera hoja con el primer dataframe
addWorksheet(wb, "Final22")
writeData(wb, "Final22", Final22, colNames = TRUE)

# Añadiendo la segunda hoja con el segundo dataframe
addWorksheet(wb, "Final23")
writeData(wb, "Final23", Final23, colNames = TRUE)

# Añadiendo la segunda hoja con el segundo dataframe
addWorksheet(wb, "Final24")
writeData(wb, "Final24", Final24, colNames = TRUE)

# Guardardando el archivo Excel
saveWorkbook(wb, "D:/LAPTOP ACER/LOCACION OEFA/Archivos/Asuntos_2.xlsx", overwrite = TRUE)
