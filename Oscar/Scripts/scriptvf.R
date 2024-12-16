

rm(list = ls())

#Instalamos las librerias necesarias
paquetes <- c("dplyr", "ggplot2", "readr", "httr")

# Función para instalar los paquetes si no están ya instalados
instalar_paquetes <- function(paquetes) {
  for (paquete in paquetes) {
        if (!require(paquete, character.only = TRUE)) {
      install.packages(paquete)
          library(paquete, character.only = TRUE)
    }
  }
}

# Llamar a la función con la lista de paquetes
instalar_paquetes(paquetes)


#Importar las base de datos 
url <- "https://github.com/PaoloValcarcel/OEFA_SMER/blob/dae31a411f440dbfb494109ed8bf2a5c6446c5fa/Paolo/Scripts/Factores/Factores.xlsx"

temp_archivo <- tempfile(fileext = ".xlsx")
download.file(url, temp_archivo, mode = "wb")

# Leer el archivo descargado
datos <- read_excel(temp_archivo)

# Opcional: eliminar el archivo temporal si ya no se necesita
unlink(temp_archivo)




datos <- read_csv(url)





