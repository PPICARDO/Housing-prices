
library(rgdal)
library(readxl)
library(readr)

setwd("C:/Users/Usuario/Desktop/PP/Tesis/Ayuda de Joselina con dgr dgc y shape")

# levantar los datos a usar
# 1 # PARCELAS PADRONES
if ( !exists("sp_df")) {
  sp_df <- rgdal::readOGR(dsn = "inputs", layer = "v_mdg_parcelas")
} else (warning("sp_df ya existe, no se volvió a cargar"))

# 2 DATOS CATASTRO (muestra de datos)
if ( !exists("catastro")) {
  catastro <- read.csv("inputs/Padrones_Urbanos.csv", header = FALSE)
  } else (warning("los datos de catastro de padrones urbanos ya existen, no se volviÃ³ a cargar"))


# 3 # DATOS TRANSACCIONES DGR (muestra pequeña de datos)
if ( !exists("transacciones")) {
  transacciones <- read_excel("inputs/DGR_muestra.xlsx")
} else (warning("el archivo de transacciones ya existe, no se volviÃ³ a cargar"))

## carga de datos oka!!
