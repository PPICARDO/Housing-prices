# librerias  y setwd ----
setwd("~/Housing-prices-tesina/DGR+DGC+SHAPE")

# levantar datos  ----
source("scripts/1_carga de datos.R")

# modificar el sistema de coordenadas  ----
source("scripts/2_transformar data espacial.R")

# agregar centroides e Id (padron)  ----
source("scripts/3_agregarCentroides.R")

# agregar variables a los polígonos (transacciones y catastro)----
source("scripts/4_agregarColumnas.R")


# Merge con todos los datos  ----
df <- sp_df_transformado@data
df$PADRON <- as.numeric(as.character(df$PADRON))
dftotal <- left_join(df, transacciones, by = "PADRON")
dftotal <- left_join(dftotal, catastro, by = "PADRON")

# DATA SET DE TRANSACCIONES con CATASTRO (para los casos que hay transacciones)  ----

catastro$unidad <- as.numeric(catastro$unidad)
transacciones$UNIDAD <- as.numeric(transacciones$UNIDAD)
transacciones$catastro <- as.factor(catastro$PADRON)
transac_catastro <- left_join(transacciones, catastro , by = c("PADRON", "UNIDAD" = "unidad"))

# Notar que pierdo algunas transacciones en este merge, tema de formato de "unidad" quizás

# Poner coordenadas al data set de transacciones y catastro ----

transacciones$PADRON <- as.factor(transacciones$PADRON)
catastro$PADRON <- as.factor(catastro$PADRON)
dftotal$PADRON <- as.factor(dftotal$PADRON)

catastro$UNIDAD <- catastro$unidad
# MERGE transacciones con catastro
transac_catastro <- merge(transacciones, catastro , by = "PADRON")


# MERGE lo anterior con las coordenadas
transac_catastro <- merge.data.frame(transacciones, centroides_df , by = "PADRON")


transac_catastro2 <- transac_catastro[complete.cases(transac_catastro[ , 7]),]
transac_catastro2



