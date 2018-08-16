# Ejemplo de cómo se trae un SHAPE a R, la idea es luego hacerle un join con los datos de DGR(PRECIOS) y de CATASTRO(CARACTERÍSTICAS)

setwd("C:/Users/Usuario/Desktop/PP/Tesis/Housing-prices/Test shape")
library(rgdal)


####Spatial Polygon Data Frame
sp_df <- readOGR(dsn = "C:/Users/Usuario/Desktop/PP/Tesis/Housing-prices/Test shape", layer = "v_mdg_parcelas_geom")
print(sp_df)

#### Despliegue grafico del vectorial - mapa de Montevideo con todos los padrones!
plot(sp_df, col='blue', bg='white')
box()
for(i in 1:4) axis(i)

####ESTRUCTURA DE DATOS ESPACIALES EN r
#@data
#@polygons
head(sp_df@data)
sapply(sp_df@data, class)
str(sp_df@data)
df <- as.data.frame(sp_df@data)


#### Ejemplo From raw data to tecnically correct data
sp_df$PADRON <- as.numeric(as.character(sp_df$PADRON))

####Exploracion de datos
plot(sp_df)
sel <- sp_df$PROTECCION == "BIEN DE INTERES IMM"
plot(sp_df[sel,])

plot(sp_df, col = "lightgrey") # 
sel <- sp_df$PROTECCION == "BIEN DE INTERES IMM"
plot(sp_df[sel, ], col = "turquoise", add = TRUE) # add selected zones to map

library(maptools)  # to plot and label
plot(sp_df)
pointLabel(coordinates(sp_df), col= 2, cex = 0.5)
