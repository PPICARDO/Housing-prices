# La idea acá es sacar las coordenadas para los padrones, para eso necesitamos sacar los "centroides" de los polígonos del shape file

# calculo los centroides
centroides <- rgeos::gCentroid(sp_df_transformado, byid = TRUE)
# los paso a un dataframe
centroides_df <- as.data.frame(centroides)
# les agrego el padron
centroides_df <- cbind(centroides_df, sp_df_transformado@data$PADRON)
colnames(centroides_df)[1] <- "long"
colnames(centroides_df)[2] <- "lat"
colnames(centroides_df)[3] <- "PADRON"
summary(centroides_df)

# agrego los centroides dentro de la data del sp_df transformado
sp_df_transformado@data <- sp_df_transformado@data %>%
  left_join(centroides_df, by = "PADRON")

# centroides con las propiedades de transacciones (notar que algunos padrones se repiten en transacciones: algunas propiedades se venden más de una vez...)
transacciones_centroid <- merge(transacciones,centroides_df,by="PADRON")
summary(transacciones_centroid)

# intentemos poner en un mapa a los padrones que tuvieron transacciones (es una muestra)
library(ggmap)
library(ggplot2)

# getting the map
mapgilbert <- get_map(location = c(lon = mean(transacciones_centroid$long), lat = mean(transacciones_centroid$lat)), zoom = 11, maptype = "roadmap", scale = 2 )

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = transacciones_centroid, aes(x = transacciones_centroid$long, y = transacciones_centroid$lat, fill = "red", alpha = 0.2), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# SE GRAFICAN !! BIEN!!
# El gráfico anterior pone una muestra de transacciones en el mapa

