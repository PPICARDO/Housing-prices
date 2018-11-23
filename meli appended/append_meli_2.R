# Trabajar con datos de MELI # test_1
load(file = "meli appended/append_meli_1.Rda")
dim(test_1)
# 284788    178

# saco los duplicados
test_2 <- unique(test_1) # (49934 duplicados, ni idea por qué hay tantos, parecen muchos ...)
dim(test_2)

#selecciono algunas variables para empezar a describir un poco y HACER !
test_3 <- unique(test_1[c("id","titulo","tipo_inmueble","precio","moneda","condicion","barrio",
                          "direccion","lat","long","terraza","cocina","comedor","estado",
                          "sup_constru","sup_cub","sup_terr","sup_terr2","mes","start_time",
                          "sup_tot","tipo_ubicacion","antiguedad","luminosidad")]) 

str(test_3) # 23 variables (algunas a complemetar en una variable)

#guardo el archivo
save(test_3, file = "muestra_var_meli.Rda")
#cargo el archivo
load(file = "muestra_var_meli.Rda")

# variables factor:
fact <- c("tipo_inmueble","moneda","condicion","barrio","terraza","cocina","comedor",
          "estado","mes","tipo_ubicacion","antiguedad","luminosidad")
test_3[,fact] <- lapply(test_3[,fact], as.factor)


#barrio
test_3$barrio <- gsub("<f3>", "o", test_3$barrio)

# variables numéricas, limpiamos un poco antes:

### sup_constru y sup_cub ----
test_3$sup_constru <- gsub("-", "", test_3$sup_constru)
# saco todo lo que haya desp del espacio
test_3$sup_constru <- gsub(" .*", "", test_3$sup_constru)
test_3$sup_cub <- gsub(" .*", "", test_3$sup_cub)
# complemento las dos variables
test_3$sup_constru <- ifelse(!is.na(test_3$sup_constru), test_3$sup_constru, test_3$sup_cub)


# sup_terr y sup_terr2 ----
 test_3$sup_terr <- gsub("-", "", test_3$sup_terr) 
 test_3$sup_terr <- gsub(" .*", "", test_3$sup_terr)
 test_3$sup_terr <- gsub("1111", "", test_3$sup_terr)

 
   # saco valores atípicos
 test_3$sup_terr <- gsub("999", "N/A", test_3$sup_terr)
 # complemento las dos variables
 
 test_3$sup_terr <- ifelse(!is.na(test_3$sup_terr), test_3$sup_terr, test_3$sup_terr2)
 
 # sup_tot
 test_3$sup_tot <- gsub("-", "", test_3$sup_tot) 
 test_3$sup_tot <- gsub(" .*", "", test_3$sup_tot)
 # saco valores atípicos
 test_3$sup_tot <- gsub("99", "N/A", test_3$sup_tot) 
 

 #antiguedad
 test_3$antiguedad <- gsub(" a<f1>os", "", test_3$antiguedad)
 test_3$antiguedad <- gsub(" a昼㸱os", "", test_3$antiguedad)
 test_3$antiguedad <- gsub("M攼㸱s de ", "", test_3$antiguedad)
 test_3$antiguedad <- gsub("-1 a<f1>os", "nuevo", test_3$antiguedad)
 test_3$antiguedad <- gsub( "-", "", test_3$antiguedad)
 test_3$antiguedad <- gsub( "Max 0", "", test_3$antiguedad)
 test_3$antiguedad <- gsub( "Max 0", "", test_3$antiguedad)
 test_3$antiguedad <- gsub( "Max ", "", test_3$antiguedad)
 test_3$antiguedad <- gsub( "a<U+663C><U+3E31>o", "", test_3$antiguedad)
 test_3$antiguedad <- gsub( " a<U+663C><U+3E31>os", "", test_3$antiguedad)
 test_3$antiguedad <- gsub( "M<e1>s de 40 a<f1>os", "40+", test_3$antiguedad)
 test_3$antiguedad <- gsub( "20 a<U+663C><U+3E31>os", "20", test_3$antiguedad)
 test_3$antiguedad <- gsub( "30 a<U+663C><U+3E31>os", "30", test_3$antiguedad) 
#hay que seguir limpiando esta variable
 
#para ver los niveles de las variables factores
 library(dplyr)
  test_3 %>%
   summarise_each(funs(list(levels(.))))
 
 
 ### no funca esto...
#t4est_3 %>%
#  mutate_if(is.character, as.numeric) %>%
#    mutate_all(funs(as.numeric(as.factor(c("precio","lat","long","sup_constru","sup_terr", "sup_tot")))))

#guardo el archivo
  save(test_3, file = "muestra_var_meli_2.Rda")
#cargo el archivo
  load(file = "muestra_var_meli_2.Rda")

  test_4 <- test_3

num <- c("precio","lat","long","sup_constru","sup_terr", "sup_tot")
test_4[,num] <- lapply(test_4[,num], as.numeric)
#NAs introduced by coercion

str(test_4)   
summary(test_4)

#muchos N/A


# Filtrar un poco precio, sup y lat/lon

test_5 <- data.frame(subset(test_4,precio<1500000))
test_5 <- data.frame(subset(test_5,precio>1000))

#margen aprox de Montevideo
test_5 <- data.frame(subset(test_5,lat > -34.93 & lat < -34.59))
test_5 <- data.frame(subset(test_5,long > -56.65 & long < -55.87))

#dos subset para aptos y casas
test_5_aptos <-  data.frame(subset(test_5,tipo_inmueble=="Apartamentos"))
test_5_casas <-  data.frame(subset(test_5,tipo_inmueble=="Casas"))

#start time
test_5$start_time <- gsub(" .*", "", test_5$start_time)
test_5$start_time <- as.POSIXct(test_5$start_time)
# recordar que los NA refieren a bajadas de febrero marzo y abril


# ver histograma de los precios
library(ggplot2)
qplot(test_5$precio/1000,
      geom="histogram",
      binwidth=25,  
      main="Histograma de precios 2018", 
      xlab="Precio en miles")

p<- ggplot(data=test_5, aes(test_5$precio/1000)) + 
  geom_histogram(aes(y =..count..), 
                 col="red", 
                 fill="blue", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="Histograma de precios 2018 - MELI", x="Precio", y="Observaciones")
p



# tomo una muestra pequena para probar cosas simples
library("tibble")    
sample_meli <- filter(test_5, sup_constru>1, sup_constru<500, moneda == "USD")
summary(sample_meli)


# Mapas ----
# vamos a usar esta guía https://statisticaloddsandends.wordpress.com/2018/10/25/getting-started-stamen-maps-with-ggmap/

library(caret)
library(dplyr)
library(ggmap)

df <- sample_meli %>% group_by(barrio) %>%
  summarize(median_price = median(precio/sup_constru, na.rm = TRUE), quantity = n(),
            latitude = mean(lat, na.rm = TRUE), longitude = mean(long, na.rm = TRUE))
ggplot() + 
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, 
                                      col = median_price/1000, size = quantity)) +
  scale_color_distiller(palette = "YlOrRd", direction = 1)


ggplot(data = df, mapping = aes(x = longitude, y = latitude)) + 
  geom_point(aes(col = median_price, size = quantity)) +
  geom_text(aes(label = barrio), size = 4, nudge_y = 0.01) +
  scale_color_distiller(palette = "YlOrRd", direction = 1)


# en un mapa ... con ggmap
height <- max(df$latitude, na.rm = TRUE) - min(df$latitude, na.rm = TRUE)
width <- max(df$longitude, na.rm = TRUE) - min(df$longitude, na.rm = TRUE)
sac_borders <- c(bottom  = -35, 
                 top     = -34.5,
                 left    = -56.5,
                 right   = -55.8)

# sac_borders <- c(bottom  = min(df$latitude, na.rm = TRUE)  - 0.1 * height, 
#                 top     = max(df$latitude, na.rm = TRUE)  + 0.1 * height,
#                 left    = min(df$longitude, na.rm = TRUE) - 0.1 * width,
#                 right   = max(df$longitude, na.rm = TRUE) + 0.1 * width)

map <- get_stamenmap(sac_borders, zoom = 10, maptype = "toner-lite")

ggmap(map) +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, 
                                      col = median_price, size = quantity)) +
  scale_color_distiller(palette = "YlOrRd", direction = 1)




# ahora otro intento con leaflet y todos los puntos, similar al de transacciones
library(leaflet)
library(htmltools)
library(dplyr)
library(ggmap)

qpal <- colorQuantile("YlOrRd", sample_meli$precio/sample_meli$sup_constru, n = 9)

mapa_meli <- leaflet(sample_meli) %>% 
  addTiles() %>% 
  fitBounds(-56.4,-34.9,-55.9,-34.8) %>% 
  addCircleMarkers(stroke=FALSE, color=~qpal(sample_meli$precio/sample_meli$sup_constru),fillOpacity = 0.2, radius = runif(10, 2, 5),label = ~htmlEscape(lat)) %>% 
  addLegend("bottomright", pal = qpal, values = ~sample_meli$precio/sample_meli$sup_constru,title = "USDM2 en USD 2018 (MELI)",opacity = 1)
mapa_meli
