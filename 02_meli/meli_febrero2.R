# data cleaning
library(readr)
library(readxl)
library(dplyr)
library(ggmap)

setwd("C:/Users/Usuario/Documents/Housing-prices/02_meli")
# comenzamos con los datos de Febrero

# Mercadolibre febrero

meli_feb_orig <- read_csv("C:/Users/Usuario/Documents/Housing-prices/02_meli/venta_2018_02_23.csv")
str(meli_feb_orig)
dim(meli_feb_orig)
# 64061   161

# Tomamos solo las de Montevideo, en esta bajada está todo el país
meli_feb_1 <- subset(meli_feb_orig, state == "Montevideo")
meli_feb_1 <- data.frame(meli_feb_1)
head(meli_feb_1)
str(meli_feb_1, list.len = ncol(meli_feb_1))


# Sacamos una primer conjunto de variables que no nos interesarán 
# Básicamente refieren a alquileres o a terrenos/campos



meli_feb_1$TV.por.cable <- NULL
meli_feb_1$Desayuno.incluido <- NULL
meli_feb_1$Heladera.con.freezer <- NULL
meli_feb_1$Hidromasaje <- NULL
meli_feb_1$Lavavajillas <- NULL
meli_feb_1$Lavarropa.Secarropa <- NULL
meli_feb_1$Microondas <- NULL
meli_feb_1$Servicio.de.limpieza <- NULL  
meli_feb_1$Wifi <- NULL
meli_feb_1$Apto.para.familias.ni.f1.os <- NULL
meli_feb_1$Forestaci.f3.n <- NULL
meli_feb_1$Silos <- NULL
meli_feb_1$Bebederos <- NULL
meli_feb_1$Corrales <- NULL
meli_feb_1$Potreros <- NULL
meli_feb_1$Check.in <- NULL
meli_feb_1$Check.out <- NULL
meli_feb_1$Desde <- NULL
meli_feb_1$Desde..Fecha. <- NULL
meli_feb_1$Hasta <- NULL
meli_feb_1$Estad.ed.a.M.ed.nima <- NULL
meli_feb_1$Tipo.de.campo <- NULL
meli_feb_1$Apto.para.fumadores <- NULL
meli_feb_1$Hect.e1.reas.totales <- NULL
meli_feb_1$Distancia.al.asfalto..Km. <- NULL
meli_feb_1$Grupo.eletr.f3.geno <- NULL
meli_feb_1$Manga..ancho. <- NULL
meli_feb_1$Restaurante <- NULL
meli_feb_1$Linea.telef.f3.nica <- NULL
meli_feb_1$Tel.e9.fono <- NULL
meli_feb_1$Luz.a.motor <- NULL
meli_feb_1$Molinos <- NULL
meli_feb_1$Hasta..Fecha. <- NULL
meli_feb_1$N.fa.mero.de.camas <- NULL

head(meli_feb_1)
dim(meli_feb_1)
str(meli_feb_1, list.len = ncol(meli_feb_1))
# Quedamos con 127 variables, vamos a categorizar una a una y, de paso, modificar los nombres
# minúscula siempre, sin tildes con _1 o _2

meli_feb_1$id  <- as.character(meli_feb_1$id)
meli_feb_1$titulo <- as.character(meli_feb_1$title)
meli_feb_1$categ <- as.factor(meli_feb_1$category_id)
meli_feb_1$operacion <- as.factor(meli_feb_1$category_1)
meli_feb_1$tipo_inmueble <- as.factor(meli_feb_1$category_2)
meli_feb_1$precio <- as.numeric(meli_feb_1$price)
meli_feb_1$moneda <- as.factor(meli_feb_1$currency_id)
meli_feb_1$condicion <- as.factor(meli_feb_1$condition)
meli_feb_1$barrio <- as.factor(meli_feb_1$city)
meli_feb_1$departamento <- as.factor(meli_feb_1$state)
meli_feb_1$direccion <- as.character(meli_feb_1$address_line)
meli_feb_1$zip <- as.factor(meli_feb_1$zip_code)
meli_feb_1$lat <- as.numeric(meli_feb_1$latitude)
meli_feb_1$long <- as.numeric(meli_feb_1$longitude)
meli_feb_1$terraza <- as.factor(meli_feb_1$Terraza)
meli_feb_1$baulera <- as.factor(meli_feb_1$Baulera)
meli_feb_1$biblio <- as.factor(meli_feb_1$Biblioteca)
meli_feb_1$cocina <- as.factor(meli_feb_1$Cocina)
meli_feb_1$comedor <- as.factor(meli_feb_1$Comedor)
meli_feb_1$depserv <- as.factor(meli_feb_1$Dependencia.de.servicio)
meli_feb_1$dormsuite <- as.factor(meli_feb_1$Dormitorio.en.suite)
meli_feb_1$entrepiso <- as.factor(meli_feb_1$Entrepiso)
meli_feb_1$escritorio <- as.factor(meli_feb_1$Escritorio)
meli_feb_1$estar <- as.factor(meli_feb_1$Estar)
meli_feb_1$lavadero <- as.factor(meli_feb_1$Lavadero)
meli_feb_1$livingcomedor <- as.factor(meli_feb_1$Living.comedor)
meli_feb_1$living <- as.factor(meli_feb_1$Living)
meli_feb_1$patio <- as.factor(meli_feb_1$Patio)
meli_feb_1$playroom <- as.factor(meli_feb_1$Playroom)
meli_feb_1$toilette <- as.factor(meli_feb_1$Toilette)
meli_feb_1$vestidor <- as.factor(meli_feb_1$Vestidor)
meli_feb_1$aircond <- as.factor(meli_feb_1$Aire.acondicionado)
meli_feb_1$calefacc <- as.factor(meli_feb_1$Calefacci.f3.n)
meli_feb_1$tenis <- as.factor(meli_feb_1$Cancha.de.tenis)
meli_feb_1$asc_serv <- as.factor(meli_feb_1$Ascensor.de.servicio)
meli_feb_1$estacionam_cortesia <- as.factor(meli_feb_1$Estacionamiento.de.cortes.ed.a) # ver esta variable
meli_feb_1$gym <- as.factor(meli_feb_1$Gimnasio)
meli_feb_1$guarderia <- as.factor(meli_feb_1$Guarder.ed.a)
meli_feb_1$internet <- as.factor(meli_feb_1$Conexi.f3.n.a.internet)
meli_feb_1$jardin <- as.factor(meli_feb_1$Jard.ed.n)
meli_feb_1$kitchenette <- as.factor(meli_feb_1$Kitchenette)
meli_feb_1$lavadero_2 <- as.factor(meli_feb_1$Laundry)
meli_feb_1$losa_rad <- as.factor(meli_feb_1$Losa.radiante)
meli_feb_1$parrillero <- as.factor(meli_feb_1$Parrillero)
meli_feb_1$piscina <- as.factor(meli_feb_1$Piscina)
meli_feb_1$sala_reuniones <- as.factor(meli_feb_1$Sala.de.reuniones)
meli_feb_1$spa <- as.factor(meli_feb_1$Spa)
meli_feb_1$salon_comunal <- as.factor(meli_feb_1$Sal.f3.n.de.usos.m.fa.ltiples)
meli_feb_1$seguridad <- as.factor(meli_feb_1$Seguridad)
meli_feb_1$jacuzzi <- as.factor(meli_feb_1$Jacuzzi)
meli_feb_1$tipo_inmueble_2 <- as.factor(meli_feb_1$Inmueble)
meli_feb_1$operacion_2 <- as.factor(meli_feb_1$Operaci.f3.n)
meli_feb_1$amoblado <- as.factor(meli_feb_1$Amoblado)
meli_feb_1$banos <- as.factor(meli_feb_1$Ba.f1.os) # ver luego de pasar a numerico
meli_feb_1$ap_ppiso <- as.factor(meli_feb_1$Apartamentos.por.piso) # ver
meli_feb_1$dormitorios <- as.factor(meli_feb_1$Dormitorios)
meli_feb_1$tipo_edif <- as.factor(meli_feb_1$Tipo.de.edificaci.f3.n)
meli_feb_1$estado <- as.factor(meli_feb_1$Estado)
meli_feb_1$expensas <- as.factor(meli_feb_1$Expensas..)
meli_feb_1$luminosidad <- as.factor(meli_feb_1$Luminosidad)
meli_feb_1$sup_constru <- as.numeric(meli_feb_1$Superficie.construida..m.U.00B2..)
meli_feb_1$sup_terr <- as.numeric(meli_feb_1$Superficie.del.terreno..m.U.00B2..)
meli_feb_1$orientacion <- as.factor(meli_feb_1$Orientaci.f3.n)
meli_feb_1$antiguedad <- as.factor(meli_feb_1$Antig.fc.edad) #ver luego para pasar a numérico, en algunos casos está el ano de construcción, en otros simplemente la antiguedad
meli_feb_1$altillo <- as.factor(meli_feb_1$Altillo)
meli_feb_1$bodega <- as.factor(meli_feb_1$Bodega)
meli_feb_1$galeria <- as.factor(meli_feb_1$Galer.ed.a)
meli_feb_1$sotano <- as.factor(meli_feb_1$S.f3.tano)
meli_feb_1$alarma <- as.factor(meli_feb_1$Alarma)
meli_feb_1$futbol <- as.factor(meli_feb_1$Cancha.de.f.fa.tbol)
meli_feb_1$estufa_lena <- as.factor(meli_feb_1$Estufa.a.le.f1.a)
meli_feb_1$porton_electr <- as.factor(meli_feb_1$Port.f3.n.el.e9.ctrico)
meli_feb_1$garage <- as.factor(meli_feb_1$Garage)
meli_feb_1$tipo_ubicacion <- as.factor(meli_feb_1$Lugar) # si está sobre la calle, country, barrio privado
meli_feb_1$num_plantas <- as.factor(meli_feb_1$Plantas)
meli_feb_1$agua_corr <- as.factor(meli_feb_1$Agua.Corriente)
meli_feb_1$gas_caneria <- as.factor(meli_feb_1$Gas.de.ca.f1.er.ed.a)
meli_feb_1$luz_electr <- as.factor(meli_feb_1$Luz.el.e9.ctrica)
meli_feb_1$acceso <- as.factor(meli_feb_1$Acceso) # con asfalto, con rampa, etc.
meli_feb_1$disposicion <- as.factor(meli_feb_1$Disposici.f3.n) # para terrenos
meli_feb_1$horario_contacto <- as.factor(meli_feb_1$Horario.de.contacto)
meli_feb_1$apto_profesional <- as.factor(meli_feb_1$Apto.profesional)
meli_feb_1$condicion_2 <- as.factor(meli_feb_1$Condici.f3.n.del..ed.tem) # usado o nuevo
meli_feb_1$aircond_pamb <- as.factor(meli_feb_1$Aire.acondicionado.por.ambiente) # entiendo que significa que hay ac en cada ambiente?
meli_feb_1$calefacc_pamb <- as.factor(meli_feb_1$Calefacci.f3.n.por.ambiente)
meli_feb_1$estacionamiento <- as.factor(meli_feb_1$Estacionamiento)
meli_feb_1$mascotas <- as.factor(meli_feb_1$Permite.mascotas)
meli_feb_1$ambientes <- as.factor(meli_feb_1$Ambientes)
meli_feb_1$ascensores <- as.factor(meli_feb_1$Ascensor)
meli_feb_1$capacidad_personas <- as.factor(meli_feb_1$Capacidad.de.personas) # es más una var de alquileres
meli_feb_1$pisos_edificio <- as.factor(meli_feb_1$Cant.de.pisos.del.edificio)
meli_feb_1$tipo_inmueble_2 <- as.character(meli_feb_1$tipo_inmueble) # descripción extensa
meli_feb_1$casco <- as.factor(meli_feb_1$Casco)
meli_feb_1$casa_casero <- as.factor(meli_feb_1$Casa.del.casero)
meli_feb_1$antig_casco <- as.factor(meli_feb_1$Antig.fc.edad.del.casco) # ni idea qué es el casco, ver
meli_feb_1$estado_casco <- as.factor(meli_feb_1$Estado.del.casco)
meli_feb_1$galpones <- as.factor(meli_feb_1$Galpones)
meli_feb_1$tanque_agua <- as.factor(meli_feb_1$Tanque.de.agua)
meli_feb_1$desag_cloaca <- as.factor(meli_feb_1$Desag.fc.e.Cloacal)
meli_feb_1$pavimento <- as.factor(meli_feb_1$Pavimento)
meli_feb_1$alarma_2 <- as.factor(meli_feb_1$Alarma.de.seguridad)
meli_feb_1$deposito <- as.factor(meli_feb_1$Dep.f3.sito)
meli_feb_1$sist_incendio <- as.factor(meli_feb_1$Sistema.contra.incendio)
meli_feb_1$vestuario <- as.factor(meli_feb_1$Vestuario) # ?? hay algunas canchas de fut o locales en venta que tienen vestuarios
meli_feb_1$localizacion_local <- as.factor(meli_feb_1$Localizaci.f3.n) # para locales, etc. 
meli_feb_1$balcon <- as.factor(meli_feb_1$Balc.f3.n) # en esta base no hay obs, estarán como terraza?
meli_feb_1$comedor_diario <- as.factor(meli_feb_1$Comedor.diario) # ni idea a q se refiere
meli_feb_1$bar <- as.factor(meli_feb_1$Bar)
meli_feb_1$caja_seg <- as.factor(meli_feb_1$Caja.de.seguridad)
meli_feb_1$golf <- as.factor(meli_feb_1$Cancha.de.golf)
meli_feb_1$paddle <- as.factor(meli_feb_1$Cancha.de.paddle)
meli_feb_1$pileta <- as.factor(meli_feb_1$Pileta) # diferente de piscina?
meli_feb_1$sauna <- as.factor(meli_feb_1$Sauna)
meli_feb_1$solarium <- as.factor(meli_feb_1$Solarium)
meli_feb_1$salon_comunal_2 <- as.factor(meli_feb_1$Sal.f3.n.de.uso.com.fa.n)
meli_feb_1$quincho <- as.factor(meli_feb_1$Quincho)
meli_feb_1$basquet <- as.factor(meli_feb_1$Cancha.de.basquet)
meli_feb_1$volley <- as.factor(meli_feb_1$Cancha.de.voley)
meli_feb_1$hogar_lena <- as.factor(meli_feb_1$Hogar.a.le.f1.a) #diferencia con estufa a lena??
meli_feb_1$salamandra <- as.factor(meli_feb_1$Salamandra)
meli_feb_1$vigilancia <- as.factor(meli_feb_1$Vigilancia)
meli_feb_1$sup_contru_2 <- as.factor(meli_feb_1$Superficie.construida...m.U.00B2..)
meli_feb_1$amb_casco <- as.factor(meli_feb_1$Ambientes.del.casco)
meli_feb_1$recepcion <- as.factor(meli_feb_1$Recepci.f3.n)
meli_feb_1$cobertura_techo <- as.factor(meli_feb_1$Tipo.de.cobertura)
meli_feb_1$tipo_cochera <- as.factor(meli_feb_1$Tipo.de.cochera)




# me quedo con las que acabo de crear, todas tienen nuevo nombre, excepto el id
str(meli_feb_1, list.len = ncol(meli_feb_1))
meli_feb_2 <- meli_feb_1[c(-2:-127)]
head(meli_feb_2)
dim(meli_feb_2)

# Me quedo con 125 variables

#genero variables para identificar momento de bajada y fuente, en este caso Mercadolibre
meli_feb_2$mes <- "febrero"
meli_feb_2$dia <- 23
meli_feb_2$ano <- 2018
meli_feb_2$fuente <- "meli"

# defino data frame 
meli_feb_2 <- as.data.frame(meli_feb_2)
str(meli_feb_2, list.len=ncol(meli_feb_2))
dim(meli_feb_2)
# 26512 obs 129 variables

# guardo en Rdata - base ordenada 1 de datos MELI febrero
save(meli_feb_2,file="2018_02_20_meli_v1.Rda")

# para traerlo
setwd("C:/Users/Usuario/Documents/Housing-prices/02_meli")
load("meli febrero data.RData")

# falta ver el tema de duplicados y sacar malos datos

-------

# Prueba para mapear los datos de Meli
  
# Algunas obs: no todas las publicaciones tienen lat y long. Las que tienen, pueden ser erróneas.
map <- get_map("Montevideo", zoom = 12, maptype = 'hybrid', language="Spanish")


# selecciono un rango de precios central para poner en el mapa
meli_feb_map <- subset.data.frame(meli_feb_2, precio > 5000 & precio < 1000000)
# Para marcar casas, aptos y  otros
apto_casa <- ifelse(meli_feb_map$tipo_inmueble=="Apartamentos" , "Apartamentos", 
                    ifelse(meli_feb_map$tipo_inmueble=="Casas", "Casas", "Otros/Sin clasificar"))

# Dibujo el mapa con puntos de diferentes colores para aptos, casas y otros
meli_feb_mapa <- ggmap(map) +
  geom_point(data=meli_feb_map, aes(x = long, y = lat, color=apto_casa) , size=0.6 , alpha=0.5)+
  scale_color_manual(breaks=c("Apartamentos", "Casas", "Otros/Sin clasificar"), values=c("dodgerblue","firebrick1", "gray20"))
meli_feb_mapa 
save(meli_feb_mapa, file = "meli_feb_mapa2.Rdata")
load(file = "meli_feb_mapa.Rdata")

# 4404 obs no tienen lat y long y no se grafican
  # hay que ver cómo hacemos con estos datos, tenemos barrio, podrían imputarse en algún lugar
  # paper de location, location, location - la ubicación exacta es LA variable para definir el precio


# intento de imputar location a algunas de las 4404 obs ###

meli_feb_sinlong <- subset(meli_feb_2, is.na(meli_feb_2$long)) # 3478 NA en long

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(meli_feb_sinlong))
{
  # Print("Working...")
  result <- geocode(meli_feb_sinlong$direccion[i], output = "latlona", source = "google")
  meli_feb_sinlong$lon[i] <- as.numeric(result[1])
  meli_feb_sinlong$lat[i] <- as.numeric(result[2])
  meli_feb_sinlong$geoAddress[i] <- as.character(result[3])
}



# api code pass google map: AIzaSyD7bVTEpIdz45zpj2cAHA5QzIZX7r4hTVQ