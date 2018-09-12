# data cleaning
library(readr)
library(readxl)
library(dplyr)
library(ggmap)
library(tidyverse)
library(tibble)

setwd("C:/Users/Usuario/Documents/Housing-prices-tesina/04_meli")
# comenzamos con los datos de Febrero

# Mercadolibre abril

meli_abril_orig <- read_csv("2018_04_23_Mercadolibre.csv")
str(meli_abril_orig)
dim(meli_abril_orig)
# 73289    225

# Tomamos solo las de Montevideo, en esta bajada está todo el país
meli_abril_1 <- subset(meli_abril_orig, state == "Montevideo")
meli_abril_1 <- data.frame(meli_abril_1)
head(meli_abril_1)
str(meli_abril_1, list.len = ncol(meli_abril_1))
dim(meli_abril_1)
# 29684   225

# Sacamos una primer conjunto de variables que no nos interesarán 
# Básicamente refieren a alquileres o a terrenos/campos

meli_abril_1$TV.por.cable <- NULL
meli_abril_1$Desayuno.incluido <- NULL
meli_abril_1$Heladera.con.freezer <- NULL
meli_abril_1$Hidromasaje <- NULL
meli_abril_1$Lavavajillas <- NULL
meli_abril_1$Lavarropa.Secarropa <- NULL
meli_abril_1$Microondas <- NULL
meli_abril_1$Servicio.de.limpieza <- NULL  
meli_abril_1$Wifi <- NULL
meli_abril_1$Apto.para.familias.ni.f1.os <- NULL
meli_abril_1$Forestaci.f3.n <- NULL
meli_abril_1$Silos <- NULL
meli_abril_1$Bebederos <- NULL
meli_abril_1$Corrales <- NULL
meli_abril_1$Potreros <- NULL
meli_abril_1$Check.in <- NULL
meli_abril_1$Check.out <- NULL
meli_abril_1$Desde <- NULL
meli_abril_1$Desde..Fecha. <- NULL
meli_abril_1$Hasta <- NULL
meli_abril_1$Estad.ed.a.M.ed.nima <- NULL
meli_abril_1$Tipo.de.campo <- NULL
meli_abril_1$Apto.para.fumadores <- NULL
meli_abril_1$Hect.e1.reas.totales <- NULL
meli_abril_1$Distancia.al.asfalto..Km. <- NULL
meli_abril_1$Grupo.eletr.f3.geno <- NULL
meli_abril_1$Manga..ancho. <- NULL
meli_abril_1$Restaurante <- NULL
meli_abril_1$Linea.telef.f3.nica <- NULL
meli_abril_1$Tel.e9.fono <- NULL
meli_abril_1$Luz.a.motor <- NULL
meli_abril_1$Molinos <- NULL
meli_abril_1$Hasta..Fecha. <- NULL
meli_abril_1$N.fa.mero.de.camas <- NULL
meli_abril_1$Tipo.de.habitaci.f3.n <- NULL
meli_abril_1$Estacionamiento.de.cortes.ed.a <- NULL
meli_abril_1$Estacionamiento <- NULL


# aparecieron más variables en esta bajada, vamos a sacar alguna más
meli_abril_1$Horario.check.in <- NULL
meli_abril_1$Horario.check.out<- NULL
meli_abril_1$Estad.ed.a.m.ed.nima..noches. <- NULL
meli_abril_1$L.ed.nea.telef.f3.nica <- NULL
meli_abril_1$Business.center<- NULL
meli_abril_1$Cisterna<- NULL
meli_abril_1$Grupo.electr.f3.geno  <- NULL
meli_abril_1$Solo.familias <- NULL  
meli_abril_1$Apto.para.familias.con.ni.f1.os <- NULL
meli_abril_1$Vajilla <- NULL
meli_abril_1$Hu.e9.spedes <- NULL
meli_abril_1$X.c1.rea.de.juegos.infantiles   <- NULL
meli_abril_1$Caballeriza <- NULL
meli_abril_1$No <- NULL
meli_abril_1$Valet.parking   <- NULL
meli_abril_1$Valor.del.IPTU  <- NULL

meli_abril_1$Servicio.de.desayuno <- NULL
meli_abril_1$X.c1.rea.de.cine <- NULL
meli_abril_1$Galer.ed.a <- NULL
meli_abril_1$Plataformas.para.trailers <- NULL
meli_abril_1$TV <- NULL

meli_abril_1$Lavarropa <- NULL
meli_abril_1$Camas<- NULL
meli_abril_1$Estacionamiento.para.visitantes <- NULL
meli_abril_1$Sal.f3.n.de.fiestas <- NULL
meli_abril_1$Sitio.de.origen <- NULL
meli_abril_1$Disposici.f3.n.del.lote  <- NULL
meli_abril_1$B.e1.scula <- NULL
meli_abril_1$Probador <- NULL
meli_abril_1$Rampa.para.silla.de.ruedas <- NULL
meli_abril_1$Distancia.al.asfalto <- NULL
meli_abril_1$X.c1.rea.de.comedor  <- NULL
meli_abril_1$X.c1.reas.verdes <- NULL
meli_abril_1$Ba.f1.os.por.piso <- NULL
meli_abril_1$N.fa.mero.de.oficinas <- NULL
meli_abril_1$Oficinas.por.piso <- NULL
meli_abril_1$Superficie.cubierta.del.casco <- NULL
meli_abril_1$Tipo.de.bodega <- NULL
meli_abril_1$Heladera <- NULL

head(meli_abril_1)
dim(meli_abril_1)
# 29684    154

str(meli_abril_1, list.len = ncol(meli_abril_1))

# Quedamos con 154 variables
# vamos a categorizar una a una y, de paso, modificar los nombres
# minúscula siempre, sin tildes con _1 o _2

meli_abril_1<-rename(meli_abril_1, 
          titulo=title, categ=category_id, operacion=category_1, tipo_inmueble=category_2, 
          precio=price, moneda=currency_id, condicion=condition, barrio=city, departamento=state, 
          direccion= address_line, zip=zip_code, lat=latitude, long=longitude, 
condicion_2=Condici.f3.n.del..ed.tem, horario_contacto=Horario.de.contacto, sup_cub=Superficie.cubierta,
operacion_2=Operaci.f3.n, antiguedad=Antig.fc.edad,tipo_inmueble_2=Inmueble, sup_tot = Superficie.total,
dormsuite=Dormitorio.en.suite, jardin=Jard.ed.n, placard = Placards, parrillero=Parrillero, 
bano_soc = Ba.f1.o.social, mascotas=Se.admiten.mascotas, aircond=Aire.acondicionado, basquet=Cancha.de.b.e1.squetbol,
calefacc=Calefacci.f3.n, jacuzzi= Jacuzzi, paddle=Cancha.de.paddle ,piscina=Piscina,tenis=Cancha.de.tenis, dormitorios=Dormitorios, pisos=Pisos,
banos=Ba.f1.os, tipo_casa=Tipo.de.casa, cocheras=Cocheras, comedor=Comedor, internet=Acceso.a.internet,
cocina=Cocina, lavadero=Lavadero, living=Living, dorm_serv=Dormitorio.de.servicio, patio=Patio,
playro=Playroom, vestidor= Vestidor, estudio=Estudio, terraza=Terraza, gym=Gimnasio, ascensores=Ascensor,
salon_comunal=Sal.f3.n.de.usos.m.fa.ltiples, seguridad= Seguridad, recepcion=Recepci.f3.n,sala_reuniones=Sala.de.reuniones,
agua_corr=Agua.Corriente, bodega=Bodegas, sist_incendio=Sistema.contra.incendio, 
porton_autom=Port.f3.n.autom.e1.tico, chimenea=Chimenea, ap_ppiso=Apartamentos.por.piso,
orientacion=Orientaci.f3.n, expensas=Gastos.comunes, balcon=Balc.f3.n, roof_garden=Roof.garden,
amoblado=Amoblado,  gas_natural=Gas.natural, comercial=Uso.comercial, altillo=Altillo, alarma=Alarma,
luz_electr=Luz.el.e9.ctrica, acceso=Acceso, apto_credito=Apto.cr.e9.dito, disposicion=Disposici.f3.n,
ambientes=Ambientes, desayunador=Desayunador, caldera=Caldera, piso_unid=Piso..unidad., 
sist_ventilacion=Sistema.de.ventilaci.f3.n, casco=Casco, galpones=Galp.f3.n, tanque_agua=Tanque.de.agua,
forma_terreno=Forma.del.terreno, desag_cloaca=Cloaca, metros_frente=Metros.de.frente, metros_fondo=Metros.de.fondo,
futbol=Cancha.de.f.fa.tbol, tipo_cochera=Tipo.de.cochera, baulera=Baulera, biblio=Biblioteca, bodega2=Bodega,
depserv=Dependencia.de.servicio, entrepiso=Entrepiso,escritorio=Escritorio, estar=Estar,  
livingcomedor=Living.comedor, sotano=S.f3.tano, toilette= Toilette, estufa_lena= Estufa.a.le.f1.a,
internet2=Conexi.f3.n.a.internet, porton_electr=Port.f3.n.el.e9.ctrico, tipo_edif=Tipo.de.edificaci.f3.n,
estado=Estado, garage=Garage, tipo_ubicacion=Lugar, luminosidad=Luminosidad, 
sup_constru=Superficie.construida..m.U.00B2..,  sup_terr=Superficie.del.terreno..m.U.00B2.., num_plantas=Plantas,
mascotas2=Permite.mascotas, capacidad_personas=Capacidad.de.personas, asc_serv=Ascensor.de.servicio, 
guarderia=Guarder.ed.a, kitchenette=Kitchenette, lavadero_2=Laundry, losa_rad=Losa.radiante, spa=Spa,
apto_profesional=Apto.profesional, pisos_edificio=Cant.de.pisos.del.edificio, expensas2=Expensas.., 
sup_balcon=Superficie.de.balc.f3.n, antig_casco=Antig.fc.edad.del.casco, mas40=M.e1.s.de.40.a.f1.os,
acceso_cochera=Acceso.de.cochera, tipo_apto=Tipo.de.departamento,
cobertura_techo=Tipo.de.cobertura, alarma_2=Alarma.de.seguridad,deposito=Dep.f3.sito,
gas_caneria=Gas.de.ca.f1.er.ed.a, vestuario=Vestuario,localizacion_local=Localizaci.f3.n, 
lobby=Lobby, amb_casco=Ambientes.del.casco, operacion_3=Subtipo.de.operaci.f3.n, 
tipo_prop=Tipo.de.propiedad, aircond_pamb=Aire.acondicionado.por.ambiente, calefacc_pamb=Calefacci.f3.n.por.ambiente,
altura=Altura, casa_casero=Casa.del.casero, desag_cloaca2=Desag.fc.e.Cloacal, pavimento=Pavimento, 
tipo_inmueble_3=Tipo.de.inmueble, sup_contru_2=Superficie.construida...m.U.00B2.., max30=Max.30.a.f1.os , 
max5=Max.05.a.f1.os,aestrenar=A.estrenar, estado_casco=Estado.del.casco, soporte_piso=Soporte.de.piso)

# se suman estas variables: sup_tot, bano_soc, pisos, tipo_casa,cocheras,dorm_serv, estudio, porton_autom, chimenea,
# roof_garden, Gas.natural, comercial, apto_credito, desayunador, caldera, piso_unid, forma_terreno
# metros_frente, sup_balcon, mas40, lobby, altura, max30.

# NO SÉ SI ESTÁ BIEN ESTO, principalmente fijar variable como factores
str(meli_abril_1, list.len = ncol(meli_abril_1))
# variables numeric
num <- c("precio", "zip", "lat", "long", "sup_cub", "antiguedad", "sup_tot", "dormitorios", "banos", "ascensores",
         "ap_ppiso", "expensas", "ambientes", "metros_frente", "metros_fondo", "sup_constru", "sup_terr",	 
         "num_plantas", "capacidad_personas", "pisos_edificio",	 "expensas2",	"sup_balcon",	 "antig_casco", 
         "amb_casco", "aircond_pamb",	 "calefacc_pamb", "altura", "sup_contru_2") 
meli_abril_1[,num] <- data.frame(apply(meli_abril_1[num], as.factor))
as.numeric(meli_abril_1[num])

# variables chr
chr <- c("titulo", "direccion")
meli_abril_1[chr] <- lapply(meli_abril_1[chr], character)
as.character(meli_abril_1[chr])
# variables logical
logic <- c( "dormsuite",	 "jardin",	 "placard",
"parrillero", "bano_soc", 	 "mascotas",	 "aircond",	 "basquet", "calefacc",	 "jacuzzi",	 "paddle",
"piscina", "tenis", "cocheras",	 "comedor",	 "internet", "cocina",	 "lavadero",	 "living",	 "dorm_serv",
"patio",	"playro",	 "vestidor",	 "estudio",	 "terraza",	 "gym",	 	"salon_comunal",	 "seguridad",
"recepcion",	"sala_reuniones",	"agua_corr",	 "bodega",	 "sist_incendio", "porton_autom", "chimenea",
"balcon",	 "roof_garden", "amoblado", "gas_natural",	 "comercial",	 "altillo",	 "alarma", "luz_electr",
"acceso",	 "apto_credito", "desayunador", "caldera", "sist_ventilacion",	 "casco",	 "galpones","tanque_agua", 
"desag_cloaca", "futbol", "baulera",	 "biblio",	 "bodega2",	"depserv",	 "entrepiso",	"escritorio",	 "estar", "livingcomedor",	 "sotano",	 "toilette",	 "estufa_lena", "internet2",
"porton_electr", "garage", "mascotas2",	 "asc_serv", "guarderia",	 "kitchenette",	 "lavadero_2",	 "losa_rad",	 "spa",
"apto_profesional", "mas40", "alarma_2","deposito", "gas_caneria",	 "vestuario", "lobby", "casa_casero",	 "desag_cloaca2",
"pavimento","max30","max5",	"aestrenar")
# variables factor
factor <- c("categ", "operacion",	 "tipo_inmueble","moneda", "condicion","barrio", "departamento",	 	
          "condicion_2", "horario_contacto", "operacion_2",	"tipo_inmueble_2", "pisos", 
          "tipo_casa","orientacion","disposicion","piso_unid",  "forma_terreno", "tipo_cochera",	 			
          "tipo_edif", "estado", "tipo_ubicacion",	 "luminosidad",	 "tipo_apto",	"acceso_cochera",
          "cobertura_techo", "localizacion_local", "operacion_3", "tipo_prop", "tipo_inmueble_3", 
          "estado_casco" , "soporte_piso")
sapply(meli_abril_1, class)


str(meli_abril_1, list.len = ncol(meli_abril_1))
summary(meli_abril_1)
head(meli_abril_1)
dim(meli_abril_1)
# Me quedo con 126 variables

#genero variables para identificar momento de bajada y fuente, en este caso Mercadolibre
meli_abril_1$mes <- "marzo"
meli_abril_1$dia <- 24
meli_abril_1$ano <- 2018
meli_abril_1$fuente <- "meli"

# defino data frame "final"
meli_2018_03 <- as.data.frame(meli_abril_1)
str(meli_2018_03, list.len=ncol(meli_2018_03))
dim(meli_2018_03)
# 33277 obs 130 variables (hay dos vars mas que en feb, son las de fecha)
 
# guardo en Rdata - base ordenada 1 de datos MELI febrero
save(meli_2018_03,file="2018_03_24_meli.Rda")

# para traerlo
load("2018_03_24_meli.Rda")

# falta ver el tema de duplicados y sacar malos datos

-------

# Prueba para mapear los datos de Meli
  
# Algunas obs: no todas las publicaciones tienen lat y long. Las que tienen, pueden ser erróneas.
library(ggmap)
map <- get_map("Montevideo", zoom = 12)


# selecciono un rango de precios central para poner en el mapa
meli_mar_map <- subset.data.frame(meli_2018_03, precio > 2000 & precio < 1800000)
# Para marcar casas, aptos y  otros
apto_casa <- ifelse(meli_mar_map$tipo_inmueble=="Apartamentos" , "Apartamentos", 
                    ifelse(meli_mar_map$tipo_inmueble=="Casas", "Casas", "Otros/Sin clasificar"))



# Dibujo el mapa con puntos de diferentes colores para aptos, casas y otros
meli_mar_map <- ggmap(map) +
  geom_point(data=meli_mar_map, aes(x = long, y = lat, color=apto_casa) , size=0.5 , alpha=0.3)+
  scale_color_manual(breaks=c("Apartamentos", "Casas", "Otros/Sin clasificar"), values=c("dodgerblue","firebrick1", "gray20"))
meli_mar_map 
save(meli_mar_map, file = "meli_marzo_mapa.Rdata")

load(file = "meli_marzo_mapa.Rdata")

# 5850 obs no tienen lat y long y no se grafican
  # hay que ver cómo hacemos con estos datos, tenemos barrio, podrían imputarse en algún lugar
  # paper de location, location, location - la ubicación exacta es LA variable para definir el precio


# intento de imputar location a algunas de las 4404 obs ###

meli_feb_sinlong <- subset(meli_2018_02, is.na(meli_2018_02$long)) # 3478 NA en long

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
}

# ojo que hay un límite de queries que es 2500 aparenetemente ... hay que averiguar
# api code pass google map: AIzaSyD7bVTEpIdz45zpj2cAHA5QzIZX7r4hTVQ

#nuevo intento

meli_feb_sinlong2 <- select(meli_feb_sinlong, var1="direccion", var2= "id")
as.character(meli_feb_sinlong2$var1)
Encoding(meli_feb_sinlong2$var1) <- "UTF-8"
for(i in 1:nrow(meli_feb_sinlong2))
{
  #Print("Working...")
  result <- geocode(meli_feb_sinlong2$var1[i], output = "latlona", source = "google")
  meli_feb_sinlong2$lon[i] <- as.numeric(result[1])
  meli_feb_sinlong2$lat[i] <- as.numeric(result[2])
}
