# data cleaning
library(readr)
library(readxl)
library(dplyr)
library(ggmap)
library(tidyverse)

setwd("C:/Users/Usuario/Documents/Housing-prices-tesina/03_meli")
# comenzamos con los datos de Febrero

# Mercadolibre febrero

meli_marzo_orig <- read_csv("2018_03_24_Mercadolibre.csv")
str(meli_marzo_orig)
dim(meli_marzo_orig)
# 84556   161

# Tomamos solo las de Montevideo, en esta bajada está todo el país
meli_marzo_1 <- subset(meli_marzo_orig, state == "Montevideo")
meli_marzo_1 <- data.frame(meli_marzo_1)
head(meli_marzo_1)
str(meli_marzo_1, list.len = ncol(meli_marzo_1))


# Sacamos una primer conjunto de variables que no nos interesarán 
# Básicamente refieren a alquileres o a terrenos/campos

meli_marzo_1$TV.por.cable <- NULL
meli_marzo_1$Desayuno.incluido <- NULL
meli_marzo_1$Heladera.con.freezer <- NULL
meli_marzo_1$Hidromasaje <- NULL
meli_marzo_1$Lavavajillas <- NULL
meli_marzo_1$Lavarropa.Secarropa <- NULL
meli_marzo_1$Microondas <- NULL
meli_marzo_1$Servicio.de.limpieza <- NULL  
meli_marzo_1$Wifi <- NULL
meli_marzo_1$Apto.para.familias.ni.f1.os <- NULL
meli_marzo_1$Forestaci.f3.n <- NULL
meli_marzo_1$Silos <- NULL
meli_marzo_1$Bebederos <- NULL
meli_marzo_1$Corrales <- NULL
meli_marzo_1$Potreros <- NULL
meli_marzo_1$Check.in <- NULL
meli_marzo_1$Check.out <- NULL
meli_marzo_1$Desde <- NULL
meli_marzo_1$Desde..Fecha. <- NULL
meli_marzo_1$Hasta <- NULL
meli_marzo_1$Estad.ed.a.M.ed.nima <- NULL
meli_marzo_1$Tipo.de.campo <- NULL
meli_marzo_1$Apto.para.fumadores <- NULL
meli_marzo_1$Hect.e1.reas.totales <- NULL
meli_marzo_1$Distancia.al.asfalto..Km. <- NULL
meli_marzo_1$Grupo.eletr.f3.geno <- NULL
meli_marzo_1$Manga..ancho. <- NULL
meli_marzo_1$Restaurante <- NULL
meli_marzo_1$Linea.telef.f3.nica <- NULL
meli_marzo_1$Tel.e9.fono <- NULL
meli_marzo_1$Luz.a.motor <- NULL
meli_marzo_1$Molinos <- NULL
meli_marzo_1$Hasta..Fecha. <- NULL
meli_marzo_1$N.fa.mero.de.camas <- NULL
meli_marzo_1$Tipo.de.habitaci.f3.n <- NULL
meli_marzo_1$Estacionamiento.de.cortes.ed.a<-NULL
meli_marzo_1$Estacionamiento<- NULL
  
head(meli_marzo_1)
dim(meli_marzo_1)
str(meli_marzo_1, list.len = ncol(meli_marzo_1))

# Quedamos con 126 variables (dos más que en febrero porque tenemos la de fechas de creación y finalización)
# vamos a categorizar una a una y, de paso, modificar los nombres
# minúscula siempre, sin tildes con _1 o _2

meli_marzo_1<-rename(meli_marzo_1, 
          titulo=title, categ=category_id,operacion=category_1, tipo_inmueble=category_2, precio=price,
          moneda=currency_id,condicion=condition,barrio=city,departamento=state, direccion= address_line,
          zip=zip_code, lat=latitude, long=longitude, terraza=Terraza, baulera=Baulera, biblio=Biblioteca, 
          cocina=Cocina, comedor=Comedor, depserv=Dependencia.de.servicio, dormsuite=Dormitorio.en.suite,
          entrepiso=Entrepiso,escritorio=Escritorio,estar=Estar, lavadero=Lavadero, livingcomedor=Living.comedor,
          living=Living,patio=Patio, playro=Playroom, toilette= Toilette,vestidor= Vestidor,aircond=Aire.acondicionado,
          calefacc=Calefacci.f3.n,tenis=Cancha.de.tenis, asc_serv=Ascensor.de.servicio, gym=Gimnasio,
          guarderia=Guarder.ed.a,internet=Conexi.f3.n.a.internet,jardin=Jard.ed.n, kitchenette=Kitchenette,
          lavadero_2=Laundry,losa_rad=Losa.radiante, parrillero=Parrillero, piscina=Piscina, sala_reuniones=Sala.de.reuniones,
          spa=Spa,salon_comunal=Sal.f3.n.de.usos.m.fa.ltiples,seguridad= Seguridad,jacuzzi= Jacuzzi,tipo_inmueble_2=Inmueble,
          operacion_2=Operaci.f3.n, amoblado=Amoblado, banos=Ba.f1.os, ap_ppiso=Apartamentos.por.piso,dormitorios=Dormitorios,
          tipo_edif=Tipo.de.edificaci.f3.n,estado=Estado,expensas=Expensas..,luminosidad=Luminosidad,sup_constru=Superficie.construida..m.U.00B2..,
          sup_terr=Superficie.del.terreno..m.U.00B2..,orientacion=Orientaci.f3.n, antiguedad=Antig.fc.edad,altillo=Altillo,
          bodega=Bodega,galeria=Galer.ed.a,sotano=S.f3.tano,alarma=Alarma, futbol=Cancha.de.f.fa.tbol,estufa_lena= Estufa.a.le.f1.a,
          porton_electr=Port.f3.n.el.e9.ctrico, garage=Garage,tipo_ubicacion=Lugar,num_plantas=Plantas,agua_corr=Agua.Corriente,
          gas_caneria=Gas.de.ca.f1.er.ed.a, luz_electr=Luz.el.e9.ctrica,acceso=Acceso,disposicion=Disposici.f3.n,
          horario_contacto=Horario.de.contacto,apto_profesional=Apto.profesional,condicion_2=Condici.f3.n.del..ed.tem,
          aircond_pamb=Aire.acondicionado.por.ambiente, calefacc_pamb=Calefacci.f3.n.por.ambiente,mascotas=Permite.mascotas,
          ambientes=Ambientes,ascensores=Ascensor, capacidad_personas=Capacidad.de.personas,pisos_edificio=Cant.de.pisos.del.edificio,
          tipo_inmueble_3=Tipo.de.inmueble,casco=Casco, casa_casero=Casa.del.casero, antig_casco=Antig.fc.edad.del.casco, 
          estado_casco=Estado.del.casco, galpones=Galpones,tanque_agua=Tanque.de.agua,desag_cloaca=Desag.fc.e.Cloacal, 
          pavimento=Pavimento,alarma_2=Alarma.de.seguridad,deposito=Dep.f3.sito,sist_incendio=Sistema.contra.incendio,
          vestuario=Vestuario,localizacion_local=Localizaci.f3.n, balcon=Balc.f3.n, comedor_diario=Comedor.diario,
          bar=Bar,caja_seg=Caja.de.seguridad,golf=Cancha.de.golf,paddle=Cancha.de.paddle,pileta=Pileta,sauna=Sauna, solarium=Solarium,
          salon_comunal_2=Sal.f3.n.de.uso.com.fa.n,quincho= Quincho,basquet=Cancha.de.basquet,volley=Cancha.de.voley,
          hogar_lena=Hogar.a.le.f1.a,salamandra=Salamandra,vigilancia=Vigilancia,sup_contru_2=Superficie.construida...m.U.00B2..,
          amb_casco=Ambientes.del.casco,recepcion=Recepci.f3.n,cobertura_techo=Tipo.de.cobertura,tipo_cochera=Tipo.de.cochera)

str(meli_marzo_1, list.len = ncol(meli_marzo_1))


meli_marzo_1$titulo<-as.character(meli_marzo_1$titulo)
meli_marzo_1$categ<-as.factor(meli_marzo_1$categ)
meli_marzo_1$operacion<-as.factor(meli_marzo_1$operacion)
meli_marzo_1$tipo_inmueble<-as.factor(meli_marzo_1$tipo_inmueble)
meli_marzo_1$precio<-as.numeric(meli_marzo_1$precio)
meli_marzo_1$moneda<-as.factor(meli_marzo_1$moneda)
meli_marzo_1$condicion<-as.factor(meli_marzo_1$condicion)
meli_marzo_1$barrio<-as.factor(meli_marzo_1$barrio) # 68 LEVELS ... esto después se podrá agrupar
meli_marzo_1$departamento<-as.factor(meli_marzo_1$departamento) #SOLO Mdeo.
meli_marzo_1$direccion<-as.character(meli_marzo_1$direccion)
meli_marzo_1$zip<-as.factor(meli_marzo_1$zip)
meli_marzo_1$lat<-as.numeric(meli_marzo_1$lat)
meli_marzo_1$long<-as.numeric(meli_marzo_1$long)
meli_marzo_1$terraza<-as.factor(meli_marzo_1$terraza) #13 levels
meli_marzo_1$baulera<-as.factor(meli_marzo_1$baulera) # error ... ver
meli_marzo_1$biblio<-as.factor(meli_marzo_1$biblio)
meli_marzo_1$cocina<-as.factor(meli_marzo_1$cocina)
meli_marzo_1$comedor<-as.factor(meli_marzo_1$comedor) 
meli_marzo_1$depserv<-as.factor(meli_marzo_1$depserv)
meli_marzo_1$dormsuite<-as.factor(meli_marzo_1$dormsuite) 
meli_marzo_1$entrepiso<-as.factor(meli_marzo_1$entrepiso)
meli_marzo_1$escritorio<-as.factor(meli_marzo_1$escritorio)
meli_marzo_1$estar<-as.factor(meli_marzo_1$estar)
meli_marzo_1$lavadero<-as.factor(meli_marzo_1$lavadero)
meli_marzo_1$livingcomedor<-as.factor(meli_marzo_1$livingcomedor)
meli_marzo_1$living<-as.factor(meli_marzo_1$living)
meli_marzo_1$patio<-as.factor(meli_marzo_1$patio)
meli_marzo_1$playroom<-as.character(meli_marzo_1$playroom)# no tiene nada
meli_marzo_1$toilette<-as.factor(meli_marzo_1$toilette)
meli_marzo_1$vestidor<-as.factor(meli_marzo_1$vestidor)
meli_marzo_1$aircond<-as.factor(meli_marzo_1$aircond)
meli_marzo_1$calefacc<-as.factor(meli_marzo_1$calefacc)
meli_marzo_1$tenis<-as.factor(meli_marzo_1$tenis)
meli_marzo_1$asc_serv<-as.factor(meli_marzo_1$asc_serv)
meli_marzo_1$gym<-as.factor(meli_marzo_1$gym)
meli_marzo_1$guarderia<-as.factor(meli_marzo_1$guarderia)
meli_marzo_1$internet<-as.factor(meli_marzo_1$internet)
meli_marzo_1$jardin<-as.factor(meli_marzo_1$jardin)
meli_marzo_1$kitchenette<-as.factor(meli_marzo_1$kitchenette)
meli_marzo_1$lavadero_2<-as.factor(meli_marzo_1$lavadero_2)
meli_marzo_1$losa_rad<-as.factor(meli_marzo_1$losa_rad)
meli_marzo_1$parrillero<-as.factor(meli_marzo_1$parrillero)
meli_marzo_1$piscina<-as.factor(meli_marzo_1$piscina)
meli_marzo_1$sala_reuniones<-as.factor(meli_marzo_1$sala_reuniones)
meli_marzo_1$spa<-as.factor(meli_marzo_1$spa)
meli_marzo_1$salon_comunal<-as.factor(meli_marzo_1$salon_comunal)
meli_marzo_1$seguridad<-as.factor(meli_marzo_1$seguridad)
meli_marzo_1$jacuzzi<-as.factor(meli_marzo_1$jacuzzi)
meli_marzo_1$tipo_inmueble_2<-as.factor(meli_marzo_1$tipo_inmueble_2)
meli_marzo_1$operacion_2<-as.factor(meli_marzo_1$operacion_2)
meli_marzo_1$amoblado<-as.factor(meli_marzo_1$amoblado)
meli_marzo_1$banos<-as.factor(meli_marzo_1$banos) # ver luego de pasar a numerico
meli_marzo_1$ap_ppiso<-as.factor(meli_marzo_1$ap_ppiso) # ver
meli_marzo_1$dormitorios<-as.factor(meli_marzo_1$dormitorios)
meli_marzo_1$tipo_edif<-as.factor(meli_marzo_1$tipo_edif)
meli_marzo_1$estado<-as.factor(meli_marzo_1$estado)
meli_marzo_1$expensas<-as.numeric(meli_marzo_1$expensas)
meli_marzo_1$luminosidad<-as.factor(meli_marzo_1$luminosidad)
meli_marzo_1$sup_constru<-as.numeric(meli_marzo_1$sup_constru)
meli_marzo_1$sup_terr<-as.numeric(meli_marzo_1$sup_terr)
meli_marzo_1$orientacion<-as.factor(meli_marzo_1$orientacion)
meli_marzo_1$antiguedad<-as.factor(meli_marzo_1$antiguedad) #ver luego para pasar a numérico, en algunos casos está el ano de construcción, en otros simplemente la antiguedad
meli_marzo_1$altillo<-as.factor(meli_marzo_1$altillo)
meli_marzo_1$bodega<-as.factor(meli_marzo_1$bodega)
meli_marzo_1$galeria<-as.factor(meli_marzo_1$galeria)
meli_marzo_1$sotano<-as.factor(meli_marzo_1$sotano)
meli_marzo_1$alarma<-as.factor(meli_marzo_1$alarma)
meli_marzo_1$futbol<-as.factor(meli_marzo_1$futbol)
meli_marzo_1$estufa_lena<-as.factor(meli_marzo_1$estufa_lena)
meli_marzo_1$porton_electr<-as.factor(meli_marzo_1$porton_electr)
meli_marzo_1$garage<-as.factor(meli_marzo_1$garage)
meli_marzo_1$tipo_ubicacion<-as.factor(meli_marzo_1$tipo_ubicacion) # si está sobre la calle, country, barrio privado
meli_marzo_1$num_plantas<-as.factor(meli_marzo_1$num_plantas)
meli_marzo_1$agua_corr<-as.factor(meli_marzo_1$agua_corr)
meli_marzo_1$gas_caneria<-as.factor(meli_marzo_1$gas_caneria)
meli_marzo_1$luz_electr<-as.factor(meli_marzo_1$luz_electr)
meli_marzo_1$acceso<-as.factor(meli_marzo_1$acceso) # con asfalto, con rampa, etc.
meli_marzo_1$disposicion<-as.factor(meli_marzo_1$disposicion) # para terrenos
meli_marzo_1$horario_contacto<-as.factor(meli_marzo_1$horario_contacto)
meli_marzo_1$apto_profesional<-as.factor(meli_marzo_1$apto_profesional)
meli_marzo_1$condicion_2<-as.factor(meli_marzo_1$condicion_2) # usado o nuevo
meli_marzo_1$aircond_pamb<-as.factor(meli_marzo_1$aircond_pamb) # entiendo que significa que hay ac en cada ambiente?
meli_marzo_1$calefacc_pamb<-as.factor(meli_marzo_1$calefacc_pamb)
meli_marzo_1$mascotas<-as.factor(meli_marzo_1$mascotas)
meli_marzo_1$ambientes<-as.factor(meli_marzo_1$ambientes)
meli_marzo_1$ascensores<-as.factor(meli_marzo_1$ascensores)
meli_marzo_1$capacidad_personas<-as.factor(meli_marzo_1$capacidad_personas) # es más una var de alquileres
meli_marzo_1$pisos_edificio<-as.factor(meli_marzo_1$pisos_edificio)
meli_marzo_1$tipo_inmueble_3<-as.character(meli_marzo_1$tipo_inmueble_3) # descripción extensa
meli_marzo_1$casco<-as.factor(meli_marzo_1$casco)
meli_marzo_1$casa_casero<-as.factor(meli_marzo_1$casa_casero)
meli_marzo_1$antig_casco<-as.factor(meli_marzo_1$antig_casco) # ni idea qué es el casco, ver
meli_marzo_1$estado_casco<-as.factor(meli_marzo_1$estado_casco)
meli_marzo_1$galpones<-as.factor(meli_marzo_1$galpones)
meli_marzo_1$tanque_agua<-as.factor(meli_marzo_1$tanque_agua)
meli_marzo_1$desag_cloaca<-as.factor(meli_marzo_1$desag_cloaca)
meli_marzo_1$pavimento<-as.factor(meli_marzo_1$pavimento)
meli_marzo_1$alarma_2<-as.factor(meli_marzo_1$alarma_2)
meli_marzo_1$deposito<-as.factor(meli_marzo_1$deposito)
meli_marzo_1$sist_incendio<-as.factor(meli_marzo_1$sist_incendio)
meli_marzo_1$vestuario<-as.factor(meli_marzo_1$vestuario) # hay algunas canchas de fut o locales en venta que tienen vestuarios
meli_marzo_1$localizacion_local<-as.factor(meli_marzo_1$localizacion_local) # para locales, etc. 
meli_marzo_1$balcon<-as.factor(meli_marzo_1$balcon) # en esta base no hay obs, estarán como terraza?
meli_marzo_1$comedor_diario<-as.factor(meli_marzo_1$comedor_diario) # ni idea a q se refiere
meli_marzo_1$bar<-as.factor(meli_marzo_1$bar)
meli_marzo_1$caja_seg<-as.factor(meli_marzo_1$caja_seg)
meli_marzo_1$golf<-as.factor(meli_marzo_1$golf)
meli_marzo_1$paddle<-as.factor(meli_marzo_1$paddle)
meli_marzo_1$pileta<-as.factor(meli_marzo_1$pileta) # diferente de piscina?
meli_marzo_1$sauna<-as.factor(meli_marzo_1$sauna)
meli_marzo_1$solarium<-as.factor(meli_marzo_1$solarium)
meli_marzo_1$salon_comunal_2<-as.factor(meli_marzo_1$salon_comunal_2)
meli_marzo_1$quincho<-as.factor(meli_marzo_1$quincho)
meli_marzo_1$basquet<-as.factor(meli_marzo_1$basquet)
meli_marzo_1$volley<-as.factor(meli_marzo_1$volley)
meli_marzo_1$hogar_lena<-as.factor(meli_marzo_1$hogar_lena) #diferencia con estufa a lena??
meli_marzo_1$salamandra<-as.factor(meli_marzo_1$salamandra)
meli_marzo_1$vigilancia<-as.factor(meli_marzo_1$vigilancia)
meli_marzo_1$sup_contru_2<-as.factor(meli_marzo_1$sup_contru_2)
meli_marzo_1$amb_casco<-as.factor(meli_marzo_1$amb_casco)
meli_marzo_1$recepcion<-as.factor(meli_marzo_1$recepcion)
meli_marzo_1$cobertura_techo<-as.factor(meli_marzo_1$cobertura_techo)
meli_marzo_1$tipo_cochera<-as.factor(meli_marzo_1$tipo_cochera)



str(meli_marzo_1, list.len = ncol(meli_marzo_1))
summary(meli_marzo_1)
head(meli_marzo_1)
dim(meli_marzo_1)
# Me quedo con 126 variables

#genero variables para identificar momento de bajada y fuente, en este caso Mercadolibre
meli_marzo_1$mes <- "marzo"
meli_marzo_1$dia <- 24
meli_marzo_1$ano <- 2018
meli_marzo_1$fuente <- "meli"

# defino data frame "final"
meli_2018_03 <- as.data.frame(meli_marzo_1)
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
