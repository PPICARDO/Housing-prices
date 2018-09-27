# data cleaning
library(readr)
library(readxl)
library(dplyr)
library(ggmap)
library(tidyverse)
library(tibble)
library(ggmap)
library(leaflet)
setwd("C:/Users/Usuario/Documents/Housing-prices-tesina/07_meli")
# comenzamos con los datos de julio

# Mercadolibre julio

meli_julio_orig <- read_csv("2018_07_25_Mercadolibre.csv", quote = )
str(meli_julio_orig, list.len = ncol(meli_julio_orig))
dim(meli_julio_orig)
# 43594   208


# Tomamos solo las de Montevideo, en esta bajada está todo el país
meli_julio_1 <- subset(meli_julio_orig, state == "Montevideo" & category_1=="Venta")
meli_julio_1 <- data.frame(meli_julio_1)
head(meli_julio_1)
str(meli_julio_1, list.len = ncol(meli_julio_1))
dim(meli_julio_1)
# 32242   209

# Sacamos una primer conjunto de variables que no nos interesarán 
# Básicamente refieren a alquileres o a terrenos/campos

meli_julio_1$TV.por.cable <- NULL
meli_julio_1$Desayuno.incluido <- NULL
meli_julio_1$Heladera.con.freezer <- NULL
meli_julio_1$Hidromasaje <- NULL
meli_julio_1$Lavavajillas <- NULL
meli_julio_1$Lavarropa.Secarropa <- NULL
meli_julio_1$Microondas <- NULL
meli_julio_1$Servicio.de.limpieza <- NULL  
meli_julio_1$Wifi <- NULL
meli_julio_1$Apto.para.familias.ni.f1.os <- NULL
meli_julio_1$Forestaci.f3.n <- NULL
meli_julio_1$Silos <- NULL
meli_julio_1$Bebederos <- NULL
meli_julio_1$Corrales <- NULL
meli_julio_1$Corral <- NULL
meli_julio_1$Potreros <- NULL
meli_julio_1$Check.in <- NULL
meli_julio_1$Check.out <- NULL
meli_julio_1$Desde <- NULL #
meli_julio_1$Desde..Fecha. <- NULL
meli_julio_1$Hasta <- NULL #
meli_julio_1$Estad.ed.a.M.ed.nima <- NULL
meli_julio_1$Tipo.de.campo <- NULL
meli_julio_1$Apto.para.fumadores <- NULL
meli_julio_1$Hect.e1.reas.totales <- NULL
meli_julio_1$Distancia.al.asfalto..Km. <- NULL
meli_julio_1$Grupo.eletr.f3.geno <- NULL
meli_julio_1$Manga..ancho. <- NULL
meli_julio_1$Restaurante <- NULL
meli_julio_1$Linea.telef.f3.nica <- NULL
meli_julio_1$Tel.e9.fono <- NULL
meli_julio_1$Luz.a.motor <- NULL
meli_julio_1$Molinos <- NULL
meli_julio_1$Hasta..Fecha. <- NULL
meli_julio_1$N.fa.mero.de.camas <- NULL
meli_julio_1$Tipo.de.habitaci.f3.n <- NULL
meli_julio_1$Estacionamiento.de.cortes.ed.a <- NULL
meli_julio_1$Estacionamiento <- NULL


# aparecieron más variables en esta bajada, vamos a sacar alguna más
meli_julio_1$Horario.check.in <- NULL
meli_julio_1$Horario.check.out<- NULL
meli_julio_1$Estad.ed.a.m.ed.nima..noches. <- NULL
meli_julio_1$L.ed.nea.telef.f3.nica <- NULL
meli_julio_1$Business.center<- NULL
meli_julio_1$Cisterna<- NULL
meli_julio_1$Grupo.electr.f3.geno  <- NULL
meli_julio_1$Solo.familias <- NULL  
meli_julio_1$Apto.para.familias.con.ni.f1.os <- NULL
meli_julio_1$Vajilla <- NULL
meli_julio_1$Hu.e9.spedes <- NULL
meli_julio_1$X.c1.rea.de.juegos.infantiles   <- NULL
meli_julio_1$Caballeriza <- NULL
meli_julio_1$No <- NULL
meli_julio_1$Valet.parking   <- NULL
meli_julio_1$Valor.del.IPTU  <- NULL

meli_julio_1$Servicio.de.desayuno <- NULL
meli_julio_1$X.c1.rea.de.cine <- NULL
meli_julio_1$Galer.ed.a <- NULL
meli_julio_1$Plataformas.para.trailers <- NULL
meli_julio_1$TV <- NULL

meli_julio_1$Lavarropa <- NULL
meli_julio_1$Camas<- NULL
meli_julio_1$Estacionamiento.para.visitantes <- NULL
meli_julio_1$Sal.f3.n.de.fiestas <- NULL
meli_julio_1$Sitio.de.origen <- NULL
meli_julio_1$Disposici.f3.n.del.lote  <- NULL
meli_julio_1$B.e1.scula <- NULL
meli_julio_1$Probador <- NULL
meli_julio_1$Rampa.para.silla.de.ruedas <- NULL
meli_julio_1$Distancia.al.asfalto <- NULL
meli_julio_1$X.c1.rea.de.comedor  <- NULL
meli_julio_1$X.c1.reas.verdes <- NULL
meli_julio_1$Ba.f1.os.por.piso <- NULL
meli_julio_1$N.fa.mero.de.oficinas <- NULL
meli_julio_1$Oficinas.por.piso <- NULL
meli_julio_1$Superficie.cubierta.del.casco <- NULL
meli_julio_1$Tipo.de.bodega <- NULL
meli_julio_1$Heladera <- NULL
meli_julio_1$Caja.de.seguridad <- NULL

head(meli_julio_1)
dim(meli_julio_1)
# 32242   145

str(meli_julio_1, list.len = ncol(meli_julio_1))

# Modificar los nombres
# minúscula siempre, sin tildes con _1 o _2

meli_julio_1 <- rename(meli_julio_1, 
                       titulo=title, categ=category_id, operacion=category_1, tipo_inmueble=category_2, 
                       precio=price, moneda=currency_id, condicion=condition, barrio=city, departamento=state, 
                       direccion= address_line, zip=zip_code, lat=latitude, long=longitude, 
                       condicion_2=Condici.f3.n.del..ed.tem, horario_contacto=Horario.de.contacto, sup_cub=Superficie.cubierta,
                       operacion_2=Operaci.f3.n, antiguedad=Antig.fc.edad,tipo_inmueble_2=Inmueble, sup_tot = Superficie.total,
                       dormsuite=Dormitorio.en.suite, jardin=Jard.ed.n, placard = Placards, parrillero=Parrillero, 
                       bano_soc = Ba.f1.o.social, mascotas=Se.admiten.mascotas, aircond=Aire.acondicionado, #quincho=Quincho,
                       basquet=Cancha.de.b.e1.squetbol,# golf=Cancha.de.golf, voley=Cancha.de.voley,
                       calefacc=Calefacci.f3.n, jacuzzi= Jacuzzi, paddle=Cancha.de.paddle ,piscina=Piscina,tenis=Cancha.de.tenis, dormitorios=Dormitorios, pisos=Pisos,
                       banos=Ba.f1.os, tipo_casa=Tipo.de.casa, cocheras=Cocheras, comedor=Comedor, #comedor2=Comedor.diario, 
                       #bar=Bar,pileta=Pileta, sauna=Sauna, solarium=Solarium, 
                       internet=Acceso.a.internet,
                       cocina=Cocina, lavadero=Lavadero, living=Living, dorm_serv=Dormitorio.de.servicio, patio=Patio,
                       playro=Playroom, vestidor= Vestidor, estudio=Estudio, terraza=Terraza, gym=Gimnasio, ascensores=Ascensor,
                       salon_comunal=Sal.f3.n.de.usos.m.fa.ltiples, #salon_comunal2=Sal.f3.n.de.uso.com.fa.n,
                       seguridad= Seguridad, recepcion=Recepci.f3.n,sala_reuniones=Sala.de.reuniones,
                       agua_corr=Agua.corriente, agua_corr2=Agua.Corriente, bodega=Bodegas, sist_incendio=Sistema.contra.incendio, 
                       porton_autom=Port.f3.n.autom.e1.tico, chimenea=Chimenea, ap_ppiso=Apartamentos.por.piso,
                       orientacion=Orientaci.f3.n, expensas=Gastos.comunes, balcon=Balc.f3.n, roof_garden=Roof.garden,
                       amoblado=Amoblado,  gas_natural=Gas.natural, comercial=Uso.comercial, altillo=Altillo, alarma=Alarma,
                       luz_electr=Luz.el.e9.ctrica, acceso=Acceso, apto_credito=Apto.cr.e9.dito, disposicion=Disposici.f3.n,
                       ambientes=Ambientes, desayunador=Desayunador, caldera=Caldera, piso_unid=Piso..unidad., 
                       sist_ventilacion=Sistema.de.ventilaci.f3.n, casco=Casco, galpones=Galp.f3.n, #galpones2=Galpones, 
                       tanque_agua=Tanque.de.agua,
                       forma_terreno=Forma.del.terreno, desag_cloaca=Cloaca, metros_frente=Metros.de.frente, metros_fondo=Metros.de.fondo,
                       futbol=Cancha.de.f.fa.tbol, tipo_cochera=Tipo.de.cochera, baulera=Baulera, biblio=Biblioteca, bodega2=Bodega,
                       depserv=Dependencia.de.servicio, entrepiso=Entrepiso,escritorio=Escritorio, estar=Estar,  
                       livingcomedor=Living.comedor, sotano=S.f3.tano, toilette= Toilette, estufa_lena= Estufa.a.le.f1.a, #estufa_lena2= Hogar.a.le.f1.a,
                       #salamandra=Salamandra, 
                       internet2=Conexi.f3.n.a.internet, porton_electr=Port.f3.n.el.e9.ctrico, tipo_edif=Tipo.de.edificaci.f3.n,
                       estado=Estado, garage=Garage, tipo_ubicacion=Lugar, luminosidad=Luminosidad, 
                       sup_constru=Superficie.construida..m.U.00B2..,  sup_terr=Superficie.del.terreno..m.U.00B2.., num_plantas=Plantas,
                       mascotas2=Permite.mascotas, capacidad_personas=Capacidad.de.personas, asc_serv=Ascensor.de.servicio, 
                       guarderia=Guarder.ed.a, kitchenette=Kitchenette, lavadero_2=Laundry, losa_rad=Losa.radiante, spa=Spa,
                       apto_profesional=Apto.profesional, pisos_edificio=Cant.de.pisos.del.edificio, expensas2=Expensas.., 
                       sup_balcon=Superficie.de.balc.f3.n, antig_casco=Antig.fc.edad.del.casco, #mas40=M.e1.s.de.40.a.f1.os,
                       acceso_cochera=Acceso.de.cochera, tipo_apto=Tipo.de.departamento,
                       cobertura_techo=Tipo.de.cobertura, alarma_2=Alarma.de.seguridad,deposito=Dep.f3.sito,
                       gas_caneria=Gas.de.ca.f1.er.ed.a, vestuario=Vestuario,localizacion_local=Localizaci.f3.n, 
                       lobby=Lobby, amb_casco=Ambientes.del.casco, operacion_3=Subtipo.de.operaci.f3.n, 
                       tipo_prop=Tipo.de.propiedad, aircond_pamb=Aire.acondicionado.por.ambiente, calefacc_pamb=Calefacci.f3.n.por.ambiente,
                       altura=Altura, #casa_casero=Casa.del.casero, desag_cloaca2=Desag.fc.e.Cloacal, pavimento=Pavimento, 
                       tipo_inmueble_3=Tipo.de.inmueble, sup_contru_2=Superficie.construida...m.U.00B2.., #max30=Max.30.a.f1.os , max5=Max.05.a.f1.os,
                       #aestrenar=A.estrenar,estado_casco=Estado.del.casco, 
                       soporte_piso=Soporte.de.piso, sup_terr2=Superficie.de.terreno)

str(meli_julio_1, list.len = ncol(meli_julio_1))
summary(meli_julio_1)


# variables numeric
num <- c("precio", "zip", "lat", "long", "sup_cub", "antiguedad", "sup_tot", "dormitorios", "banos", "ascensores",
         "ap_ppiso", "expensas", "ambientes", "metros_frente", "metros_fondo", "sup_constru", "sup_terr", "sup_terr2",	 
         "num_plantas", "capacidad_personas", "pisos_edificio",	 "expensas2",	"sup_balcon",	 "antig_casco", 
         "altura", "sup_contru_2") 
# meli_julio_1[,num] <- lapply(meli_julio_1[,num], as.numeric)

# variables chr
chr <- c("titulo", "direccion")
meli_julio_1[,chr] <- lapply(meli_julio_1[,chr], as.character)

# variables logical
logic <- c( "dormsuite",	 "jardin",	 "placard",
            "parrillero", "bano_soc", 	 "mascotas",	 "aircond",	 "basquet", "calefacc",	 "jacuzzi",	 "paddle",
            "piscina", "tenis", "cocheras",	 "comedor",	 "internet", "cocina",	 "lavadero",	 "living",	 "dorm_serv",
            "patio",	"playro",	 "vestidor",	 "estudio",	 "terraza",	 "gym",	 	"salon_comunal",	 "seguridad",
            "recepcion",	"sala_reuniones",	"agua_corr",	 "bodega",	 "sist_incendio", "porton_autom", "chimenea",
            "balcon",	 "roof_garden", "amoblado", "gas_natural",	 "comercial",	 "altillo",	 "alarma", "luz_electr",
            "acceso",	 "apto_credito", "desayunador", "caldera", "sist_ventilacion",	 "casco",	 "galpones","tanque_agua", 
            "desag_cloaca", "futbol", "baulera",	 "biblio",	 "bodega2",	"depserv",	 "entrepiso",	"escritorio",	 "estar", 
            "livingcomedor",	 "sotano",	 "toilette",	 "estufa_lena", "internet2", "porton_electr", "garage", "mascotas2",	 
            "asc_serv", "guarderia",	 "kitchenette",	 "lavadero_2",	 "losa_rad",	 "spa", "apto_profesional", #"mas40", 
            "alarma_2","deposito", "gas_caneria",	 "vestuario", "lobby" #"casa_casero",	 "desag_cloaca2", "pavimento", "max30","max5",	"aestrenar"
)
meli_julio_1[,logic] <- lapply(meli_julio_1[,logic], as.logical)

# variables factor
factor <- c("categ", "operacion",	 "tipo_inmueble","moneda", "condicion","barrio", "departamento",	 	
            "condicion_2", "horario_contacto", "operacion_2",	"tipo_inmueble_2", "pisos", 
            "tipo_casa","orientacion","disposicion","piso_unid",  "forma_terreno", "tipo_cochera",	 			
            "tipo_edif", "estado", "tipo_ubicacion",	 "luminosidad",	 "tipo_apto",	"acceso_cochera",
            "cobertura_techo", "localizacion_local", "operacion_3", "tipo_prop", "tipo_inmueble_3", #"estado_casco" ,
            "soporte_piso")
meli_julio_1[,factor] <- lapply(meli_julio_1[,factor], as.factor)


str(meli_julio_1, list.len = ncol(meli_julio_1))
summary(meli_julio_1, list.len = ncol(meli_julio_1))
head(meli_julio_1)
dim(meli_julio_1)
# 32242 145 variables

#genero variables para identificar momento de bajada y fuente, en este caso Mercadolibre
meli_julio_1$mes <- "julio"
meli_julio_1$dia <- 25
meli_julio_1$ano <- 2018
meli_julio_1$fuente <- "meli"

# defino data frame "final"
meli_2018_07 <- as.data.frame(meli_julio_1)
str(meli_2018_07, list.len=ncol(meli_2018_07))
dim(meli_2018_07)
# 32242 obs 149 variables

# guardo en Rdata - base ordenada 1 de datos MELI febrero
save(meli_2018_07,file="2018_07_27_meli.Rda")

# para traerlo
load("2018_07_27_meli.Rda")

# falta ver el tema de duplicados y sacar malos datos
