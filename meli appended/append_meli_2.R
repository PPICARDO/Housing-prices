# Trabajar con datos de MELI # test_1
load(file = "meli appended/append_meli_1.Rda")
dim(test_1)
# 284788    178

# saco los duplicados
test_2 <- unique(test_1) # (49934 duplicados, ni idea por qué hay tantos, parecen muchos ...)
dim(test_2)
str(test_2,  list.len = ncol(test_2))

#selecciono variables (prácticamente todas ahora) para empezar a describir un poco y HACER !
test_3 <- unique(test_2[c("id","titulo","tipo_inmueble","precio","moneda","condicion","barrio",
                          "direccion","lat","long","terraza", "biblio","cocina","comedor","entrepiso","escritorio","estar","lavadero","lavadero_2" ,"livingcomedor","living","estado","patio","toilette","aircond","calefacc","asc_serv","jardin","kitchenette","losa_rad","parrillero","piscina", "salon_comunal", "seguridad", "jacuzzi", "tipo_inmueble_2","operacion_2","amoblado","banos", "ap_ppiso", "dormitorios","tipo_edif", "expensas2","luminosidad", "orientacion","antiguedad", "altillo", "sotano", "alarma", "estufa_lena", "porton_electr", "garage","tipo_ubicacion", "sup_constru","sup_cub", "sup_tot", "sup_terr","sup_terr2","num_plantas", "gas_caneria", "ascensores","pisos_edificio", "tipo_inmueble_3", "alarma_2", "deposito", "balcon", "quincho", "estufa_lena2", "sup_contru_2","mes","start_time","stop_time","expensas", "bano_soc", "pisos", "tipo_casa", "cocheras","estudio","porton_autom","chimenea", "roof_garden", "gas_natural", "comercial","apto_credito","desayunador","piso_unid", "sist_ventilacion", "forma_terreno", "metros_frente", "metros_fondo", "sup_balcon", "tipo_apto", "lobby", "operacion_3", "tipo_prop" )])


str(test_3,  list.len = ncol(test_3)) # 234854 obs. of  97 variables (algunas a "mergear" en una variable, otras a eliminar porque son puro n/a)

#guardo el archivo
save(test_3, file = "muestra_var_meli2.Rda")
#cargo el archivo
load(file = "muestra_var_meli2.Rda")


# Casi todas las variables están como character, salvo precio. Vamos a empezar a verlas.

# variables factor ----
#algunos podrían ser numéricas (por ej, dormitorios, baños), lógicas (sí / no)
fact <- c("tipo_inmueble","moneda","condicion","barrio",
         "terraza", "biblio","cocina","comedor","entrepiso","escritorio","estar","lavadero","lavadero_2" ,"livingcomedor","living","estado","patio","toilette","aircond","calefacc","asc_serv","jardin","kitchenette","losa_rad","parrillero","piscina", "salon_comunal", "seguridad", "jacuzzi", "tipo_inmueble_2","operacion_2","amoblado","banos", "ap_ppiso", "dormitorios","tipo_edif","luminosidad", "orientacion","antiguedad", "altillo", "sotano", "alarma", "estufa_lena", "porton_electr", "garage","tipo_ubicacion","num_plantas", "gas_caneria", "ascensores","pisos_edificio", "tipo_inmueble_3", "alarma_2", "deposito", "balcon", "quincho", "estufa_lena2","mes", "bano_soc", "pisos", "tipo_casa", "cocheras","estudio","porton_autom","chimenea", "roof_garden", "gas_natural", "comercial","apto_credito","desayunador","piso_unid", "sist_ventilacion", "forma_terreno", "metros_frente", "metros_fondo", "tipo_apto", "lobby", "operacion_3", "tipo_prop")
test_3[,fact] <- lapply(test_3[,fact], as.factor)

str(test_3,  list.len = ncol(test_3))


#tema de encoding ---- fuente: https://rdrr.io/r/base/iconv.html
# encoding issue, resuelvo así para no demorar más
test_3$titulo <- gsub("\\?+","'",iconv(test_3$titulo, "latin1", "UTF-8", sub=""))
test_3$barrio <- gsub("\\?+","'",iconv(test_3$barrio, "latin1", "UTF-8", sub=""))
test_3$direccion <- gsub("\\?+","'",iconv(test_3$direccion, "latin1", "UTF-8", sub=""))
test_3$tipo_apto <- gsub("\\?+","'",iconv(test_3$tipo_apto, "latin1", "UTF-8", sub=""))
test_3$antiguedad <- gsub("\\?+","'",iconv(test_3$antiguedad, "latin1", "UTF-8", sub=""))
test_3$tipo_inmueble_3 <- gsub("\\?+","'",iconv(test_3$tipo_inmueble_3, "latin1", "UTF-8", sub=""))
test_3$terraza <- gsub("\\?+","'",iconv(test_3$terraza, "latin1", "UTF-8", sub=""))
test_3$tipo_casa <- gsub("\\?+","'",iconv(test_3$tipo_casa, "latin1", "UTF-8", sub=""))
test_3$banos <- gsub("\\?+","'",iconv(test_3$banos, "latin1", "UTF-8", sub=""))
test_3$ap_ppiso <- gsub("\\?+","'",iconv(test_3$ap_ppiso, "latin1", "UTF-8", sub=""))



#con las si / no
logic <- c("biblio","cocina","comedor","entrepiso","escritorio","estar","lavadero","lavadero_2","livingcomedor","living","patio","toilette", "aircond","calefacc","asc_serv","jardin", "kitchenette", "losa_rad", "parrillero", "salon_comunal", "seguridad", "jacuzzi", "altillo", "sotano", "alarma", "estufa_lena",  "porton_electr","gas_caneria", "alarma_2", "deposito", "balcon", "quincho","estufa_lena2","bano_soc", "estudio","porton_autom","chimenea","roof_garden","gas_natural","comercial","apto_credito","desayunador","sist_ventilacion","lobby")
#luego ver cómo emprolijarlas
str(test_3,  list.len = ncol(test_3))

### esto tranca todo: ver : test_3[logic] <- gsub("\\?+","'",iconv(test_3[logic], "latin1", "UTF-8", sub=""))



# variables numéricas, limpiamos un poco antes ----

## sup_constru y sup_cub ----
test_3$sup_constru <- gsub("-", "", test_3$sup_constru)
test_3$sup_constru <- gsub("222222", "", test_3$sup_constru)
test_3$sup_constru <- gsub("999", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("9999", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("99999", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("999999", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("9999999", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("11111111111111110000", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("111111111111111104", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("222222", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("222222", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("1111111111", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("11111", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("1111", "", test_3$sup_constru) 
 test_3$sup_constru <- gsub("5555555555", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("4444444444", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("2777595", "", test_3$sup_constru)
 test_3$sup_constru <- gsub("3333", "", test_3$sup_constru)
 
 
# saco todo lo que haya desp del espacio
test_3$sup_constru <- gsub(" .*", "", test_3$sup_constru)
test_3$sup_cub <- gsub(" .*", "", test_3$sup_cub)
# complemento las dos variables
test_3$sup_constru <- ifelse(!is.na(test_3$sup_constru), test_3$sup_constru, test_3$sup_cub)
View(test_3$sup_constru)
test_3$sup_constru <- sub(".", "", test_3$sup_constru, fixed = TRUE)
test_3$sup_constru <- sub("0", "NA", test_3$sup_constru, fixed = TRUE)
test_3$sup_constru <- sub("1", "NA", test_3$sup_constru, fixed = TRUE)
test_3$sup_constru <- as.numeric(test_3$sup_constru)
summary(test_3$sup_constru)

test_3$sup_cub <- NULL

#### sup_constru_2
View(test_3$sup_contru_2) #POCAS OBS
test_3$sup_contru_2 <- NULL

# sup_terr y sup_terr2 ----
 test_3$sup_terr <- gsub("-", "", test_3$sup_terr) 
 test_3$sup_terr <- gsub(" .*", "", test_3$sup_terr)
 test_3$sup_terr <- gsub("1111", "", test_3$sup_terr)
 test_3$sup_terr <- gsub("9999", "", test_3$sup_terr)
 test_3$sup_terr2 <- gsub("-", "", test_3$sup_terr2) 
 test_3$sup_terr2 <- gsub(" .*", "", test_3$sup_terr2)
 test_3$sup_terr2 <- gsub("11111", "", test_3$sup_terr2)
 test_3$sup_terr2 <- gsub("9999", "", test_3$sup_terr2)
 # complemento las dos variables
 test_3$sup_terr <- ifelse(!is.na(test_3$sup_terr), test_3$sup_terr, test_3$sup_terr2)
 test_3$sup_terr2 <- NULL
 test_3$sup_terr <- as.numeric(test_3$sup_terr)
 
  # sup_tot
 test_3$sup_tot <- gsub("-", "", test_3$sup_tot) 
 test_3$sup_tot <- gsub(" .*", "", test_3$sup_tot)
 test_3$sup_tot <- gsub("N/AN/AN/AN/A", "", test_3$sup_tot) 
 test_3$sup_tot <- gsub("N/AN/AN/A", "", test_3$sup_tot) 
 test_3$sup_tot <- gsub("N/AN/A", "", test_3$sup_tot)  
 test_3$sup_tot <- gsub("N/A", "", test_3$sup_tot)   
 View(test_3$sup_tot)

 # saco valores atípicos
 test_3$sup_tot <- gsub("999", "N/A", test_3$sup_tot) 
 test_3$sup_tot <- gsub("1111", "N/A", test_3$sup_tot) 
 test_3$sup_tot <-  as.numeric(test_3$sup_tot)
 
  #antiguedad --- 
 test_3$antiguedad <- gsub(" años", "", test_3$antiguedad)
 test_3$antiguedad <- gsub("Más de ", "", test_3$antiguedad)
 test_3$antiguedad <- gsub("-1", "1", test_3$antiguedad)
 test_3$antiguedad <- gsub( "-", "N/A", test_3$antiguedad)
 test_3$antiguedad <- gsub( "Max 0", "", test_3$antiguedad)
 test_3$antiguedad <- gsub( "Max ", "", test_3$antiguedad)

 
 #guardo el archivo
 save(test_3, file = "muestra_var_meli3.Rda")
 #cargo el archivo
 load(file = "muestra_var_meli3.Rda")
 
 ################NUMERICAS####################
 
 ############### Antiguedad

 
 str(test_3$antiguedad)
 # saco las comillas que quedaron por ahí
 test_3$antiguedad <- gsub("\"","", test_3$antiguedad)

 #Resta transformar los que están en 1900 ...
 # paso a num variable nueva
 test_3$antiguedad2 <- as.numeric(test_3$antiguedad)
 # na coerced
 summary(test_3$antiguedad2)
 
 # para transformar las observaciones que están en 1900. Las de 2013 en adelante las dejo igual
 test_3$antiguedad2 <- ifelse((test_3$antiguedad2 > 1700), (2018-test_3$antiguedad2),test_3$antiguedad2)
 #  las que están en negativo son a futuro: "En construccion"

 
 # complemento las dos variables, completando las que están NA en antiguedad2
 test_3$antiguedad2 <- ifelse(!is.na(test_3$antiguedad2), test_3$antiguedad2, test_3$antiguedad)
 # supesto que "En construcción" es -2"
 test_3$antiguedad2 <- sub("En construcción", "-2", test_3$antiguedad2)
 # las de 2018 que valen 0 son "A estrenar"
 test_3$antiguedad2 <- sub("A estrenar", "0", test_3$antiguedad2)
 # "Sin definir" las pongo como 999
 test_3$antiguedad2 <- sub("Sin definir", "999", test_3$antiguedad2) 
  View(test_3$antiguedad2)
  test_3$antiguedad2 <- sub( "40 a¿¿os", "40", test_3$antiguedad2) 
  test_3$antiguedad2 <- sub( "1111", "NA", test_3$antiguedad2)
 # Indico como variable numerica
  test_3$antiguedad2 <- as.numeric(test_3$antiguedad2)
  summary(test_3$antiguedad2)
  str(test_3$antiguedad2) # 90702 valores incluido el 999 "Sin definir"
  
  
  #guardo el archivo
  save(test_3, file = "muestra_var_meli4.Rda")
  #cargo el archivo
  load(file = "muestra_var_meli4.Rda")

  ############### banos###################33
  # saco las comillas que quedaron por ahí
  test_3$banos <- gsub("\"","", test_3$banos)  
  test_3$banos <- gsub("-","", test_3$banos) 
  test_3$banos <- gsub("No tiene","0", test_3$banos) 
 # supuesto de que "más de 4" es igual a 5
  test_3$banos <- gsub(".*\\s","5", test_3$banos) 
    test_3$banos <- sub("1111","1", test_3$banos,fixed = TRUE) 
  test_3$banos <- sub("111111111","1", test_3$banos,fixed = TRUE) 
  test_3$banos <- sub("111111","1", test_3$banos,fixed = TRUE)
  test_3$banos <- sub("000000000","0", test_3$banos,fixed = TRUE)  
  test_3$banos <- as.numeric(test_3$banos)  
str(test_3$banos)
summary(test_3$banos)

############################
save(test_3, file = "muestra_var_meli5.Rda")
#cargo el archivo
load(file = "muestra_var_meli5.Rda")

str(test_3)


##### metros_frente 
View(test_3$metros_frente)
# borrro el texto que dice "m" o "metros"
test_3$metros_frente <- gsub(" .*", "", test_3$metros_frente)
# fijo como numérica
test_3$metros_frente <- as.numeric(test_3$metros_frente)

##### metros_fondo
View(test_3$metros_fondo)
# borrro el texto que dice "m" o "metros"
test_3$metros_fondo <- gsub(" .*", "", test_3$metros_fondo)
test_3$metros_fondo <- sub("11111111", "", test_3$metros_fondo, fixed = TRUE)
# fijo como numérica
test_3$metros_fondo <- as.numeric(test_3$metros_fondo)

##### piso_unid
# issue con 901 801 etc. ponen el apto en lugar del piso
View(test_3$piso_unid)
test_3$piso_unid <- as.numeric(as.character(test_3$piso_unid))
test_3$piso_unid <- ifelse((test_3$piso_unid >= 1000),substr(test_3$piso_unid, 0, 2) ,test_3$piso_unid)
test_3$piso_unid <- ifelse((test_3$piso_unid >= 100),substr(test_3$piso_unid, 0, 1) ,test_3$piso_unid)
summary(test_3$piso_unid) # 17217 con valores

#### pisos
View(test_3$pisos)
test_3$pisos <- as.numeric(as.character(test_3$pisos))
test_3$pisos <- sub("122", "12", test_3$pisos, fixed = TRUE)
test_3$pisos <- sub("98", "9", test_3$pisos, fixed = TRUE)
test_3$pisos <- sub("92", "9", test_3$pisos, fixed = TRUE)

##### ap_ppiso
View(test_3$ap_ppiso)
test_3$ap_ppiso <- gsub("\\?+","'",iconv(test_3$ap_ppiso, "latin1", "UTF-8", sub=""))
test_3$ap_ppiso <- sub("713", "13", test_3$ap_ppiso, fixed = TRUE)
test_3$ap_ppiso <- sub("204", "4", test_3$ap_ppiso, fixed = TRUE)
test_3$ap_ppiso <- sub("207", "7", test_3$ap_ppiso, fixed = TRUE)
test_3$ap_ppiso <- sub("63", "10", test_3$ap_ppiso, fixed = TRUE) #me fijé
test_3$ap_ppiso <- sub("40", "14", test_3$ap_ppiso, fixed = TRUE) #me fijé es el salvo
test_3$ap_ppiso <- sub("35", "5", test_3$ap_ppiso, fixed = TRUE) #me fijé
test_3$ap_ppiso <- sub("38", "8", test_3$ap_ppiso, fixed = TRUE) #me fijé

test_3$ap_ppiso <- as.factor(test_3$ap_ppiso)

##### cocheras
View(test_3$cocheras)
test_3$cocheras <- gsub("-","", test_3$cocheras) 
test_3$cocheras <- gsub("1111","", test_3$cocheras) 
test_3$cocheras <- as.numeric(test_3$cocheras)


##### terraza
View(test_3$terraza)
test_3$terraza <- gsub("Terraza","Sí", test_3$terraza) 
test_3$terraza <- as.factor(test_3$terraza)

##### barrio
test_3$barrio <- as.factor(test_3$barrio) # Factor w/ 68 levels


##### tipo_inmueble y tipo_inmueble_2 redundantes, el_2 está peor
View(test_3$tipo_inmueble_2)
View(test_3$tipo_inmueble)
test_3$tipo_inmueble_2 <- NULL

tipo_inmueble_3

##### dormitorios
View(test_3$dormitorios)
test_3$dormitorios <- gsub("\\?+","'",iconv(test_3$dormitorios, "latin1", "UTF-8", sub="")) 
test_3$dormitorios <- sub("^$", "NA",test_3$dormitorios)
test_3$dormitorios <- gsub("\",\",","",test_3$dormitorios)
test_3$dormitorios <- gsub(" ","NA",test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- gsub(" ","",test_3$dormitorios)
test_3$dormitorios <- gsub("-","", test_3$dormitorios) 
test_3$dormitorios <- sub("11111111111", "1", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- sub("00", "0", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- sub("01", "1", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- sub("02", "2", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- sub("0", "No tiene", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- sub("111", "1", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- sub("1No tiene", "No tiene", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- sub("No tiene2", "No tiene", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- sub("", "NA", test_3$dormitorios, fixed = TRUE)
test_3$dormitorios <- as.factor(test_3$dormitorios)
str(test_3$dormitorio) # Factor w/ 28 levels


#### sup_balcon 
View(test_3$sup_balcon)
test_3$sup_balcon <- gsub("\\?+","'",iconv(test_3$sup_balcon, "latin1", "UTF-8", sub=""))
test_3$sup_balcon <- gsub("-","",test_3$sup_balcon)
test_3$sup_balcon <- gsub(" .*", "", test_3$sup_balcon)
test_3$sup_balcon <- as.numeric(test_3$sup_balcon)
summary(test_3$sup_balcon)

#### tipo_apto
View(test_3$tipo_apto)
test_3$tipo_apto <- as.factor(test_3$tipo_apto)

#### tipo_casa
View(test_3$tipo_casa)
test_3$tipo_casa <- as.factor(test_3$tipo_casa)

############################
save(test_3, file = "muestra_var_meli6.Rda")
#cargo el archivo
load(file = "muestra_var_meli6.Rda")

str(test_3)

####  expensas y expensas_2
View(test_3$expensas)
test_3$expensas <- gsub("\\?+","'",iconv(test_3$expensas, "latin1", "UTF-8", sub=""))
test_3$expensas <- gsub("-","",test_3$expensas)
test_3$expensas <- sub("false","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("no tiene","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("NO TIENE","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No tiene","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("no paga","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No Paga","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("NO PAGA","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No posee","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("no hay","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("Sin Gastos comunes","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("NO","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("no","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No tiene g.comunes","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No tiene gastos comunes","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("Sin gastos comunes","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No tiene.","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No Tiene","No tiene",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("propietario ","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("Pregunte!","NA",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("Aprox. ","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" Aprox","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" aprx.","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" APROX.","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" APROX","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" Aprox.","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" aprox.","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" aprox","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" todo el año","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" incluye F.Reserva","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("en el entorNo tiene de ","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("U$D 500","17000",test_3$expensas, fixed = TRUE) #supuesto TC son solo unas pocas obs
test_3$expensas <- sub("b","17000",test_3$expensas, fixed = TRUE) #supuesto TC son solo unas pocas obs
test_3$expensas <- sub(" IMPECABLE","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(" (incluye agua)","",test_3$expensas, fixed = TRUE)

test_3$expensas <- sub("$U ","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("$","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub(".","",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("0.","0",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No tiene","0",test_3$expensas, fixed = TRUE)
test_3$expensas <- sub("No","0",test_3$expensas, fixed = TRUE)

test_3$expensas <- sub("Gastos Bajos","1000",test_3$expensas, fixed = TRUE) # sup
test_3$expensas <- sub("muy bajas","800",test_3$expensas, fixed = TRUE) # sup
test_3$expensas <- sub("bajas","1000",test_3$expensas, fixed = TRUE) # sup
test_3$expensas <- sub("BAJAS","1000",test_3$expensas, fixed = TRUE) # sup
test_3$expensas <- sub("bajos","1000",test_3$expensas, fixed = TRUE) # sup
test_3$expensas <- sub("Bajos","1000",test_3$expensas, fixed = TRUE) # sup
test_3$expensas <- sub("Máx ","",test_3$expensas, fixed = TRUE) 
test_3$expensas <- sub("Desde Us100150","5000",test_3$expensas, fixed = TRUE) #sup
test_3$expensas <- sub(" Todo el año","",test_3$expensas, fixed = TRUE) 
test_3$expensas <- sub(" y fondo de reserva 1000","",test_3$expensas, fixed = TRUE) #sup
test_3$expensas <- sub(" pesos","",test_3$expensas, fixed = TRUE) #sup
test_3$expensas <- sub(" fijos","",test_3$expensas, fixed = TRUE) #sup


test_3$expensas <- as.numeric(test_3$expensas)
# NAs introduced by coercion  SON LOS DE "CONSULTAR" "A CONFIRMAR" y ese estilo
summary(test_3$expensas)


View(test_3$expensas2)
test_3$expensas2 <- gsub("\\?+","'",iconv(test_3$expensas2, "latin1", "UTF-8", sub=""))
test_3$expensas2 <- gsub("-","",test_3$expensas2)
test_3$expensas2 <- sub("false","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("no tiene","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("NO TIENE","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No tiene","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("no paga","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No Paga","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("NO PAGA","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No posee","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("no hay","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("Sin Gastos comunes","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("NO","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("no","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No tiene g.comunes","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No tiene gastos comunes","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("Sin gastos comunes","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No tiene.","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No Tiene","No tiene",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("propietario ","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("Pregunte!","NA",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("Aprox. ","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" Aprox","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" aprx.","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" APROX.","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" APROX","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" Aprox.","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" aprox.","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" aprox","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" todo el año","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" incluye F.Reserva","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("en el entorNo tiene de ","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("U$D 500","17000",test_3$expensas2, fixed = TRUE) #supuesto TC son solo unas pocas obs
test_3$expensas2 <- sub("b","17000",test_3$expensas2, fixed = TRUE) #supuesto TC son solo unas pocas obs
test_3$expensas2 <- sub(" IMPECABLE","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(" (incluye agua)","",test_3$expensas2, fixed = TRUE)

test_3$expensas2 <- sub("$U ","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("$","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub(".","",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("0.","0",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No tiene","0",test_3$expensas2, fixed = TRUE)
test_3$expensas2 <- sub("No","0",test_3$expensas2, fixed = TRUE)

test_3$expensas2 <- sub("Gastos Bajos","1000",test_3$expensas2, fixed = TRUE) # sup
test_3$expensas2 <- sub("muy bajas","800",test_3$expensas2, fixed = TRUE) # sup
test_3$expensas2 <- sub("bajas","1000",test_3$expensas2, fixed = TRUE) # sup
test_3$expensas2 <- sub("BAJAS","1000",test_3$expensas2, fixed = TRUE) # sup
test_3$expensas2 <- sub("bajos","1000",test_3$expensas2, fixed = TRUE) # sup
test_3$expensas2 <- sub("Bajos","1000",test_3$expensas2, fixed = TRUE) # sup
test_3$expensas2 <- sub("Máx ","",test_3$expensas2, fixed = TRUE) 
test_3$expensas2 <- sub("Desde Us100150","5000",test_3$expensas2, fixed = TRUE) #sup
test_3$expensas2 <- sub(" Todo el año","",test_3$expensas2, fixed = TRUE) 
test_3$expensas2 <- sub(" y fondo de reserva 1000","",test_3$expensas2, fixed = TRUE) #sup
test_3$expensas2 <- sub(" pesos","",test_3$expensas2, fixed = TRUE) #sup
test_3$expensas2 <- sub(" fijos","",test_3$expensas2, fixed = TRUE) #sup
test_3$expensas2 <- as.numeric(test_3$expensas2)

test_3$expensas <- ifelse(!is.na(test_3$expensas), test_3$expensas, test_3$expensas2)
test_3$expensas2 <- NULL
summary(test_3$expensas)



#### tipo_inmueble_3
View(test_3$tipo_inmueble) # ok , factor
View(test_3$tipo_inmueble_3) # lo dejo como character

############################
save(test_3, file = "muestra_var_meli7.Rda")
#cargo el archivo
load(file = "muestra_var_meli7.Rda")

str(test_3)


##### start time
test_3$start_time <- gsub(" .*", "", test_3$start_time)
test_3$start_time <- as.POSIXct(test_3$start_time)
View(test_3$start_time)
# recordar que los NA refieren a bajadas de febrero marzo y abril

############################
save(test_3, file = "muestra_var_meli8.Rda")
#cargo el archivo
load(file = "muestra_var_meli8.Rda")
str(test_3)

#lat long como numéricas
test_3$long <- as.numeric(test_3$long)
test_3$lat <- as.numeric(test_3$lat)


# Filtrar un poco precio 
test_4 <- data.frame(subset(test_3,precio < 5000001))
test_4 <- data.frame(subset(test_4,precio > 1111))
summary(test_4$precio)
###########################################
str(test_4) # 233524 obs. of  91 variables:
View(test_4$precio)
###########################################



####################### extra algunos gráficos
# ver histograma de los precios
library(ggplot2)
qplot(test_4$precio/1000,
      geom="histogram",
      binwidth=25,  
      main="Histograma de precios 2018", 
      xlab="Precio en miles")

p<- ggplot(data=test_4, aes(test_4$precio/1000)) + 
  geom_histogram(aes(y =..count..), 
                 col="red", 
                 fill="blue", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="Histograma de precios 2018 - MELI", x="Precio", y="Observaciones")
p

# tomo una muestra con sup_constru y moneda USDpara probar cosas simples
library("tibble")    
sample_meli <- filter(test_4, sup_constru>1, sup_constru<700, moneda == "USD")
summary(sample_meli)

############################
save(sample_meli, file = "muestra_var_meli8.Rda")
#cargo el archivo
load(file = "muestra_var_meli8.Rda")

str(sample_meli) # 110657 obs. of  91 variables:



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
  scale_color_distiller(palette = "YlOrRd", direction = 1)+ 
  labs(title="Mediana de precios 2018-MELI por coordenadas")


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


map <- get_stamenmap(sac_borders, zoom = 10, maptype = "toner-lite")

ggmap(map) +
  geom_point(data = df, mapping = aes(x = longitude, y = latitude, 
                                      col = median_price, size = quantity)) +
  scale_color_distiller(palette = "YlOrRd", direction = 1)+ 
  labs(title="Mediana de precios por zonas")


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

# Ojo que el mapa enlentece la machine
mapa_meli
