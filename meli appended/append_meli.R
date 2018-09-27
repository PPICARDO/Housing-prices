# Append online data de Meli

library(gtools)
library(dplyr)

setwd("C:/Users/Usuario/Documents/Housing-prices-tesina/meli appended")

# Cargamos los datos de febrero a julio de MELI
load("~/Housing-prices-tesina/meli appended/2018_02_23_meli.Rda")
load("~/Housing-prices-tesina/meli appended/2018_03_24_meli.Rda")
load("~/Housing-prices-tesina/meli appended/2018_04_23_meli.Rda")
load("~/Housing-prices-tesina/meli appended/2018_05_25_meli.Rda")
load("~/Housing-prices-tesina/meli appended/2018_06_28_meli.Rda")
load("~/Housing-prices-tesina/meli appended/2018_07_27_meli.Rda")
load("~/Housing-prices-tesina/meli appended/2018_08_25_meli.Rda")
load("~/Housing-prices-tesina/meli appended/2018_09_24_meli.Rda")

# append
test_1 <- smartbind(meli_2018_02,meli_2018_03,meli_2018_04,meli_2018_04, meli_2018_05, 
                meli_2018_06, meli_2018_07, meli_2018_08, meli_2018_09)
str(test_1,  list.len = ncol(test_1)) # la mayoría está como character
dim(test_1)
# 284788    178

# guardo en Rdata - base ordenada 1 de datos MELI febrero-setiembre (8 meses)
save(test_1,file = "append_meli_1.Rda")
load(file = "append_meli_1.Rda")

# cambiar formatos de las variables
# numéricas

num <- c("precio", "zip", "lat", "long", "sup_cub", "antiguedad", "sup_tot", "dormitorios", "banos", "ascensores",
         "ap_ppiso", "expensas", "ambientes", "metros_frente", "metros_fondo", "sup_constru", "sup_terr", "sup_terr2",	 
         "num_plantas", "capacidad_personas", "pisos_edificio",	 "expensas2",	"sup_balcon",	 "antig_casco", 
         "altura", "sup_contru_2") 
str(test_1[,num])
summary(test_1[,num])
test_1[,num] <- lapply(test_1[,num], as.numeric)


# sacar duplicados
test_2 <- unique(test_1) # (49934 duplicados, parece mucho ...)
dim(test_2)
# 234854    178
test_2%>% mutate(comedor_diario=coalesce(Comedor.diario,comedor_diario))

head(test_2$lat[is.na(as.numeric(test_2$lat))])

# extras ...
test_99 <- subset.data.frame(test_1, precio > 1000 & precio < 2000000)
a <- tapply(test_99$precio, test_99$mes, median)
plot(a, type = "l")
