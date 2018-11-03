library(dplyr)
# agrego las columnas de catastro al SpatialPolygon

# primero renombro las columnas de catastro
catastro <- rename(catastro, tipo_inmueble = V1, departamento = V2, localidad = V3, padron = V4, block = V5, epss = V6, unidad = V7, sup_terr = V8, sup_constru = V9, valor_realter = V10, valor_realmej = V11, valor_realtot = V12, valor_impuesto = V13, fecha_djcu = V14, fecha_vigencia = V15)

#merge de catastro con spatial dataframe y transacciones
colnames(catastro)[4] <- "PADRON"
if(!is.numeric(sp_df_transformado@data$PADRON)){
  sp_df_transformado@data$PADRON <- as.numeric(as.character(sp_df_transformado@data$PADRON))
}
catastro$PADRON <- as.numeric(as.character(catastro$PADRON))

sp_df@data <- sp_df_transformado@data %>% left_join(catastro, by = "PADRON")

# agrego las columnas de transacciones al sp

sp_df_transformado@data <- sp_df_transformado@data %>%
    left_join(transacciones, by = "PADRON")

