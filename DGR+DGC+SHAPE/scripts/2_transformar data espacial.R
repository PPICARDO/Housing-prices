# EPSG codes for commonly used CRS (in the U.S.)
# Latitude/Longitude
# WGS84 (EPSG: 4326)
# Commonly used by organizations that provide GIS data for the entire globe or many countries. CRS used by Google Earth!!

# opciones para la transf del sistema de coordenadas

# 1 indicando el sistema
sp_df_transformado <- sp::spTransform(sp_df, CRS("+init=epsg:4326"))

# 2 - haciendo referencia a otro objeto,or, reference the CRS of another spatial object:
#newData <- spTransform(x, proj4string(OtherData))

# lista de todos los tipos de sistemas de coordenadas de proyecciones espaciales
# epsg <- rgdal::make_EPSG()
