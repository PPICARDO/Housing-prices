#pasar de la dirección a lat long

# Initialize the data frame
geocoded <- test_1

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(geocoded))
{
  # Print("Working...")
  result <- geocode(geocoded$direccion[i], output = "latlona", source = "google", key = AIzaSyBzOzc3D4xm2JggPIzU-URl2OgH59IvqyI)
  geocoded$lon[i] <- as.numeric(result[1])
  geocoded$lat[i] <- as.numeric(result[2])
  geocoded$geoAddress[i] <- as.character(result[3])
}

# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "geocoded.csv", row.names=FALSE)

