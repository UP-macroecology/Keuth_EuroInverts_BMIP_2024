# Extract the global range of some species from the IUCN Red List data

# Load libraries
library(terra)

# Load data
odonata <- vect("data/IUCN polygons/FW_ODONATA/FW_ODONATA.shp")
