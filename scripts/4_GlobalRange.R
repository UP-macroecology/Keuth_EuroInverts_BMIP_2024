# Extract the global range of some species from the IUCN Red List data

# Load libraries
library(terra)

# Load data
odonata <- vect("data/IUCN polygons/FW_ODONATA/FW_ODONATA.shp")
TREAM <- read.csv("data/TREAM_preprocessed.csv")

names(odonata)
odonata_names <- unique(odonata$sci_name)

TREAM_odonata <- subset(TREAM, TREAM$Group == "Odonata")
TREAM_odonata <- TREAM_odonata[!is.na(TREAM_odonata$taxon),]
length(unique(TREAM_odonata$taxon)) #1371

polygon_odonata <- TREAM_odonata[which(TREAM_odonata$taxon %in% odonata_names), "taxon"]
length(unique(polygon_odonata)) #25 species names

odonata_sub <- subset(odonata, odonata$sci_name %in% polygon_odonata)

