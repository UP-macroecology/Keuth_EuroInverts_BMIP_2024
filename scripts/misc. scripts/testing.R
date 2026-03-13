# find the reason for the two species with the larger coverage of our data than global data

# Baetis vernus
#Potamanthus luteus

load("data/TREAM.sp.Rdata")

library(rgbif)
library(maps)

potamanthus_gbif <- occ_search(scientificName = "Amphinemura sulcicollis", hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 10000)
potamanthus_gbif <- potamanthus_gbif$data
baetis_gbif <- occ_search(scientificName = "Baetis vernus", hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 10000)
baetis_gbif <- baetis_gbif$data

potamanthus_tream <- subset(TREAM.sp, TREAM.sp$binomial == "Amphinemura sulcicollis")
baetis_tream <- subset(TREAM.sp, TREAM.sp$binomial == "Baetis vernus")


maps::map('world')
points(baetis_gbif$decimalLongitude, baetis_gbif$decimalLatitude, col='red',  pch=19)
points(baetis_tream$Longitude_X, baetis_tream$Latitude_Y, col='blue',  pch=19)

maps::map('world')
points(potamanthus_gbif$decimalLongitude, potamanthus_gbif$decimalLatitude, col='red',  pch=19)
points(potamanthus_tream$Longitude_X, potamanthus_tream$Latitude_Y, col='blue',  pch=19)

# extract the convex hulls
potamanthus_gbif_test <- potamanthus_gbif[,c("decimalLongitude", "decimalLatitude", "species")]
potamanthus_gbif_test <- st_as_sf(potamanthus_gbif_test, coords=  c("decimalLongitude", "decimalLatitude"), crs = 4326)
st_is_longlat(potamanthus_gbif_test) #this should be true to know that they extracted the correct coordinate system
st_is_valid(potamanthus_gbif_test) #this should also be true so the different data points are correct
potamanthus_gbif_convex <- st_convex_hull(st_union(potamanthus_gbif_test))
st_is_valid(potamanthus_gbif_convex, reason = T) #This is now FALSE, due to Edge crossing -> How can I fix this?
potamanthus_gbif_convex <- st_make_valid(potamanthus_gbif_convex)
st_is_valid(potamanthus_gbif_convex, reason = T) #make valid did not fix the edge crossing

test <- st_convex_hull(st_combine(potamanthus_gbif_test))
st_area(test)
plot(test, add = T)

potamanthus_tream_convex <- st_as_sf(potamanthus_tream, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)

sf::sf_use_s2(TRUE)
potamanthus_tream_convex <- st_convex_hull(st_union(potamanthus_tream_convex))

potamanthus_gbif_convex <- st_as_sf(potamanthus_gbif, coords=  c("decimalLongitude", "decimalLatitude"), crs = 4326)
potamanthus_gbif_convex <- st_make_valid(potamanthus_gbif_convex)
potamanthus_gbif_convex <- st_buffer(potamanthus_gbif_convex, 0.0)
potamanthus_gbif_convex <- st_make_valid(st_transform(potamanthus_gbif_convex, 4326))
potamanthus_gbif_convex <- st_convex_hull(st_union(potamanthus_gbif_convex))

plot(potamanthus_tream_convex, add = T, col = "blue")
plot(potamanthus_gbif_convex, add = T, col = "red")

sum(st_area(st_make_valid(potamanthus_tream_convex)))
sum(st_area(st_make_valid(potamanthus_gbif_convex)))

baetis_tream_convex <- st_as_sf(baetis_tream, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)
baetis_tream_convex <- st_make_valid(baetis_tream_convex)
sf::sf_use_s2(TRUE)
baetis_tream_convex <- st_convex_hull(st_union(baetis_tream_convex))

baetis_gbif_convex <- st_as_sf(baetis_gbif, coords=  c("decimalLongitude", "decimalLatitude"), crs = 4326)
baetis_gbif_convex <- st_make_valid(baetis_gbif_convex)
baetis_gbif_convex <- st_convex_hull(st_union(baetis_gbif_convex))

plot(baetis_tream_convex, add = T, col = "blue")
plot(baetis_gbif_convex, add = T, col = "red")

sum(st_area(st_make_valid(baetis_tream_convex)))
sum(st_area(st_make_valid(baetis_gbif_convex)))


# extract species with invalid geometry
species_invalid <- subset(species_info, species_info$MCP2 == "invalid geometry")


library(rgbif)
library(maps)

gbif <- occ_search(scientificName = "Ablabesmyia longistyla", hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 5000)
gbif <- gbif$data

tream <- subset(TREAM.sp, TREAM.sp$binomial == "Lepidostoma hirtum")
names(tream)[names(tream) == "binomial"] <- "species"
tream_convex <- st_as_sf(tream, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)

maps::map('world')
points(gbif$decimalLongitude, gbif$decimalLatitude, col='red',  pch=19)
points(tream$Longitude_X, tream$Latitude_Y, col='blue',  pch=19)
plot(tmp5k, col = "green", add = T, pch = 19)
plot(tmp10k, col = "red", add = T, pch = 19)


gbif_test <- gbif[,c("decimalLongitude", "decimalLatitude", "species")]
gbif_test <- st_as_sf(gbif_test, coords=  c("decimalLongitude", "decimalLatitude"), crs = 4326)
gbif_tream <- rbind(gbif_test, tream_convex)
st_is_longlat(gbif_tream) #this should be true to know that they extracted the correct coordinate system
st_is_valid(gbif_tream) 
gbif_convex <- st_convex_hull(st_union(gbif_tream))
st_is_valid(gbif_convex, reason = T)


tream_convex <- st_convex_hull(st_union(tream_convex))
st_is_valid(tream_convex, reason = T)


plot(gbif_convex, add = T, col = "red")
plot(tream_convex, add = T, col = "blue")

sum(st_area(st_make_valid(tream_convex)))
sum(st_area(st_make_valid(gbif_convex)))

sum(st_area(st_make_valid(tream_convex))) / sum(st_area(st_make_valid(gbif_convex)))

# valid calculations
#Ablabesmyia monilis
#Aeshna grandis
#Baetis vernus
# Cordulia aenea
# Erythromma najas
# Glossiphonia paludosa
# Gordius aquaticus
# Hemiclepsis marginata
# Labiobaetis atrebatinus
# Lepidostoma hirtum


# turn of probably needed
#Acilius canaliculatus
# Hygrotus inaequalis

# weird results
#Baetis fuscatus
#Ceraclea albimacula
# Hydraena riparia

load("data/species_info_incomplete_sf.Rdata")
load("data/TREAM_sf.Rdata")

# add range size to the correct species if a file is there (if a range was calculated)
for (i in 1:nrow(species_info)) {
  sp_name <- unique(species_info$binomial)[i]
  print(sp_name)
  gbif <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 5000)
  gbif <- gbif$data
  
  tream <- subset(TREAM.sp, TREAM.sp$binomial == sp_name)
  names(tream)[names(tream) == "binomial"] <- "species"
  tream <- st_as_sf(tream, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)
  
  maps::map('world')
  points(gbif$decimalLongitude, gbif$decimalLatitude, col='red',  pch=19)
  points(tream$Longitude_X, tream$Latitude_Y, col='blue',  pch=19)
  plot(tmp.sf, col = "green", add = T, pch = 19)
  
  
  gbif_test <- gbif[,c("decimalLongitude", "decimalLatitude", "species")]
  gbif_test <- st_as_sf(gbif_test, coords=  c("decimalLongitude", "decimalLatitude"), crs = 4326)
  gbif_tream <- rbind(gbif_test, tream_convex)
  st_is_longlat(gbif_tream) #this should be true to know that they extracted the correct coordinate system
  st_is_valid(gbif_tream) 
  gbif_convex <- st_convex_hull(st_union(gbif_tream))
  st_is_valid(gbif_convex, reason = T)
  
  
  tream_convex <- st_convex_hull(st_union(tream_convex))
  st_is_valid(tream_convex, reason = T)
  
  
  plot(gbif_convex, add = T, col = "red")
  plot(tream_convex, add = T, col = "blue")
  
  sum(st_area(st_make_valid(tream_convex)))
  sum(st_area(st_make_valid(gbif_convex)))
  
  sum(st_area(st_make_valid(tream_convex))) / sum(st_area(st_make_valid(gbif_convex)))
}

