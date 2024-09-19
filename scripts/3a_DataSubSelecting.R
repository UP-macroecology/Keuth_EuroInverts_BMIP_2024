# Work on the selection of species for inclusion

#1. remove all data before year 1990 and remove study sites with less than 20 years of sampling 
#2. Classify the taxonLevel for each , i.e. which of these are species and which are genera or families.
#3. Calculate the number of sites on which each of these taxa is found
#4. Calculate the equivalent number of sites in the full dataset
#5. With the sub data set calculate a simple trend in abundance as log(N) ~ factor(site) + year
#6. Calculate range extent as the Minimum Convex Polygon for each taxon using all the sites in the survey dataset (not just the 202 good sites)
#7. Download records from GBIF for each taxon. Add the GBIF records to the site locations for each species and calculate a new MCP
#8. Prepare a dataframe with columns: name, taxonLevel, nsite1, nsite2, trend, MCP1, MCP2.

# Load packages
library(dplyr)
library(adehabitatHR)
library(sp)
library(rgbif)

# Load data
TREAM <- read.csv("data/TREAM_zeros.csv")

# remove a mistake that happened in the binomial column for some reason
TREAM[which(TREAM$binomial == "NA NA"), "binomial"] <- NA

#1. remove all data before year 1990 and remove study sites with less than 20 years of sampling ----------
TREAM <- subset(TREAM, TREAM$year >= 1990)

# only use the data with species level identification
TREAM <- subset(TREAM, !is.na(TREAM$binomial))

# Obtain number of years per study site
no_years_studysites <- TREAM %>% group_by(site_id) %>% summarise(years_studysite = n_distinct(year))

# merge information with large dataset
TREAM <- TREAM %>% full_join(no_years_studysites, by = join_by(site_id))

# subset based on threshold of 20 years
TREAM_sub <- subset(TREAM, TREAM$years_studysite >= 20)

# Extract information about number of species, genus, family
length(unique(TREAM_sub$binomial)) #762
length(unique(TREAM_sub$site_id)) #202

#3. Calculate the number of sites on which each of these taxa is found
species_info <- TREAM_sub %>% group_by(binomial) %>% summarise(nsite1 = n_distinct(site_id))

TREAM_sub2 <- subset(TREAM, TREAM$binomial %in% unique(species_info$binomial))

species_info <- full_join(species_info, TREAM_sub2 %>% group_by(binomial) %>% summarise(nsite2 = n_distinct(site_id)), by = "binomial")

#5. With the sub data set calculate a simple trend in abundance as log(N) ~ factor(site) + year

species_info$trend <- NA
for (i in 1:length(unique(species_info$binomial))) {
  tmp <-  subset(TREAM_sub, TREAM_sub$binomial == unique(species_info$binomial)[i])
  if(length(unique(tmp$site_id)) > 1){
  model <- lm(log(abundance + 1) ~ factor(site_id) + year, data = tmp)
  species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"trend"] <- model[["coefficients"]][["year"]]
  } else {
    model <- lm(log(abundance + 1) ~ year, data = tmp)
    species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"trend"] <- model[["coefficients"]][["year"]]
  }
}

# test <- subset(TREAM_sub, TREAM_sub$binomial == "Acilius canaliculatus")
# model1 <- lm(log(abundance + 1) ~ factor(site_id) + year, data = test)
# model1 <- lm(log(abundance + 1) ~ year, data = test)
# 
# plot(test$abundance ~ test$year)
# abline(lm(test$abundance ~ test$year))
# 
# plot(log(test$abundance +1) ~ test$year)
# abline(lm(log(test$abundance +1) ~ test$year))

#6. Calculate range extent as the Minimum Convex Polygon for each taxon using all the sites in the survey dataset (not just the 202 good sites)
site_GPS <- read.csv("data/TREAM/TREAM_siteLevel.csv")

# only keep species that are found on more than two study sites in the large data set
species_reduced <- species_info[which(species_info$nsite2 > 2),]
TREAM_sub3 <- subset(TREAM, TREAM$binomial %in% species_reduced$binomial)

# add GPS data
TREAM_sub3 <- merge(TREAM_sub3, site_GPS[,c("site_id", "Longitude_X", "Latitude_Y")], by = "site_id")

#Calculate MCP

#extract species and coordinates
TREAM.sp <- TREAM_sub3[,c("binomial", "Longitude_X", "Latitude_Y")]
#TREAM.sp <- TREAM.sp[which(!is.na(TREAM$binomial)),]

#TREAM.sp %>% group_by(binomial) %>% summarise(n_long = n_distinct(Longitude_X), n_lat = n_distinct(Latitude_Y))

# TREAM.sf <- st_as_sf(TREAM.sp, coords=  c("Longitude_X", "Latitude_Y"))
# range_coverage_df <- sum(st_area(st_make_valid(TREAM.sf)))

# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(TREAM.sp) <- c("Longitude_X", "Latitude_Y")
proj4string(TREAM.sp) <- CRS("+init=epsg:4326") 

#add coordinate system
#TREAM.sp <- spTransform(TREAM.sp, CRS("+proj=utm zone=33 +datum=WGS84 +units=m +no_defs"))

TREAM.mcp <- mcp(TREAM.sp, percent = 100)
MCP <- as.data.frame(TREAM.mcp)

# join the data 

species_info <- full_join(species_info, MCP, by = join_by("binomial" == "id"))

# rename column
names(species_info)[names(species_info) == "area"] <- "MCP1"
# library(maps)
# maps::map('world',xlim=c(-20,40), ylim=c(30,80))
# plot(TREAM.sp, col = "red", add = T)
# plot(TREAM.mcp)
# 
# library(scales) # Helps make polygons partly transparent using the alpha argument below
# plot(TREAM.sp, col = as.factor(TREAM.sp@data$binomial), pch = 16)
# plot(TREAM.mcp, col = alpha(1:5, 0.5), add = TRUE)

#7. Download records from GBIF for each taxon. Add the GBIF records to the site locations for each species and calculate a new MCP
species_info$MCP2 <- NA

for (i in 1:length(unique(species_info$binomial))) {
  sp_name <- unique(species_info$binomial)[i]
  test <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 10000)
  test <- test$data

  #removes data with less than two distinct sites
  if(length(unique(test$decimalLatitude)) > 3){
  #extract species and coordinates
  test.sp <- test[,c("species", "decimalLatitude", "decimalLongitude")]

  # Create a SpatialPointsDataFrame by defining the coordinates
  coordinates(test.sp) <- c("decimalLatitude", "decimalLongitude")

  #add coordinate system
  proj4string(TREAM.sp) <- CRS("+init=epsg:4326") 

  test.mcp <- mcp(test.sp, percent = 100)
  MCP <- as.data.frame(test.mcp)
  
  species_info[which(species_info$binomial == sp_name), "MCP2"] <- MCP[1,2]
  }

}

write.csv(species_info, file = "data/species_information_TREAM.csv", row.names = F)

# add the MCP in ha
TREAM.sp <- TREAM_sub3[,c("binomial", "Longitude_X", "Latitude_Y")]
#TREAM.sp <- TREAM.sp[which(!is.na(TREAM$binomial)),]

#TREAM.sp %>% group_by(binomial) %>% summarise(n_long = n_distinct(Longitude_X), n_lat = n_distinct(Latitude_Y))

# TREAM.sf <- st_as_sf(TREAM.sp, coords=  c("Longitude_X", "Latitude_Y"))
# range_coverage_df <- sum(st_area(st_make_valid(TREAM.sf)))

# Create a SpatialPointsDataFrame by defining the coordinates
coordinates(TREAM.sp) <- c("Longitude_X", "Latitude_Y")
proj4string(TREAM.sp) <- CRS("+init=epsg:4326") 

#add coordinate system
TREAM.sp <- spTransform(TREAM.sp, CRS("+proj=utm zone=33 +datum=WGS84 +units=m +no_defs"))

TREAM.mcp <- mcp(TREAM.sp, percent = 100)
MCP <- as.data.frame(TREAM.mcp)

# join the data 

species_info <- full_join(species_info, MCP, by = join_by("binomial" == "id"))

# rename column
names(species_info)[names(species_info) == "area"] <- "MCP1_ha"

test <- occ_search(scientificName = "Micropsectra bidentata", hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 10000)
test <- test$data

length(test)


# Test second approach for calculating area
#Calculate MCP

#extract species and coordinates
TREAM.sp <- TREAM_sub3[,c("binomial", "Longitude_X", "Latitude_Y")]

TREAM.sf <- st_as_sf(TREAM.sp, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)
TREAM.sf <- st_make_valid(TREAM.sf)

for (i in 3) {
  tmp <- subset(TREAM.sf, TREAM.sf$binomial == unique(species_info$binomial)[i])
  range_coverage_df <- sum(st_area(st_make_valid(tmp)))
  
}

st_area(tmp)

test2 <- st_as_sf(test, coords = c("decimalLongitude", "decimalLatitude"), crs= 4326)
st_area(test2)
# range_coverage_df <- sum(st_area(st_make_valid(TREAM.sf)))

#add coordinate system
#TREAM.sp <- spTransform(TREAM.sp, CRS("+proj=utm zone=33 +datum=WGS84 +units=m +no_defs"))

TREAM.mcp <- mcp(TREAM.sp, percent = 100)
MCP <- as.data.frame(TREAM.mcp)

# join the data 

species_info <- full_join(species_info, MCP, by = join_by("binomial" == "id"))

# rename column
names(species_info)[names(species_info) == "area"] <- "MCP1"
# library(maps)
# maps::map('world',xlim=c(-20,40), ylim=c(30,80))
# plot(TREAM.sp, col = "red", add = T)
# plot(TREAM.mcp)
# 
# library(scales) # Helps make polygons partly transparent using the alpha argument below
# plot(TREAM.sp, col = as.factor(TREAM.sp@data$binomial), pch = 16)
# plot(TREAM.mcp, col = alpha(1:5, 0.5), add = TRUE)

#7. Download records from GBIF for each taxon. Add the GBIF records to the site locations for each species and calculate a new MCP
species_info$MCP2 <- NA

for (i in 1:length(unique(species_info$binomial))) {
  sp_name <- unique(species_info$binomial)[i]
  test <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 1000)
  test <- test$data
  
  #removes data with less than two distinct sites
  if(length(unique(test$decimalLatitude)) > 3){
    #extract species and coordinates
    test.sp <- test[,c("species", "decimalLatitude", "decimalLongitude")]
    
    # Create a SpatialPointsDataFrame by defining the coordinates
    coordinates(test.sp) <- c("decimalLatitude", "decimalLongitude")
    
    #add coordinate system
    proj4string(TREAM.sp) <- CRS("+init=epsg:4326") 
    
    test.mcp <- mcp(test.sp, percent = 100)
    MCP <- as.data.frame(test.mcp)
    
    species_info[which(species_info$binomial == sp_name), "MCP2"] <- MCP[1,2]
  }
  
}