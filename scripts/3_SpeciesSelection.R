# Work on the selection of species for inclusion

# Plan:

#1. remove all data before year 1990 and remove study sites with less than 20 years of sampling 
#2. Classify the taxonLevel for each , i.e. which of these are species and which are genera or families. (This step can be excluded as we currently only consider species level)
#3. Calculate the number of sites on which each of these taxa is found
#4. Calculate the equivalent number of sites in the full dataset
#5. With the sub data set calculate a simple trend in abundance as log(N) ~ factor(site) + year
#6. Calculate range extent as the Minimum Convex Polygon for each taxon using all the sites in the survey dataset (not just the 202 good sites)
#7. Download records from GBIF for each taxon. Add the GBIF records to the site locations for each species and calculate a new MCP
#8. Prepare a dataframe with columns: name, taxonLevel, nsite1, nsite2, trend, MCP1, MCP2 and taxonomic levels

# Load packages
library(dplyr)
library(sp)
library(rgbif)
library(sf)

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

# subset based on threshold of 20 years of sampling per study site
TREAM_sub <- subset(TREAM, TREAM$years_studysite >= 20)

# Extract information about number of species and study sites
length(unique(TREAM_sub$binomial)) #762
length(unique(TREAM_sub$site_id)) #202

# save the smaller data set with the "good" data
write.csv(TREAM_sub, "data/TREAM_gooddata.csv", row.names = F)

#3. Calculate the number of sites on which each of these taxa is found ------------
species_info <- TREAM_sub %>% group_by(binomial) %>% summarise(nsite1 = n_distinct(site_id))

#4. Calculate the equivalent number of sites in the full dataset -----------

TREAM_sub2 <- subset(TREAM, TREAM$binomial %in% unique(species_info$binomial))

species_info <- full_join(species_info, TREAM_sub2 %>% group_by(binomial) %>% summarise(nsite2 = n_distinct(site_id)), by = "binomial")

#5. With the sub data set calculate a simple trend in abundance as log(N) ~ factor(site) + year -----------

# Add column to data set with all information
species_info$trend <- NA
species_info$p.value <- NA
species_info$std.error <- NA

# loop for every species
for (i in 1:length(unique(species_info$binomial))) {
  tmp <-  subset(TREAM_sub, TREAM_sub$binomial == unique(species_info$binomial)[i]) #extract data of single species
  
  # use a different model when only data for one study site is available (linear model can't calculate regression over one level)
  if(length(unique(tmp$site_id)) > 1){
  model <- lm(log(abundance + 1) ~ factor(site_id) + year, data = tmp)
  species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"trend"] <- model[["coefficients"]][["year"]]
  species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"p.value"] <- summary(model)$coefficients[, 4]["year"] #overall_p(model)
  species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"std.error"] <- summary(model)$coefficients[, 2]["year"] #summary(model)$sigma #summary(model)$coefficients[, 2]
  #species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"R2"] <- summary(model)$r.squared
  } else {
    model <- lm(log(abundance + 1) ~ year, data = tmp)
    species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"trend"] <- model[["coefficients"]][["year"]]
    species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"p.value"] <- summary(model)$coefficients[, 4]["year"]# overall_p(model)
    species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"std.error"] <- summary(model)$coefficients[, 2]["year"] #summary(model)$sigma #summary(model)$coefficients[, 2]
    #species_info[which(species_info$binomial == unique(species_info$binomial)[i]),"R2"] <- summary(model)$r.squared
  }
}

#6. Calculate range extent as the Minimum Convex Polygon for each taxon using all the sites in the survey dataset (not just the 202 good sites) ----------

#add GPS data of study sites
site_GPS <- read.csv("data/TREAM/TREAM_siteLevel.csv")
TREAM <- merge(TREAM, site_GPS[,c("site_id", "Longitude_X", "Latitude_Y")], by = "site_id")

#Extract those species with more than two different study sites in the large data set (MCP can't be calculated for only two locations)
species_reduced <- species_info[which(species_info$nsite2 > 2),]
TREAM_red_species <- subset(TREAM, TREAM$binomial %in% species_reduced$binomial)

# Calculate the MCP (using the sf package as Katrin did for the BBS data)

#extract species and coordinates
TREAM.sp <- TREAM_red_species[,c("binomial", "Longitude_X", "Latitude_Y")]

save(TREAM.sp, file = "data/TREAM_gooddata_spatial.Rdata")

# transform into sf object
TREAM.sf <- st_as_sf(TREAM.sp, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)
TREAM.sf <- st_make_valid(TREAM.sf)

#extract species (only keep species with more than two distinct locations), obtain convex hull and calculate area
for (i in 1:nrow(species_info)) {
  tmp <- subset(TREAM.sf, TREAM.sf$binomial == unique(species_info$binomial)[i])
  if(nrow(tmp) > 2){
    sf::sf_use_s2(TRUE)
    tmp <- st_convex_hull(st_union(tmp))
    range_coverage_df <- sum(st_area(st_make_valid(tmp)))
    species_info[which(species_info$binomial == unique(species_info$binomial)[i]), "MCP1"] <- as.numeric(range_coverage_df)
  }
}

save(species_info, file = "data/species_info_incomplete.Rdata")

# #7. Download records from GBIF for each taxon. Add the GBIF records to the site locations for each species and calculate a new MCP
# #This step is executed in a different script to be run on the cluster to download more occurrences
# 
# # add new column
# species_info$MCP2 <- NA
# 
# # For each species download observations from GBIF, extract the coordinates and calculate MCP for species with more than three different locations
# for (i in 1:length(unique(species_info$binomial))) {
#   sp_name <- unique(species_info$binomial)[i]
#   tmp <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 1000)
#   tmp <- tmp$data
#   
#   #only continue with the species that have more than two different locations
#   if(length(unique(test$decimalLatitude)) > 2){
#     
#     #extract species and coordinates
#     tmp.sp <- tmp[,c("species", "decimalLatitude", "decimalLongitude")]
#     
#     # transform into sf object
#     test.sf <- st_as_sf(test.sp, coords=  c("decimalLatitude", "decimalLongitude"), crs = 4326)
#     test.sf <- st_make_valid(test.sf)
#     
#     # obtain convex hull and calculte range size
#     test.sf <- st_convex_hull(st_union(test.sf))
#     range_coverage_df <- sum(st_area(st_make_valid(test.sf)))
#     species_info[which(species_info$binomial == unique(species_info$binomial)[i]), "MCP2"] <- as.numeric(range_coverage_df)
#   }
# }
# 
# #for (i in 1:length(unique(species_info$binomial))) {
# tmp <- subset(TREAM.sf, TREAM.sf$binomial == unique(species_notworked$binomial)[11])
# # rename column
# names(tmp)[names(tmp) == "binomial"] <- "species"
# if(nrow(tmp) > 2){
#   sp_name <- unique(species_notworked$binomial)[11]
#   tmp_gbif <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 100)
#   tmp_gbif <- tmp_gbif$data
#   #only continue with the species that have more than two different locations
#   if(length(unique(tmp_gbif$decimalLatitude)) > 2){
#     
#     #extract species and coordinates
#     tmp_gbif.sp <- tmp_gbif[,c("species", "decimalLatitude", "decimalLongitude")]
#     
#     # transform into sf object
#     tmp_gbif.sf <- st_as_sf(tmp_gbif.sp, coords=  c("decimalLatitude", "decimalLongitude"), crs = 4326)
#     
#     # add data occurrences and gbif data together
#     tmp.sf <- rbind(tmp_gbif.sf, tmp)
#     tmp.sf <- st_make_valid(tmp.sf)
#     tmp.sf <- st_convex_hull(st_union(tmp.sf))
#     #save(test.sf, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/convex_hull_", sp_name, ".Rdata"))
#     #sf::sf_use_s2(FALSE)
#     range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
#     #species_info[which(species_info$binomial == unique(species_info$binomial)[i]), "MCP2"] <- as.numeric(range_coverage_df)
#     save(range_coverage_df, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/MCP2_", sp_name, ".Rdata"))
#   }
# }
# 
# #save data set
# #write.csv(species_info, file = "data/species_information_TREAM_sf.csv", row.names = F)
