# Work on the selection of species for inclusion

# Plan:

#1. remove all data before year 1990 and remove study sites with less than 20 years of sampling 
#2. Harmonise sampling units to Ind/m^2
#3. Calculate the number of sites on which each of these taxa is found
#4. Calculate the equivalent number of sites in the full dataset
#5. Calculate range extent as the Minimum Convex Polygon for each taxon using all the sites in the survey dataset (not just the 202 good sites)
#6. Download records from GBIF for each taxon. Add the GBIF records to the site locations for each species and calculate a new MCP
#7. Pre select the species (non-flying insects)
#8. Classify the range extent in small, medium, large
#9. Apply second filtering step (have to be present on more than 10 sites and cover 25% of the global range)
#10. With the sub data set calculate a simple trend in abundance as log(N) ~ factor(site) + year

# Load packages
library(dplyr)
library(sp)
library(rgbif)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)

# Load data
TREAM <- read.csv("data/TREAM_zeros.csv")
site_GPS <- read.csv("data/TREAM/TREAM_siteLevel.csv") # GPS coordinates from the study site

# add GPS locations & sampling unit to the data
TREAM <- merge(TREAM, site_GPS[,c("site_id", "Longitude_X", "Latitude_Y", "unit")], by = "site_id")

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

#2. Harmonise sampling units to Ind/m^2
unique(TREAM_sub$unit)
# set new columns
TREAM_sub$abundance_new <- NA
TREAM_sub$unit_new <- NA

# bring all sampling units to Ind/m^2 or Ind/sample
for (i in 1:nrow(TREAM_sub)) {
  if(TREAM_sub$unit[i] == "Ind/0.12 m^2"){
    if(TREAM_sub$abundance[i] > 0){
      TREAM_sub$abundance_new[i] <- TREAM_sub$abundance[i] * 8.4
      TREAM_sub$unit_new[i] <- "Ind/m^2"
    } else {
      TREAM_sub$abundance_new[i] <- 0
      TREAM_sub$unit_new[i] <- "Ind/m^2"
    }
  } else if(TREAM_sub$unit[i] == "Ind/1.25 m^2"){
    if(TREAM_sub$abundance[i] > 0){
      TREAM_sub$abundance_new[i] <- TREAM_sub$abundance[i] * 0.8
      TREAM_sub$unit_new[i] <- "Ind/m^2"
    } else {
      TREAM_sub$abundance_new[i] <- 0
      TREAM_sub$unit_new[i] <- "Ind/m^2"
    }
  } else if(TREAM_sub$unit[i] == "composite of 12 samples totaling Ind/0.6 m^2"){
    if(TREAM_sub$abundance[i] > 0){
      TREAM_sub$abundance_new[i] <- TREAM_sub$abundance[i] * 1.7
      TREAM_sub$unit_new[i] <- "Ind/m^2"
    } else {
      TREAM_sub$abundance_new[i] <- 0
      TREAM_sub$unit_new[i] <- "Ind/m^2"
    }
  } else if(TREAM_sub$unit[i] == "Ind/ 5 subsamples"){
    if(TREAM_sub$abundance[i] > 0){
      TREAM_sub$abundance_new[i] <- TREAM_sub$abundance[i] / 5
      TREAM_sub$unit_new[i] <- "Ind/sample"
    } else {
      TREAM_sub$abundance_new[i] <- 0
      TREAM_sub$unit_new[i] <- "Ind/sample"
    }
  } else if(TREAM_sub$unit[i] == "Ind/sample"){
    TREAM_sub$abundance_new[i] <- TREAM_sub$abundance[i]
    TREAM_sub$unit_new[i] <- "Ind/sample"
  } else if(TREAM_sub$unit[i] == "Ind/m^2"){
    TREAM_sub$abundance_new[i] <- TREAM_sub$abundance[i]
    TREAM_sub$unit_new[i] <- "Ind/m^2"
  }
}

unique(TREAM_sub$unit_new)

# harmonize both metrics together
TREAM_X1 <- subset(TREAM_sub, TREAM_sub$unit_new == "Ind/m^2")
TREAM_X2 <- subset(TREAM_sub, TREAM_sub$unit_new == "Ind/sample")

TREAM_X2$abundance_new <- TREAM_X2$abundance_new * (mean(TREAM_X1$abundance_new)/ mean(TREAM_X2$abundance_new))
TREAM_X2$unit_new <- "Ind/m^2"

# remerge datasets
TREAM_sub <- rbind(TREAM_X1, TREAM_X2)

# save data set
write.csv(TREAM_sub, "data/TREAM_gooddata_harmonised_unit.csv", row.names = F)

TREAM_sub <- read.csv("data/TREAM_gooddata_harmonised_unit.csv", header = T)

#3. Calculate the number of sites on which each of these taxa is found ------------
species_info <- TREAM_sub %>% group_by(binomial) %>% summarise(nsite1 = n_distinct(site_id))

#4. Calculate the equivalent number of sites in the full dataset -----------

TREAM_sub2 <- subset(TREAM, TREAM$binomial %in% unique(species_info$binomial))

species_info <- full_join(species_info, TREAM_sub2 %>% group_by(binomial) %>% summarise(nsite2 = n_distinct(site_id)), by = "binomial")

#5. Calculate range extent as the Minimum Convex Polygon for each taxon using all the sites in the survey dataset (not just the 202 good sites) ----------

#Extract those species with more than two different study sites in the large data set (MCP can't be calculated for only two locations)
species_reduced <- species_info[which(species_info$nsite2 > 2),]
TREAM_red_species <- subset(TREAM, TREAM$binomial %in% species_reduced$binomial)

# Calculate the MCP (using the sf package as Katrin did for the BBS data)

#extract species and coordinates
TREAM.sp <- TREAM_red_species[,c("binomial", "Longitude_X", "Latitude_Y")]

#save(TREAM.sp, file = "data/TREAM_gooddata_spatial.Rdata")

# transform into sf object
TREAM.sf <- st_as_sf(TREAM.sp, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)
TREAM.sf <- st_make_valid(TREAM.sf)

#extract species (only keep species with more than two distinct locations), obtain convex hull and calculate area
for (i in 1:nrow(species_info)) {
  tmp <- subset(TREAM.sf, TREAM.sf$binomial == unique(species_info$binomial)[i])
  print(nrow(tmp))
  if(nrow(tmp) > 2){
    tmp <- st_convex_hull(st_union(tmp))
    range_coverage_df <- sum(st_area(st_make_valid(tmp)))
    species_info[which(species_info$binomial == unique(species_info$binomial)[i]), "MCP1"] <- as.numeric(range_coverage_df)
  }
  rm(tmp)
}

# add taxonKey to later download the Gbif occurrences
species_info$taxonKey <- NA

for (i in 1:nrow(species_info)) {
  tmp <- name_backbone(species_info$binomial[i])
  if("usageKey" %in% names(tmp)){
    species_info$taxonKey[i] <- tmp$usageKey
  } else {
    species_info$taxonKey[i] <- NA
  }
  rm(tmp)
}

#save(species_info, file = "data/species_info_incomplete.Rdata")

# #6. Download records from GBIF for each taxon. Add the GBIF records to the site locations for each species and calculate a new MCP -----

# remove all species with NA in taxonKey
# species_gbif <- species_info[which(!is.na(species_info$taxonKey)),]
# 
# occ_download(
#   pred_in("taxonKey", species_gbif$taxonKey),
#   pred("hasCoordinate", TRUE),
#   pred("occurrenceStatus","PRESENT"),
#   pred_in("basisOfRecord", c('HUMAN_OBSERVATION', "MACHINE_OBSERVATION", "OBSERVATION", "OCCURRENCE")),
#   format = "SIMPLE_CSV"
# )
# 
# occ_download_wait('0049729-251009101135966')

# # save the occurrence data
# occ <- occ_download_get('0049729-251009101135966') %>%
#   occ_download_import()
# 
# saveRDS(occ,file = "data/gbif_occurrences.rds")
# 
# occ <- readRDS("data/gbif_occurrences.rds")
# 
# # clean the occurrences a bit
# occ_cleaned <- occ %>% dplyr::filter(!(is.na(decimalLatitude) | is.na(decimalLongitude)),           # only records with coords
#                                      !(decimalLatitude == decimalLongitude | decimalLatitude == 0 | decimalLongitude == 0),  # coords should not be equal
#                                      !(year < 1900 | year > 2025))
# 
# # Store the cleaned point locations
# #occ_cleaned <- occ[occ_cleaned$.summary,]
# 
# # Remove columns which contain additional information
# occ_cleaned <- occ_cleaned %>% select(-one_of(c("gbifID", "datasetKey", "occurrenceID", "kingdom", "phylum", "infraspecificEpithet", "taxonRank", 
#                                                 "verbatimScientificNameAuthorship", "publishingOrgKey", "issue", "occurrenceStatus", "individualCount", "collectionCode", 
#                                                 "identifiedBy", "dateIdentified", "license", "rightsHolder", "recordedBy", "typeStatus", "establishmentMeans", "lastInterpreted", 
#                                                 "mediaType")))
# 
# # Save data set
# saveRDS(occ_cleaned, file = "data/gbif_occurrences_cleaned.rds")

#GBIF Occurrence Download https://www.gbif.org/occurrence/download/0049729-251009101135966 Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2025-10-22

# Calculate the MCP for every single species using the GBIF occurrence points
occ_gbif <- readRDS("data/gbif_occurrences_cleaned.rds")
#load("data/species_info_incomplete.Rdata")
load("data/TREAM_gooddata_spatial.Rdata")
species_info$MCP2 <- NA

#extract species and coordinates
occ_gbif.sp <- occ_gbif[,c("species", "decimalLongitude", "decimalLatitude")]

for (i in 1:nrow(species_info)) {
  #extract species (only keep species with more than two distinct locations), obtain convex hull and calculate area
  tmp <- subset(occ_gbif.sp, occ_gbif.sp$species == unique(species_info$binomial)[i])
  tmp_TREAM <- subset(TREAM.sp, TREAM.sp$binomial == unique(species_info$binomial)[i])
  
  #transform into sf object
  tmp_TREAM.sf <- st_as_sf(tmp_TREAM, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)
  tmp_TREAM.sf <- st_make_valid(tmp_TREAM.sf)
  
  # rename column
  names(tmp_TREAM.sf)[names(tmp_TREAM.sf) == "binomial"] <- "species"
  
  #Transform GBIF data into sf file if occurrences are present
  if(length(unique(tmp$decimalLatitude)) >= 1){
    
    # transform into sf object
    tmp_gbif.sf <- st_as_sf(tmp, coords=  c("decimalLongitude", "decimalLatitude"), crs = 4326)
    tmp_gbif.sf <- st_make_valid(tmp_gbif.sf)
    
    # add data occurrences and gbif data together
    tmp.sf <- rbind(tmp_gbif.sf, tmp_TREAM.sf)
    tmp.sf <- st_make_valid(tmp.sf)
  } else {
    # if no occurrences for the species is available, only use the TREAM occurrences
    tmp.sf <- st_make_valid(tmp_TREAM.sf)
  }
  if(nrow(tmp) > 2){
    tmp.sf <- st_convex_hull(st_union(tmp.sf))
    range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
    species_info[which(species_info$binomial == unique(species_info$binomial)[i]), "MCP2"] <- as.numeric(range_coverage_df)
  }
  rm(tmp)
}

# add taxonomic information as well as 
# load in large data set to extract the taxonomic information of it
TREAM <- read.csv("data/TREAM_zeros.csv")

TREAM <- subset(TREAM, select = c("order", "family", "binomial"))
TREAM <- TREAM[which(!is.na(TREAM$binomial)),]
TREAM_taxonomic <- distinct(TREAM)

# merge the data set by the binomial column
species_info <- merge(species_info, TREAM_taxonomic, by = "binomial")

# change 0 values to NA (data set was not available for the 0s meaning we don't have any information)
species_info[which(species_info$MCP2 == 0), "MCP2"] <- NA

# calculate percentage of global range covered by our data
species_info$perc_range_coverage <- round((species_info$MCP1/species_info$MCP2)*100,2)


# save the data
write.csv(species_info, "data/species_information_TREAM_before_selection.csv", row.names = F)

species_info <- read.csv("data/species_information_TREAM_before_selection.csv", header = T)

#7 Pre-select the species
# Removing non-insect species, 

insectGroups <- c("Coleoptera",
                  "Diptera",
                  "Ephemeroptera",
                  "Heteroptera" ,
                  "Megaloptera",
                  "Plecoptera", 
                  "Trichoptera")

species_info_final <- subset(species_info, order %in% insectGroups)

# 8. Calcualte the range classes
species_info_final <- species_info_final %>% mutate(range_class = factor(dplyr::ntile(MCP2, 3),
                              
                              levels = 1:3,
                              
                              labels = c("small", "medium", "large")))


# 9. Apply second filtering step

# be present on more than 10 sites and cover over 25% of the global range
species_info_final <- subset(species_info_final, nsite1 >= 10 & perc_range_coverage >= 25)

#10. With the sub data set calculate a simple trend in abundance as log(N) ~ year -----------

# Add column to data set with all information
species_info_final$trend <- NA
species_info_final$p.value <- NA
species_info_final$std.error <- NA

# loop for every species
pdf("plots/abundance_trends_TREAM_jitter_lm_25percent.pdf")
for (i in 1:length(unique(species_info_final$binomial))) {
  tmp <-  subset(TREAM_sub, TREAM_sub$binomial == unique(species_info_final$binomial)[i]) #extract data of single species
  
  # use a different model when only data for one study site is available (linear model can't calculate regression over one level)
  model <- lm(log(abundance_new + 1) ~ year, data = tmp)
  species_info_final[which(species_info_final$binomial == unique(species_info_final$binomial)[i]),"trend"] <- model[["coefficients"]][["year"]]
  species_info_final[which(species_info_final$binomial == unique(species_info_final$binomial)[i]),"p.value"] <- summary(model)$coefficients[, 4]["year"] #overall_p(model)
  species_info_final[which(species_info_final$binomial == unique(species_info_final$binomial)[i]),"std.error"] <- summary(model)$coefficients[, 2]["year"] #summary(model)$sigma #summary(model)$coefficients[, 2]
  
  # Plot the abundance trend calculations
  plot(log(abundance_new + 1) ~ jitter(year), data = tmp, main = unique(tmp$binomial))
  abline(lm(log(abundance_new + 1) ~ year, data = tmp), col = "red", lwd = 2)
}
dev.off()

# Plot the map of data distribution
# Download data to world extent
world_map <- ne_countries(scale = 50, returnclass = 'sf')

# extract European countries
europeanUnion <- c("Austria", "Andorra", "Belgium","Bulgaria","Croatia","Cyprus",
                   "Czechia","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands", "Norway", "Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden", "Switzerland", "United Kingdom")

europe_map <- world_map %>% filter(name %in% europeanUnion)

# Separate MultiPolygons into single polygons to remove islands I am not interested in
europe_single_polygons <- st_cast(europe_map, "POLYGON")

# vectorize map of europe
europe_vect <- vect(europe_single_polygons)

# crop to smaller extent to extract islands
new_extent <- ext(-10, 34, 28, 70.06484375)
europe <- crop(europe_vect, new_extent)

pdf("plots/map_TREAM_points.pdf")
for (i in 1:length(unique(species_info_final$binomial))) {
  tmp <-  subset(TREAM.sp, TREAM.sp$binomial == unique(species_info_final$binomial)[i]) #extract data of single species
  
  # plot the occurrences in europe
  plot(europe, col = 'lightgrey', main = unique(tmp$"binomial"))
  points(tmp$Longitude_X, tmp$Latitude_Y, col='red',  pch=19)
}
dev.off

# save data set
write.csv(species_info_final, "data/species_information_TREAM_lm.csv", row.names = F)

# nyrs = 2020 - 1990
# 
# species_info_final <- species_info_final %>%
#   
#   tidyr::drop_na() %>%
#   
#   mutate(perc_trend = 100 * (exp(trend * nyrs) - 1))
# 
# 
# species_info_final <- species_info_final %>% mutate(trend_class = case_when(is.na(p.value) | is.na(perc_trend)            ~ NA_character_,
#                                                                             
#                                                                             perc_trend >=  5            ~ "increasing",
#                                                                             
#                                                                             perc_trend <= -5            ~ "declining",
#                                                                             
#                                                                             abs(perc_trend) < 5 ~ "stable",
#                                                                             
#                                                                             TRUE ~ "other"))
# 
# table(species_info_final$trend_class, species_info_final$range_class)
