# GBIF Information cluster script

# Load packages
library(foreach)
library(doParallel)

load("/import/ecoc9z/data-zurell/keuth/Work/species_info_incomplete.Rdata")
load("/import/ecoc9z/data-zurell/keuth/Work/TREAM_gooddata_spatial.Rdata")

#set up cluster
ncores <- 40
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(i=1:length(species_info), .packages = c("dplyr", "sp", "rgbif", "sf", "maps")) %dopar% {
  # Extract species name of the loop
  sp_name <- unique(species_info$binomial)[i]
  
  # transform TREAM spatial data into sf object
  TREAM.sf <- st_as_sf(TREAM.sp, coords=  c("Longitude_X", "Latitude_Y"), crs = 4326)
  TREAM.sf <- st_make_valid(TREAM.sf)
  
  #Extract species from TREAM data set
  tmp_TREAM.sf <- subset(TREAM.sf, TREAM.sf$binomial == sp_name)

  # rename column
  names(tmp_TREAM.sf)[names(tmp_TREAM.sf) == "binomial"] <- "species"
  
  # Download GBIF data
  tmp_gbif <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 5000)
  tmp_gbif <- tmp_gbif$data
  
  #Transform GBIF data into sf file if occurrences are present
  if(length(unique(tmp_gbif$decimalLatitude)) >= 1){
    #extract species and coordinates
    tmp_gbif.sp <- tmp_gbif[,c("species", "decimalLatitude", "decimalLongitude")]
    
    # transform into sf object
    tmp_gbif.sf <- st_as_sf(tmp_gbif.sp, coords=  c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # add data occurrences and gbif data together
    tmp.sf <- rbind(tmp_gbif.sf, tmp_TREAM.sf)

    tmp.sf <- st_make_valid(tmp.sf)
  } else {
    # if no occurrences for the species is available, only use the TREAM occurrences
    tmp.sf <- st_make_valid(tmp_TREAM.sf)
  }
  if(nrow(tmp.sf) > 2){
    # save the data set only if there are enough occurrences to calculate the MCP
    save(tmp.sf, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/data/spatial_data_TREAM_GBIF_", sp_name, ".Rdata"))
    
    # plots to control how the occurrences are distributed in the world
    # pdf(paste0("/import/ecoc9z/data-zurell/keuth/Work/plots/plot_GBIF_TREAM_occ_", sp_name, ".pdf"))
    # maps::map('world')
    # plot(tmp.sf, col = "blue", add = T, pch = 19, main = sp_name)
    # dev.off()
  }
}

stopCluster(cl)

# Adding the calculated range sizes to the data set

#Load packages
library(sf)
library(dplyr)

# Load in the incomplete data set
load("/import/ecoc9z/data-zurell/keuth/Work/species_info_incomplete.Rdata")

#add column for MCP2
species_info$MCP2 <- NA

# calculate the MCP for each species if enough data points were available
for (i in 1:nrow(species_info)) {
  if (file.exists(paste0("/import/ecoc9z/data-zurell/keuth/Work/data/spatial_data_TREAM_GBIF_", species_info$binomial[i], ".Rdata"))){
    load(paste0("/import/ecoc9z/data-zurell/keuth/Work/data/spatial_data_TREAM_GBIF_", species_info$binomial[i], ".Rdata"))
    tmp.sf <- st_convex_hull(st_union(tmp.sf))
    if(st_is_valid(tmp.sf) == F){
      species_info$MCP2[i] <- "invalid geometry"
    } else {
      range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
      species_info$MCP2[i] <- as.numeric(range_coverage_df)
    }
  }
}


#save(species_info, file = "/import/ecoc9z/data-zurell/keuth/Work/in_between_MCP2_results_10k.Rdata")

# adding taxonomic information to the data set

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

# change column name
names(species_info)[names(species_info) == "binomial"] <- "name"

# save data set
write.csv(species_info, "/import/ecoc9z/data-zurell/keuth/Work/species_information_TREAM.csv", row.names = F)

gc()
rm(list=ls())