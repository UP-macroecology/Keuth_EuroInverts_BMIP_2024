# GBIF Information cluster script

# Load packages
library(dplyr)
library(adehabitatHR)
library(sp)
library(rgbif)
library(foreach)
library(doParallel)

load("/import/ecoc9z/data-zurell/keuth/Work/species_info_incomplete_sf.Rdata")

species_info$MCP2 <- NA

#set up cluster
ncores <- 20
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(i=1:length(unique(species_info$binomial)), .packages = c("adehabitatHR", "dplyr", "sp", "rgbif", "sf")) %dopar% {
#for (i in 1:length(unique(species_info$binomial))) {
  sp_name <- unique(species_info$binomial)[i]
  test <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 5000)
  test <- test$data
  
  if(length(unique(test$decimalLatitude)) > 2){
    
    #extract species and coordinates
    test.sp <- test[,c("species", "decimalLatitude", "decimalLongitude")]
    
    # transform into sf object
    test.sf <- st_as_sf(test.sp, coords=  c("decimalLatitude", "decimalLongitude"), crs = 4326)
    test.sf <- st_make_valid(test.sf)
    
    test.sf <- st_convex_hull(st_union(test.sf))
    range_coverage_df <- sum(st_area(st_make_valid(test.sf)))
    species_info[which(species_info$binomial == unique(species_info$binomial)[i]), "MCP2"] <- as.numeric(range_coverage_df)
    save(range_coverage_df, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/MCP2_", sp_name, ".Rdata"))
  }
  
}

save(species_info, file = "/import/ecoc9z/data-zurell/keuth/Work/species_info_complete_sf.Rdata")

