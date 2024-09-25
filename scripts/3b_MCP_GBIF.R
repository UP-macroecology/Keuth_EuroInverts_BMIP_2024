# GBIF Information cluster script

# Load packages
library(dplyr)
library(adehabitatHR)
library(sp)
library(rgbif)
library(foreach)
library(doParallel)
library(sf)

load("/import/ecoc9z/data-zurell/keuth/Work/species_info_incomplete_sf.Rdata")
#load("/import/ecoc9z/data-zurell/keuth/Work/species_MCP_notworked.Rdata")
load("/import/ecoc9z/data-zurell/keuth/Work/TREAM_sf.Rdata")

#species_info$MCP2 <- NA

#set up cluster
ncores <- 40
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(i=1:nrow(species_info), .packages = c("adehabitatHR", "dplyr", "sp", "rgbif", "sf")) %dopar% {
  #for(i in 2) {
  # Extract species name of the loop
  sp_name <- unique(species_info$binomial)[i]
  print(sp_name)
  #Extract species from TREAM data set
  tmp_TREAM.sf <- subset(TREAM.sf, TREAM.sf$binomial == sp_name)
  print(head(tmp_TREAM.sf))
  # rename column
  names(tmp_TREAM.sf)[names(tmp_TREAM.sf) == "binomial"] <- "species"
  
  # Download GBIF data
  tmp_gbif <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 5000)
  tmp_gbif <- tmp_gbif$data
  print(head(tmp_gbif))
  save(tmp_TREAM.sf, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/MCP2_TREAM_", sp_name, ".Rdata"))
  
  #Transform GBIF data into sf file if occurrences are present
  if(length(unique(tmp_gbif$decimalLatitude)) >= 1){
    #extract species and coordinates
    tmp_gbif.sp <- tmp_gbif[,c("species", "decimalLatitude", "decimalLongitude")]
    
    # transform into sf object
    tmp_gbif.sf <- st_as_sf(tmp_gbif.sp, coords=  c("decimalLatitude", "decimalLongitude"), crs = 4326)
    
    save(tmp_gbif, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/MCP2_GBIF", sp_name, ".Rdata"))
    
    # add data occurrences and gbif data together
    tmp.sf <- rbind(tmp_gbif.sf, tmp_TREAM.sf)
    print(head(tmp.sf))
    tmp.sf <- st_make_valid(tmp.sf)
  } else {
    tmp.sf <- st_make_valid(tmp_TREAM.sf)
  }
  if(nrow(tmp.sf) > 2){
    save(tmp.sf, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/tmp.sf_", sp_name, ".Rdata"))
    tmp.sf <- st_convex_hull(st_union(tmp.sf))
    print(head(tmp.sf))
    range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
    print(range_coverage_df)
    #species_info[which(species_info$binomial == sp_name), "MCP2"] <- as.numeric(range_coverage_df)
    save(range_coverage_df, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/MCP2_", sp_name, ".Rdata"))
  }
}

#save(species_info, file = "/import/ecoc9z/data-zurell/keuth/Work/species_info_complete_sf.Rdata")

stopCluster(cl)

gc()
rm(list=ls())