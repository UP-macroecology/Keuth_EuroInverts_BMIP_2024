# GBIF Information cluster script

# Load packages
library(dplyr)
library(sp)
library(rgbif)
library(foreach)
library(doParallel)
library(sf)
library(maps)

load("/import/ecoc9z/data-zurell/keuth/Work/species_info_incomplete_sf.Rdata")
#load("data/species_info_incomplete_sf.Rdata")
#load("/import/ecoc9z/data-zurell/keuth/Work/species_MCP_notworked.Rdata")
load("/import/ecoc9z/data-zurell/keuth/Work/TREAM_sf.Rdata")
#load("data/TREAM_sf.Rdata")

species_info$MCP2 <- NA

#set up cluster
ncores <- 40
cl <- makeCluster(ncores)
registerDoParallel(cl)

foreach(i=1:nrow(species_info), .packages = c("dplyr", "sp", "rgbif", "sf", "maps")) %dopar% {
#for(i in 1:5){
  # Extract species name of the loop
  sp_name <- unique(species_info$binomial)[i]
  #print(sp_name)
  
  #Extract species from TREAM data set
  tmp_TREAM.sf <- subset(TREAM.sf, TREAM.sf$binomial == sp_name)

  # rename column
  names(tmp_TREAM.sf)[names(tmp_TREAM.sf) == "binomial"] <- "species"
  
  # Download GBIF data
  tmp_gbif <- occ_search(scientificName = sp_name, hasCoordinate=T, basisOfRecord='HUMAN_OBSERVATION', limit = 20000)
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
    tmp.sf <- st_make_valid(tmp_TREAM.sf)
  }
  if(nrow(tmp.sf) > 2){
    save(tmp.sf, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/data/tmp.sf_", sp_name, "20k.Rdata"))
    #save(tmp.sf, file = paste0("data/GBIF_occurrences/tmp.sf_", sp_name, ".Rdata"))
    # pdf(paste0("/import/ecoc9z/data-zurell/keuth/Work/plots/plot_GBIF_TREAM_occ_", sp_name, ".pdf"))
    # maps::map('world')
    # plot(tmp.sf, col = "blue", add = T, pch = 19, main = sp_name)
    # dev.off()
    
    #tmp.sf <- st_convex_hull(st_union(tmp.sf))

    #range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))

    #save(range_coverage_df, file = paste0("/import/ecoc9z/data-zurell/keuth/Work/MCP2_", sp_name, ".Rdata"))
    
    # tmp.sf <- st_convex_hull(st_union(tmp.sf))
    # 
    # if(st_is_valid(tmp.sf) == F){
    #   species_info$MCP2[i] <- "invalid geometry"
    # } else {
    #   range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
    #   species_info$MCP2[i] <- as.numeric(range_coverage_df)
    # }
  }
}

#save(species_info, file = "data/species_info_MCP2_complete.Rdata")

#save(species_info, file = "/import/ecoc9z/data-zurell/keuth/Work/species_info_complete_sf.Rdata")

stopCluster(cl)

gc()
rm(list=ls())