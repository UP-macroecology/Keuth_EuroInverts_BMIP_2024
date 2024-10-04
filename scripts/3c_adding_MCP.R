# Adding the calculated range sizes to the data set

#Load packages
library(sf)

# Load in the incomplete data set
load("/import/ecoc9z/data-zurell/keuth/Work/species_info_incomplete_sf.Rdata")
#load("data/species_info_incomplete_sf.Rdata")

#add column for MCP2
species_info$MCP2 <- NA

# add range size to the correct species if a file is there (if a range was calculated)
for (i in 1:nrow(species_info)) {
  #if (file.exists(paste0("data/MCP/tmp.sf_", species_info$binomial[i], ".Rdata"))){
    if (file.exists(paste0("/import/ecoc9z/data-zurell/keuth/Work/data/tmp.sf_", species_info$binomial[i], "20k.Rdata"))){
    #load(paste0("data/MCP/tmp.sf_", species_info$binomial[i], ".Rdata"))
      load(paste0("/import/ecoc9z/data-zurell/keuth/Work/data/tmp.sf_", species_info$binomial[i], "20k.Rdata"))
    tmp.sf <- st_convex_hull(st_union(tmp.sf))
    print(st_is_valid(tmp.sf, reason = T))
    if(st_is_valid(tmp.sf) == F){
      species_info$MCP2[i] <- "invalid geometry"
    } else {
    range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
    species_info$MCP2[i] <- as.numeric(range_coverage_df)
    }
  }
}

#save(species_info, file = "/import/ecoc9z/data-zurell/keuth/Work/in_between_MCP2_results_20k.Rdata")

 #save(species_info, file = "data/in_between_MCP2_results.Rdata")
# write.csv(species_info, "data/in_between_MCP2_results.csv")
# 
load("data/in_between_MCP2_results.Rdata")
# change 0 values to NA (data set was not available for the 0s meaning we don't have any information)
species_info[which(species_info$MCP2 == 0), "MCP2"] <- NA

# calculate percentage of global range covered by our data

species_info$perc_range_coverage <- round((species_info$MCP1/species_info$MCP2)*100,2)
#column name
names(species_info)[names(species_info) == "binomial"] <- "name"

write.csv(species_info, file = "data/species_information_TREAM.csv", row.names = F)


#species_notworked <- species_info[which(is.na(species_info$MCP2)), "name"]
#save(species_notworked, file = "data/species_MCP_notworked.Rdata")

