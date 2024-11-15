# test number of occurrences

library(sf)

# prepare data sets

load("data/in_between_MCP2_results.Rdata")

GBIF_MCP_5k <- species_info[, c("binomial", "MCP2")]

load("data/in_between_MCP2_results_10k.Rdata")

GBIF_MCP_10k <- species_info[, c("binomial", "MCP2")]

load("data/in_between_MCP2_results_20k.Rdata")

GBIF_MCP_20k <- species_info[, c("binomial", "MCP2")]

# species with extra calculations
extra_species <- c("Chironomus riparius", "Corbicula fluminalis", "Corbicula fluminea", "Cordylophora caspia", "Corynoneura scutellata", "Dreissena bugensis", "Eiseniella tetraedra", "Galba truncatula", 
                   "Helobdella stagnalis", "Lumbriculus variegatus", "Lymnaea stagnalis", "Nais variabilis",  "Physella acuta", "Pisidium casertanum", "Planorbarius corneus", "Potamopyrgus antipodarum", 
                   "Procambarus clarkii", "Radix auricularia", "Rhantus suturalis") 

for(i in 1:length(extra_species)){
  #for 5k
  load(paste0("data/MCP/tmp.sf_", extra_species[i], ".Rdata"))
  tmp.sf <- st_convex_hull(st_union(tmp.sf))
  if(st_is_valid(tmp.sf) == F){
    GBIF_MCP_5k[which(GBIF_MCP_5k$binomial == extra_species[i]), "MCP2"] <- NA
  } else {
  range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
  GBIF_MCP_5k[which(GBIF_MCP_5k$binomial == extra_species[i]), "MCP2"] <- as.numeric(range_coverage_df)
  }
  
  #for 10k
  load(paste0("data/MCP/tmp.sf_", extra_species[i], "10k.Rdata"))
  tmp.sf <- st_convex_hull(st_union(tmp.sf))
  if(st_is_valid(tmp.sf) == F){
    GBIF_MCP_10k[which(GBIF_MCP_10k$binomial == extra_species[i]), "MCP2"] <- NA
  } else {
  range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
  GBIF_MCP_10k[which(GBIF_MCP_10k$binomial == extra_species[i]), "MCP2"] <- as.numeric(range_coverage_df)
  }
  
  #for 20k
  load(paste0("data/MCP/tmp.sf_", extra_species[i], "20k.Rdata"))
  tmp.sf <- st_convex_hull(st_union(tmp.sf))
  if(st_is_valid(tmp.sf) == F){
    GBIF_MCP_20k[which(GBIF_MCP_20k$binomial == extra_species[i]), "MCP2"] <- NA
  } else {
  range_coverage_df <- sum(st_area(st_make_valid(tmp.sf)))
  GBIF_MCP_20k[which(GBIF_MCP_20k$binomial == extra_species[i]), "MCP2"] <- as.numeric(range_coverage_df)
  }
}

# calculate differences in MCP
GBIF_MCP_5k$MCP2_perc <- 1
GBIF_MCP_10k$MCP2_perc <- GBIF_MCP_10k$MCP2/ GBIF_MCP_5k$MCP2
GBIF_MCP_20k$MCP2_perc <- GBIF_MCP_20k$MCP2/ GBIF_MCP_5k$MCP2

# add number of occurrences
GBIF_MCP_5k$occ <- "5000"
GBIF_MCP_10k$occ <- "10000" 
GBIF_MCP_20k$occ <- "20000"   

# join all data sets
GBIF_MCP <- rbind(GBIF_MCP_5k, GBIF_MCP_10k, GBIF_MCP_20k)
GBIF_MCP$occ <- as.numeric(GBIF_MCP$occ)

# rename columns
names(GBIF_MCP_10k)[names(GBIF_MCP_10k) == "MCP2"] <- "MCP2_10k"
names(GBIF_MCP_20k)[names(GBIF_MCP_20k) == "MCP2"] <- "MCP2_20k"
names(GBIF_MCP_5k)[names(GBIF_MCP_5k) == "MCP2"] <- "MCP2_5k"

# cbind the different data sets
GBIF_MCP2 <- merge(GBIF_MCP_5k[, c("binomial", "MCP2_5k")], GBIF_MCP_10k[, c("binomial", "MCP2_10k")], by = "binomial")
GBIF_MCP2 <- merge(GBIF_MCP_20k[, c("binomial", "MCP2_20k")], GBIF_MCP2, by = "binomial")

save(GBIF_MCP2, file = "data/GBIF_MCP_occurrence_comparison.Rdata")

load("data/species_info_incomplete.Rdata")

# merge with other species info
species_info <- merge(species_info, GBIF_MCP2, by = "binomial")

# calculate range coverage of MCP1 in comparison to the other ones
species_info$perc_range_coverage_5k <- round((species_info$MCP1/species_info$MCP2_5k)*100,2)
species_info$perc_range_coverage_10k <- round((species_info$MCP1/species_info$MCP2_10k)*100,2)
species_info$perc_range_coverage_20k <- round((species_info$MCP1/species_info$MCP2_20k)*100,2)

save(species_info, file = "data/species_info_different_GBF.Rdata")

# Extract the top 100 species with the most range coverage with the different GBIF occurrences

GBIF_top100_5k <- species_info %>% select(c(binomial, perc_range_coverage_5k)) %>% arrange(desc(perc_range_coverage_5k)) %>% slice(1:100)
GBIF_top100_10k <- species_info %>% select(c(binomial, perc_range_coverage_10k)) %>% arrange(desc(perc_range_coverage_10k)) %>% slice(1:100)
GBIF_top100_20k <- species_info %>% select(c(binomial, perc_range_coverage_20k)) %>% arrange(desc(perc_range_coverage_20k)) %>% slice(1:100)

#check for differences in the data set
length(which(GBIF_top100_5k$binomial != GBIF_top100_10k$binomial))
length(which(GBIF_top100_10k$binomial != GBIF_top100_20k$binomial))

# Extract species with "Inf" values
weird_species <- GBIF_top100_10k[which(GBIF_top100_10k$perc_range_coverage_10k == Inf), "binomial"]

# extract values from the large data set
weird_species_GBIF <- GBIF_MCP2[which(GBIF_MCP2$binomial %in% weird_species),]

load("data/MCP/tmp.sf_Chironomus riparius.Rdata")

# library(ggplot2)
# 
# ggplot(GBIF_MCP, aes(x=occ, y = MCP2_perc))+
#   geom_point()+
#   geom_smooth(aes(group = binomial), method = "loess", linewidth = 0.8)+
#   xlab("Number of GBIF occurrences")+
#   ylab("MCP")+
#   geom_smooth(col = "red", method = "loess", linewidth = 1.5)
# 
# ggplot(GBIF_MCP, aes(x=occ, y = MCP2_perc))+
#   geom_point()+
#   geom_smooth(aes(group = binomial), method = "loess", linewidth = 0.8)+
#   xlab("Number of GBIF occurrences")+
#   ylab("MCP")+
#   geom_smooth(col = "red", method = "loess", linewidth = 1.5)+
#   ylim(c(0.8,8))
# 
# library(ggforce)
# 
# p <- ggplot(GBIF_MCP, aes(x=occ, y = MCP2))+
#   geom_point()+
#   geom_line()+
#   facet_wrap_paginate(~binomial, ncol = 5, nrow = 5, scales = "free_y")
# 
# for(i in 1:n_pages(p)){
#   p_save <-  p + 
#     facet_wrap_paginate(~ binomial, ncol = 5, nrow = 5, page = i, scales = "free_y")
#   ggsave(plot = p_save, filename = paste0('data/plots/page_', i, '.jpg'))
# }
