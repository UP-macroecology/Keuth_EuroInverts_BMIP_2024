# Control for misspelled species names or synonyms

# Load packages
library(taxize)
library(help = taxize)

# Load data set
Euro_Invert <- read.csv("data/Euro_FreshInv_all_sites_split_taxa.csv")

# extract species list
species <- as.data.frame(sort(unique(Euro_Invert$species)))
names(species) <- c("identified")

# check for correct species name/ misspelling
species_check1 <- gnr_resolve(species[1:round(nrow(species)/2),], best_match_only = T)
species_check2 <- gnr_resolve(species[round(nrow(species)/2)+1:nrow(species),], best_match_only = T)

#check if given name in the column and the found name online are the same
row_index1 <-which(species_check1$submitted_name != species_check1$matched_name)
species_check1[row_index1,]
row_index2 <-which(species_check2$submitted_name != species_check2$matched_name)
species_check2[row_index2,]

#there are two species with wrong names and two with misspelled names

# check for duplicates
# loop through every single species and extract tsn code, which is then checked for the accepted name if the species was found in tsn
species$tsn_check <- NA
for (i in 1:nrow(species)) {
  tsn <- get_tsn(species[i,"identified"], accepted = F)
  if(is.na(tsn)){
    species$tsn_check[i] <- NA
  } else {
    itis <- lapply(tsn, itis_acceptname)
    if(is.na(itis[[1]]$acceptedname)){
      species$tsn_check[i] <- species$identified[i]
    } else {
      species$tsn_check[i] <- itis[[1]]$acceptedname
    }
  }
}
# species with NA in tsn_check need to be checked by hand for their correct species name

#save the data set
save(species, file="data/species_tsn_check.Rdata")

row_index <- which(species$identified != species$tsn_check)
species[row_index,]
