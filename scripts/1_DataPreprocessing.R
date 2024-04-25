# 1. Preprocessing Data
# Aim of this script is to combine the single csv files for the different study sites into one large data set

# Load packages
library(taxize)
library(stringr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(tibble)

#Loading functions
# capitalization 
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# preprocess Bulgaria_007.csv to match columns of the other data sets ---------
Bulgaria_007 <- read.csv("Paper + Information/GitHub repository/raw-data/csvs_each_dataset/Bulgaria_007.csv")

#split code and taxa into several columns
Bulgaria_007[c('site_id', 'day', "month", "year")] <- str_split_fixed(Bulgaria_007$code, '_', 4)
Bulgaria_007[c('taxon_id', 'taxon')] <- str_split_fixed(Bulgaria_007$taxa, '_', 2)

# create the column sample_id
Bulgaria_007$date <- paste(Bulgaria_007$day, Bulgaria_007$month, Bulgaria_007$year, sep=".")
Bulgaria_007$sample_id <- paste(Bulgaria_007$site_id, Bulgaria_007$date, sep = "_")

#reorder columns and remove old columns
Bulgaria_007 <- Bulgaria_007[, c("sample_id", "site_id", "day", "month", "year", "taxon_id", "taxon", "abundance")]

#save adjusted data frame
write.csv(Bulgaria_007, "data/Bulgaria_007.csv", row.names = F)

#Load in all other csv files in a list ----------

# list all files of the rawdata folder of the github repository
raw_freshinv_names <- list.files("Paper + Information/GitHub repository/raw-data/csvs_each_dataset")
raw_freshinv_names <- raw_freshinv_names[! raw_freshinv_names %in% c("Bulgaria_007.csv")] #remove the adjusted Bulgaria csv file from the file list

# Import data sets, modify and save in a list
raw_freshinv_list <- vector("list", length(raw_freshinv_names))
for (i in 1:length(raw_freshinv_names)) {
  raw_freshinv_list[[i]] <- read.csv(paste0("Paper + Information/GitHub repository/raw-data/csvs_each_dataset/", raw_freshinv_names[i])) # Importing file
  if(any(colnames(raw_freshinv_list[[i]]) == "X")){raw_freshinv_list[[i]] <- raw_freshinv_list[[i]] %>% select(-X)} #removing empty columns
  if(any(colnames(raw_freshinv_list[[i]]) == "X.1")){raw_freshinv_list[[i]] <- raw_freshinv_list[[i]] %>% select(-X.1)}
  if(raw_freshinv_names[i] == "UK_001.csv"){raw_freshinv_list[[i]]$listed.in.freshwaterecology.com <- "NA"}#add missing column to one specific dataset
  country <- str_remove(raw_freshinv_names[i], ".csv") #Extract country from file name
  country <- str_remove(country, "\\d+" )
  country <- str_remove(country, "_" )
  raw_freshinv_list[[i]]$country <- country #add country as new column
  raw_freshinv_list[[i]]$study_site <- paste(country, raw_freshinv_list[[i]]$site_id, sep = "_")
  names(raw_freshinv_list)[i] <- country  #change name of the list element
}

#add Bulgaria_007 additionally to the list
raw_freshinv_list[[43]] <- read.csv("data/Bulgaria_007.csv")
raw_freshinv_list[[43]]$listed.in.freshwaterecology.com <- "NA" #add missing column
raw_freshinv_list[[43]]$country <- "Bulgaria" #add country
raw_freshinv_list[[43]]$study_site <- paste("Bulgaria", raw_freshinv_list[[43]]$site_id, sep = "_") #add study site
names(raw_freshinv_list)[43] <- "Bulgaria" #change name of the element

#combine elements of the list to a large data frame --------
raw_freshinv <- do.call(rbind, raw_freshinv_list)
rownames(raw_freshinv) <- NULL #remove row names

# correcting taxa names ------------
Euro_Invert <- raw_freshinv
# fix misspelling of some taxa
Euro_Invert[which(Euro_Invert$taxon == "\"Ancylidae\" Gen. sp."),"taxon"] <- "Ancylidae Gen. sp." 
Euro_Invert[which(Euro_Invert$taxon == "Thienemannimyia , gen. indet."),"taxon"] <- "Thienemannimyia sp."
Euro_Invert[which(Euro_Invert$taxon == "Corbicula \"fluminalis\""),"taxon"] <- "Corbicula fluminalis"

#remove abbreviations ?to subspecies?
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " Lv.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " Ad.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " ssp.")
Euro_Invert$taxon <- str_replace(Euro_Invert$taxon, "-", " -")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " -gr.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " -agg.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " -Gr.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " -Agg.")

#remove wrong capitalization
Euro_Invert$taxon <- str_to_lower(Euro_Invert$taxon)
Euro_Invert$taxon <- firstup(Euro_Invert$taxon)

#replicate taxa column to species, genus, coarser
Euro_Invert$species <- Euro_Invert$taxon
Euro_Invert$genus <- Euro_Invert$taxon
Euro_Invert$coarser <- Euro_Invert$taxon

# extract cells with "sp." and set species to NA for those cells
row_index <- grep("sp.",Euro_Invert[,"species"])
Euro_Invert[row_index, "species"] <- NA

#extract cells with / in species and set species to NA for those cells
row_index <- grep("/",Euro_Invert[,"species"])
Euro_Invert[row_index, "species"] <- NA

#extract cells with gen. sp. in species and set species to NA for those cells
row_index <- grep(" gen. ",Euro_Invert[,"species"])
Euro_Invert[row_index, "species"] <- NA

#remove subspecies
Euro_Invert <- Euro_Invert%>%
  as_tibble() %>%
  mutate(
    species = map_chr(
      str_split(species, pattern = "\\s+"),
      ~ str_flatten(.x[1:2], " ")))

#back transform to data frame
Euro_Invert <- as.data.frame(Euro_Invert)

#remove families from genus column
row_index <- grep("gen. sp.",Euro_Invert[,"genus"])
Euro_Invert[row_index, "genus"] <- NA

#remove 2nd word in genus
Euro_Invert <- Euro_Invert%>%
  as_tibble() %>%
  mutate(
    genus = map_chr(
      str_split(genus, pattern = "\\s+"),
      ~ str_flatten(.x[1], " ")))

#back transform to data frame
Euro_Invert <- as.data.frame(Euro_Invert)

#remove unclear genus identification
row_index <- grep("/",Euro_Invert[,"genus"])
Euro_Invert[row_index, "genus"] <- NA

#remove genus and species names from coarser column
Euro_Invert[which(!is.na(Euro_Invert$genus)), "coarser"] <- NA

#remove gen.sp. from coarser column
Euro_Invert$coarser <- str_remove(Euro_Invert$coarser, "gen. sp.")

# remove unclear coarser identification
Euro_Invert[which(Euro_Invert$coarser == "Anax/hemianax sp."),"coarser"] <- "Aeshnidae"
Euro_Invert[which(Euro_Invert$coarser == "Annitella/chaetopteryx sp."),"coarser"] <- "Limnephilidae"
Euro_Invert[which(Euro_Invert$coarser == "Atherix/ibisia sp."),"coarser"] <- "Athericidae"
Euro_Invert[which(Euro_Invert$coarser == "Ceratopogoninae/palpomyiinae "),"coarser"] <- "Ceratopogonidae"
Euro_Invert[which(Euro_Invert$coarser == "Chaetopterygini/stenophylacini "),"coarser"] <- "Limnephilidae"
Euro_Invert[which(Euro_Invert$coarser == "Chelifera/hemerodromia sp."),"coarser"] <- "Empididae"
Euro_Invert[which(Euro_Invert$coarser == "Crangonyx/niphargus sp."),"coarser"] <- "Amphipoda"
Euro_Invert[which(Euro_Invert$coarser == "Habroleptoides/paraleptophlebia sp."),"coarser"] <- "Leptobhlebiidae"
Euro_Invert[which(Euro_Invert$coarser == "Tubificidae "),"coarser"] <- "Naididae"
Euro_Invert[which(Euro_Invert$coarser == "Naididae/tubificidae "),"coarser"] <- "Naididae"
Euro_Invert[which(Euro_Invert$coarser == "Perlidae/perlodidae "),"coarser"] <- "Plecoptera"
Euro_Invert[which(Euro_Invert$coarser == "Micropsectra/tanytarsus sp."),"coarser"] <- "Chironomidae"
Euro_Invert[which(Euro_Invert$coarser == "Leuctridae/capniidae "),"coarser"] <- "Plecoptera"
Euro_Invert$coarser <- str_remove(Euro_Invert$coarser, " ")

# check for correct species names (any misspelling or synonyms used?)
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

# correct the names in the large data set or extract the coarsest level that is known
# Holostomis is a old genus I could not find anywhere, hence I remove both species
Euro_Invert[which(Euro_Invert$species == "Vejdovskiella comata"), "species"] <- "Vejdovskyella comata"
Euro_Invert[which(Euro_Invert$species == "Vejdovskiella intermedia"), "species"] <- "Vejdovskyella intermedia"
Euro_Invert[which(Euro_Invert$species == "Orthocladiini cop"), "coarser"] <- "Chironomidae"
Euro_Invert[which(Euro_Invert$species == "Orthocladiini cop"), "species"] <- NA
Euro_Invert[which(Euro_Invert$species == "Orthocladiini cop"), "genus"] <- NA
Euro_Invert[which(Euro_Invert$species == "Holostomis atrata"), "genus"] <- NA
Euro_Invert[which(Euro_Invert$species == "Holostomis atrata"), "species"] <- NA
Euro_Invert[which(Euro_Invert$species == "Holostomis phalaenoides"), "genus"] <- NA
Euro_Invert[which(Euro_Invert$species == "Holostomis phalaenoides"), "species"] <- NA

# extract species list
species <- as.data.frame(sort(unique(Euro_Invert$species)))
names(species) <- c("identified")

# loop through every single species and extract tsn code, which is then checked for the accepted name if the species was found in tsn to see if any synonyms were used
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
#save(species, file="data/species_tsn_check.Rdata")
load("data/species_tsn_check.Rdata")

# check corrected species names for mistakes
row_index <- which(species$identified != species$tsn_check)
tmp <- species[row_index,]

# correct some mistakes that were made by assorting new names to the species
species[which(species$identified == "Agnetina elegantula"), "tsn_check"] <- "Agnetina elegantula"
species[which(species$identified == "Dendrocoelum lacteum"), "tsn_check"] <- "Dendrocoelum lacteum"
species[which(species$identified == "Dreissena rostriformis"), "tsn_check"] <- "Dreissena rostriformis"
species[which(species$identified == "Menetus dilatatus"), "tsn_check"] <- "Menetus dilatatus"

# check species that could not be found by hand
row_index <- which(is.na(species$tsn_check))
tmp <- species[row_index,]

# correcting names using GBIF (if a species name is marked as synonym the accepted scientific name is retrieved)
species[which(species$identified == "Afghanurus joernensis"), "tsn_check"] <- "Nixe joernensis"
species[which(species$identified == "Allotrichia pallicornis"), "tsn_check"] <- "Agraylea pallicornis"
species[which(species$identified == "Baetis atrebatinus"), "tsn_check"] <- "Labiobaetis atrebatinus"
species[which(species$identified == "Baetis digitatus"), "tsn_check"] <- "Nigrobaetis digitatus"
species[which(species$identified == "Baetis muticus"), "tsn_check"] <- "Alainites muticus"
species[which(species$identified == "Baetis niger"), "tsn_check"] <- "Nigrobaetis niger"
species[which(species$identified == "Baetis tricolor"), "tsn_check"] <- "Labiobaetis tricolor"
species[which(species$identified == "Capnia bifrons"), "tsn_check"] <- "Zwicknia bifrons"
species[which(species$identified == "Cercion lindenii"), "tsn_check"] <- "Erythromma lindenii"
species[which(species$identified == "Chironomus thummi"), "tsn_check"] <- "Chironomus riparius"
species[which(species$identified == "Cladopelma lateralis"), "tsn_check"] <- "Cladopelma goetghebueri"
species[which(species$identified == "Corophium robustum"), "tsn_check"] <- "Chelicorophium robustum"
species[which(species$identified == "Corophium sowinskyi"), "tsn_check"] <- "Chelicorophium sowinskyi"
species[which(species$identified == "Echinogammarus trichiatus"), "tsn_check"] <- "Chaetogammarus trichiatus"
species[which(species$identified == "Elodes marginata"), "tsn_check"] <- "Odeles marginata"
species[which(species$identified == "Ephemerella ignita"), "tsn_check"] <- "Serratella ignita"
species[which(species$identified == "Ephemerella mesoleuca"), "tsn_check"] <- "Teloganopsis mesoleuca"
species[which(species$identified == "Epoicocladius ephemerae"), "tsn_check"] <- "Epoicocladius flavens"
species[which(species$identified == "Ferrissia clessiniana"), "tsn_check"] <- "Pettancylus clessinianus"
species[which(species$identified == "Gomphus flavipes"), "tsn_check"] <- "Stylurus flavipes"
species[which(species$identified == "Gyraulus laevis"), "tsn_check"] <- "Gyraulus parvus"
species[which(species$identified == "Haliplus wehnckei"), "tsn_check"] <- "Haliplus sibiricus"
species[which(species$identified == "Heptagenia coerulans"), "tsn_check"] <- "Dacnogenia coerulans"
species[which(species$identified == "Heterotrissocladius grimshawi"), "tsn_check"] <- "Heterotanytarsus grimshawi"
species[which(species$identified == "Hydrochus carinatus"), "tsn_check"] <- "Hydrochus crenatus"
species[which(species$identified == "Hydropsyche newae"), "tsn_check"] <- "Ceratopsyche newae"
species[which(species$identified == "Hydropsyche silfvenii"), "tsn_check"] <- "Ceratopsyche silfvenii"
species[which(species$identified == "Ibisia marginata"), "tsn_check"] <- "Atherix marginata"
species[which(species$identified == "Limnephilus incisus"), "tsn_check"] <- "Pseudostenophylax incisus"
species[which(species$identified == "Limnodrilus claparedeanus"), "tsn_check"] <- "Limnodrilus claparedianus"
species[which(species$identified == "Marstoniopsis scholtzi"), "tsn_check"] <- "Marstoniopsis insubrica"
species[which(species$identified == "Micropterna lateralis"), "tsn_check"] <- "Stenophylax lateralis"
species[which(species$identified == "Micropterna nycterobia"), "tsn_check"] <- "Stenophylax nycterobius"
species[which(species$identified == "Micropterna sequax"), "tsn_check"] <- "Stenophylax sequax"
species[which(species$identified == "Neolimnomyia nemoralis"), "tsn_check"] <- "Dicranophragma nemorale"
species[which(species$identified == "Normandia nitens"), "tsn_check"] <- "Riolus nitens"
species[which(species$identified == "Nymphula stagnata"), "tsn_check"] <- "Nymphula nitidulata"
species[which(species$identified == "Palaemonetes longirostris"), "tsn_check"] <- "Palaemon longirostris"
species[which(species$identified == "Paracladopelma laminata"), "tsn_check"] <- "Paracladopelma laminatum"
species[which(species$identified == "Paracladopelma nigritula"), "tsn_check"] <- "Paracladopelma nigritulum"
species[which(species$identified == "Paralauterborniella nigrohalteralis"), "tsn_check"] <- "Paralauterborniella nigrohalterale"
species[which(species$identified == "Paralimnophyes longiseta"), "tsn_check"] <- "Paralimnophyes hydrophilus"
species[which(species$identified == "Pisidium hibernicum"), "tsn_check"] <- "Euglesa parvula"
species[which(species$identified == "Pisidium moitessierianum"), "tsn_check"] <- "Odhneripisidium moitessierianum"
species[which(species$identified == "Pisidium obtusale"), "tsn_check"] <- "Euglesa obtusalis"
species[which(species$identified == "Pisidium personatum"), "tsn_check"] <- "Euglesa personata"
species[which(species$identified == "Pisidium pulchellum"), "tsn_check"] <- "Euglesa pulchella"
species[which(species$identified == "Pisidium tenuilineatum"), "tsn_check"] <- "Odhneripisidium tenuilineatum"
species[which(species$identified == "Radix balthica"), "tsn_check"] <- "Ampullaceana balthica"
species[which(species$identified == "Radix labiata"), "tsn_check"] <- "Peregriana labiata"
species[which(species$identified == "Satchelliella mutua"), "tsn_check"] <- "Pneumia mutua"
species[which(species$identified == "Satchelliella nubila"), "tsn_check"] <- "Pneumia nublia"
species[which(species$identified == "Satchelliella pilularia"), "tsn_check"] <- "Pneumia pilularia"
species[which(species$identified == "Satchelliella trivialis"), "tsn_check"] <- "Pneumia trivialis"
species[which(species$identified == "Simulium rostratum"), "tsn_check"] <- "Simulium longirostre"
species[which(species$identified == "Sphaerium radiatum"), "tsn_check"] <- "Sphaerium ovale"
species[which(species$identified == "Synagapetus iridipennis"), "tsn_check"] <- "Agapetus iridipennis"
species[which(species$identified == "Synagapetus moselyi"), "tsn_check"] <- "Agapetus moselyi"
species[which(species$identified == "Synendotendipes impar"), "tsn_check"] <- "Endochironomus impar"
species[which(species$identified == "Synendotendipes lepidus"), "tsn_check"] <- "Endochironomus lepidus"

# all other species names are correct, thus the NAs can be replaced by the original species name
for (i in 1:nrow(species)){
  if(is.na(species$tsn_check[i])){
    species$tsn_check[i] <- species$identified[i]
  }
}

# add corrected names to the data set
for (i in 1:nrow(species)){
  Euro_Invert[which(Euro_Invert$taxon == species$identified[i]),"species"] <- species$tsn_check[i]
}

# save large data frame
write.csv(raw_freshinv, "data/Euro_FreshInv_preprocessed.csv", row.names = F)
