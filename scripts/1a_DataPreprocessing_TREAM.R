# Preprocessing the TREAM data set
# This is the better prepared dataset about the macroinvertebrate freshwater data from Europe
# Goal: Preprocessing of the taxon column

# Load packages
library(taxize)
library(stringr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(tibble)

# load in data
TREAM <- read.csv("data/TREAM/TREAM_allTaxa.csv", na.strings=c("", "NA"))

# correct object classes of some columns
TREAM$site_id <- as.character(TREAM$site_id)
TREAM$month <- as.integer(TREAM$month) # entries of month of Italy is set to NA, as they specified the month as 6 o 8

# fix taxonID column
#sort(unique(TREAM$taxon_id), decreasing = T)
#in the taxon ID four taxon groups are wongly added: Platyhelminthes Gen. sp.; Nemertea Gen. sp.; Mollusca Gen. sp.; Clitellata Gen. sp.

#set them to NA as the additional information is also available in other columns
TREAM[which(TREAM$taxon_id %in% c("Platyhelminthes Gen. sp.", "Nemertea Gen. sp.", "Mollusca Gen. sp.", "Clitellata Gen. sp.") ), "taxon_id"] <- NA

# add date to the data set
TREAM$date <- paste(TREAM$year, TREAM$month, TREAM$day,  sep = "-")

# modify date column for Finland and Italy as both of them do not provide a day but only a month
TREAM[which(is.na(TREAM$month)), "date"] <- TREAM[which(is.na(TREAM$month)), "year"]
TREAM[which(TREAM$country == "Finland"), "date"] <- paste(TREAM[which(TREAM$country == "Finland"), "year"], TREAM[which(TREAM$country == "Finland"), "month"],  sep = "-")

#remove species appendices
TREAM$species <- str_remove(TREAM$species, " Lv.")
TREAM$species <- str_remove(TREAM$species, " Ad.")
# Create full scientific species name from two columns
TREAM$binomial <- paste(TREAM$Genus.group, TREAM$species)

# reorder columns in data frame
TREAM <- TREAM %>% relocate(country, .after = site_id) %>% relocate(date, .after = year) %>% relocate(binomial, .after = species)

# remove binomial name for individuals that are just identified to genus or family level
TREAM[grep("/",TREAM[,"binomial"]), "binomial"] <- NA
TREAM[TREAM$species %in% c("sp.", "Gen. sp."), "binomial"] <- NA
TREAM[which(TREAM$binomial == " "),] <- NA

#remove species appendices
TREAM$binomial <- str_remove(TREAM$binomial, " Lv.")
TREAM$binomial <- str_remove(TREAM$binomial, " Ad.")
TREAM$binomial <- str_remove(TREAM$binomial, " ssp.")
TREAM$binomial <- str_replace(TREAM$binomial, "-", " -")
TREAM$binomial <- str_remove(TREAM$binomial, " -gr.")
TREAM$binomial <- str_remove(TREAM$binomial, " -agg.")
TREAM$binomial <- str_remove(TREAM$binomial, " -Gr.")
TREAM$binomial <- str_remove(TREAM$binomial, " -Agg.")

#remove subspecies identification
TREAM <- TREAM %>%
  as_tibble() %>%
  mutate(
    binomial = map_chr(
      str_split(binomial, pattern = "\\s+"),
      ~ str_flatten(.x[1:2], " ")))

TREAM <- as.data.frame(TREAM)

# adjust certain minor mistakes in species names
TREAM[which(TREAM$binomial == "Vejdovskiella comata"), "binomial"] <- "Vejdovskyella comata"
TREAM[which(TREAM$binomial == "Corbicula \"fluminalis\""),"binomial"] <- "Corbicula fluminalis"

#remove binomial names which don't make sense and can not be find
TREAM[which(TREAM$binomial == "Thienemannimyia Gr."), "binomial"] <- NA
TREAM[which(TREAM$binomial == "Orthocladiini COP"),"binomial"] <- NA
TREAM[which(TREAM$species == "Gr."), "species"] <- NA
TREAM[which(TREAM$species == "COP"), "species"] <- NA

# check for correct binomial names -----------

# extract species names
species <- as.data.frame(sort(unique(TREAM$binomial)))
names(species) <- c("identified")

# loop through every single species and extract tsn code, which is then checked for the accepted name if the species was found in tsn to see if any synonyms were used
# species$tsn_check <- NA
# for (i in 1:nrow(species)) {
#   tsn <- get_tsn(species[i,"identified"], accepted = F)
#   if(is.na(tsn)){
#     species$tsn_check[i] <- NA
#   } else {
#     itis <- lapply(tsn, itis_acceptname)
#     if(is.na(itis[[1]]$acceptedname)){
#       species$tsn_check[i] <- species$identified[i]
#     } else {
#       species$tsn_check[i] <- itis[[1]]$acceptedname
#     }
#   }
# }
# species with NA in tsn_check need to be checked by hand for their correct species name

#save the data set
#save(species, file="data/species_TREAM_tsn_check.Rdata")
load("data/species_TREAM_tsn_check.Rdata")

# check corrected species names of the function for mistakes
row_index <- which(species$identified != species$tsn_check)
tmp <- species[row_index,]

# correct some mistakes that were made by the function for assorting new names to the species
species[which(species$identified == "Agnetina elegantula"), "tsn_check"] <- "Agnetina elegantula"
species[which(species$identified == "Cloeon inscriptum"), "tsn_check"] <- "Cloeon inscriptum"
species[which(species$identified == "Dendrocoelum lacteum"), "tsn_check"] <- "Dendrocoelum lacteum"
species[which(species$identified == "Dugesia lugubris"), "tsn_check"] <- "Dugesia lugubris"
species[which(species$identified == "Menetus dilatatus"), "tsn_check"] <- "Menetus dilatatus"
species[which(species$identified == "Tvetenia discoloripes"), "tsn_check"] <- "Tvetenia discoloripes"
species[which(species$identified == "Ula sylvatica"), "tsn_check"] <- "Ula sylvatica"

# check species by hand that could not be found by the function
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

# extract corrected species names
row_index <- which(species$identified != species$tsn_check)
species_corrected <- species[row_index, "identified"]

# replace all NAs with the identified binomial names, as they are correct
for (i in 1:nrow(species)){
  if(is.na(species$tsn_check[i])){
    species$tsn_check[i] <- species$identified[i]
  }
}

# extract data from the species with the changed name to later check the coarser levels
df_species_corrected <- c()
for(i in 1:length(species_corrected)){
  df_species_corrected <- rbind(df_species_corrected, TREAM[which(TREAM$binomial == species_corrected[i]),c(7:10, 12)])
}

# keep only the distinct species names
df_species_corrected <- distinct(df_species_corrected)

# merge the extracted old species names with the new ones
df_species_corrected <- merge(df_species_corrected, species, by.x = "binomial", by.y = "identified")

# add the corrected names to the data set
for (i in 1:nrow(species)){
 TREAM[which(TREAM$binomial == species$identified[i]),"binomial"] <- species$tsn_check[i]
}

# correct wrong coarser levels due to changes in the species names
TREAM[which(TREAM$binomial == "Peringia ulvae"), "Group"] <- "Gastropoda"
TREAM[which(TREAM$binomial == "Pettancylus clessinianus"), "Family"] <- "Planorbidae"
TREAM[which(TREAM$binomial == "Odeles marginata"), "Family"] <- "Scirtidae"

# correct genus and species in the TREAM data set
# split the corrected species names
TREAM[c("Genus_corrected", "Species_corrected")] <- str_split_fixed(TREAM$binomial, " ", 2)
# replace missing data with NA
TREAM[which(TREAM$Species_corrected == ""), "Species_corrected"] <- NA
# extract rows where the identified scientific name is not the same as the corrected one
row.index <- which(TREAM["Genus.group"] != TREAM["Genus_corrected"])
row.index2 <- which(TREAM["species"] != TREAM["Species_corrected"])

# replace the identified scientific name with the corrected one
for (i in 1:length(row.index)) {
    TREAM[row.index[i], "Genus.group"] <- TREAM[row.index[i], "Genus_corrected"]
}

for (i in 1:length(row.index2)) {
  TREAM[row.index2[i], "species"] <- TREAM[row.index2[i], "Species_corrected"]
}

# remove additional columns
TREAM <- TREAM[, -which(names(TREAM) %in% c("Genus_corrected", "Species_corrected"))]

# add column which specifies to which level the individuum was identified
TREAM$taxon_level <- NA
TREAM[which(!is.na(TREAM$binomial)), "taxon_level"] <- "s"
TREAM[which(TREAM$species == "sp."), "taxon_level"] <- "g"
TREAM[grep("/",TREAM[,"species"]), "taxon_level"] <- "g"
TREAM[which(is.na(TREAM$taxon_level)), "taxon_level"] <- "c"

# back transform data set to data frame
TREAM <- as.data.frame(TREAM)

# Genus level ------

# set Genus to NA, if it wasn't determined
TREAM[which(TREAM$species == "Gen. sp."), "Genus.group"] <- NA
TREAM[grep("/",TREAM[,"species"]), "species"] <- NA
TREAM$species[TREAM$species %in% c("sp.", "Gen. sp.", "s.")] <- NA

# rename column
names(TREAM)[names(TREAM) == "Genus.group"] <- "genus"

# Family level -------

# obtain families name with outside function
# obtain the order level for every single identification
genus <- sort(unique(TREAM$genus))

# genus_families <- data.frame()
# for (i in 460:length(genus)){
#   temp <- tax_name(genus[i], get = "family")
#   genus_families <- rbind(genus_families, temp)
# }

# #check NAs
# genus_families[which(genus_families$query == "Agriotypus"), "family"] <- "Ichneumonidae"
# genus_families[which(genus_families$query %in% c("Ampullaceana", "Alainites", "Nigrobaetis")), "family"] <- "Baetidae"
# genus_families[which(genus_families$query == "Anisus"), "family"] <- "Curculionidae"
# genus_families[which(genus_families$query %in% c("Atrichops", "Atherix/Ibisia")), "family"] <- "Athericidae"
# genus_families[which(genus_families$query %in% c("Bathyomphalus", "Hippeutis", "Pettancylus", "Planorbarius", "Segmentina")), "family"] <- "Planorbidae"
# genus_families[which(genus_families$query %in% c("Batracobdelloides", "Hemiclepsis")), "family"] <- "Glossiphoniidae"
# genus_families[which(genus_families$query %in% c("Bazarella", "Berdeniella", "Clytocerus", "Jungiella", "Peripsychoda", "Pneumia", "Satchelliella", "Tonnoiriella", "Ulomyia")), "family"] <- "Psychodidae"
# genus_families[which(genus_families$query == "Borysthenia"), "family"] <- "Valvatidae"
# genus_families[which(genus_families$query %in% c("Brachytron", "Anax/Hemianax")), "family"] <- "Aeshnidae"
# genus_families[which(genus_families$query %in% c("Branchiura", "Vejdovskiella")), "family"] <- "Naididae"
# genus_families[which(genus_families$query == "Cataclysta"), "family"] <- "Crambidae"
# genus_families[which(genus_families$query %in% c("Ceriagrion", "Erythromma", "Pyrrhosoma", "Cercion")), "family"] <- "Coenagrionidae"
# genus_families[which(genus_families$query %in% c("Chalcolestes", "Sympecma")), "family"] <- "Lestidae"
# genus_families[which(genus_families$query == "Chelicorophium"), "family"] <- "Corophiidae"
# genus_families[which(genus_families$query %in% c("Cyphon", "Hydrocyphon", "Odeles")), "family"] <- "Scirtidae"
# genus_families[which(genus_families$query %in% c("Dacnogenia", "Electrogena", "Kageronia")), "family"] <- "Heptageniidae"
# genus_families[which(genus_families$query %in% c("Dicranomyia", "Dicranophragma", "Discobola", "Eloeophila", "Euphylidorea", "Neolimnomyia", "Phylidorea", "Rhypholophus", "Scleroprocta", "Eutonia", 
#                                                  "Neolimnomyia (Brachylimnophila)", "Neolimnomyia (Neolimnomyia)")), "family"] <- "Limoniidae"
# genus_families[which(genus_families$query %in% c("Dictyogenus", "Guadalgenus", "Hemimelaena", "Perlodes")), "family"] <- "Perlodidae"
# genus_families[which(genus_families$query == "Dikerogammarus"), "family"] <- "Gammaridae"
# genus_families[which(genus_families$query == "Esperiana"), "family"] <- "Melanopsidae"
# genus_families[which(genus_families$query %in% c("Euglesa", "Odhneripisidium", "Sphaerium")), "family"] <- "Sphaeriidae"
# genus_families[which(genus_families$query %in% c("Habroleptoides", "Habroleptoides/Paraleptophlebia")), "family"] <- "Leptophlebiidae"
# genus_families[which(genus_families$query %in% c("Isoptena", "Siphonoperla")), "family"] <- "Chloroperlidae"
# genus_families[which(genus_families$query == "Italobdella"), "family"] <- "Piscicolidae"
# genus_families[which(genus_families$query %in% c("Kloosia", "Lipiniella", "Chironomus (Chironomus)", "Micropsectra/Tanytarsus", "Orthocladiini", "Polypedilum (Polypedilum)")), "family"] <- "Chironomidae"
# genus_families[which(genus_families$query == "Liponeura"), "family"] <- "Blephariceridae"
# genus_families[which(genus_families$query == "Marstoniopsis"), "family"] <- "Amnicolidae"
# genus_families[which(genus_families$query == "Metreletus"), "family"] <- "Ameletidae"
# genus_families[which(genus_families$query %in% c("Micropterna", "Annitella/Chaetopteryx")), "family"] <- "Limnephilidae"
# genus_families[which(genus_families$query %in% c("Myxas", "Omphiscola", "Peregriana")), "family"] <- "Lymnaeidae"
# genus_families[which(genus_families$query == "Nemurella"), "family"] <- "Nemouridae"
# genus_families[which(genus_families$query == "Niphargus"), "family"] <- "Niphargidae"
# genus_families[which(genus_families$query %in% c("Obesogammarus", "Pontogammarus")), "family"] <- "Pontogammaridae"
# genus_families[which(genus_families$query == "Oligoneuriella"), "family"] <- "Oligoneuriidae"
# genus_families[which(genus_families$query == "Onychogomphus"), "family"] <- "Gomphidae"
# genus_families[which(genus_families$query == "Orthetrum"), "family"] <- "Libellulidae"
# genus_families[which(genus_families$query == "Osmylus"), "family"] <- "Osmylidae"
# genus_families[which(genus_families$query == "Palingenia"), "family"] <- "Palingeniidae"
# genus_families[which(genus_families$query == "Platycnemis"), "family"] <- "Platycnemididae"
# genus_families[which(genus_families$query == "Pomatinus"), "family"] <- "Dryopidae"
# genus_families[which(genus_families$query == "Rhabdiopteryx"), "family"] <- "Taeniopterygidae"
# genus_families[which(genus_families$query == "Thyas"), "family"] <- "Erebidae"
# genus_families[which(genus_families$query == "Torleya"), "family"] <- "Ephemerellidae"
# genus_families[which(genus_families$query == "Tricyphona"), "family"] <- "Pediciidae"
# genus_families[which(genus_families$query == "Tyrrhenoleuctra"), "family"] <- "Leuctridae"
# genus_families[which(genus_families$query == "Velia"), "family"] <- "Veliidae"
# genus_families[which(genus_families$query == "Bezzia-Gr."), "family"] <- "Ceratopogonidae"
# genus_families[which(genus_families$query == "Chelifera/Hemerodromia"), "family"] <- "Empididae"
# genus_families[which(genus_families$query == "Dicranota (Dicranota)"), "family"] <- "Pediciidae"
# genus_families[which(genus_families$query == "Rhyacophila (Rhyacophila)"), "family"] <- "Rhyacophilidae"
# genus_families[which(genus_families$query %in% c("Simulium (Boophthora)", "Simulium (Eusimulium)", "Simulium (Nevermannia)", "Simulium (Odagmia)", "Simulium (Simulium)", "Simulium (Wilhelmia)") ), "family"] <- "Simuliidae"
# genus_families[which(genus_families$query == "Tipula (Yamatotipula)"), "family"] <- "Tipulidae"
# 
# # clean wrongly corrected family names
# genus_families[which(genus_families$query == "Ampullaceana"), "family"] <- "Lymnaeidae"
# genus_families[which(genus_families$query == "Anisus"), "family"] <- "Planorbidae"
# genus_families[which(genus_families$query %in% c("Pseudolimnophila", "Lipsothrix", "Pilaria", "Paradelphomyia", "Ormosia", "Molophilus", "Limonia", "Limnophila", "Hexatoma", "Antocha", "Gnophomyia", "Austrolimnophila", "Cheilotrichia", "Erioptera", "Helius", "Gonomyia")), "family"] <- "Limoniidae"
# genus_families[which(genus_families$query == "Argyroneta"), "family"] <- "Dictynidae"
# genus_families[which(genus_families$query == "Chloroperla"), "family"] <- "Chloroperlidae"
# genus_families[which(genus_families$query %in% c("Dicranota", "Ula", "Pedicia")), "family"] <- "Pediciidae"
# genus_families[which(genus_families$query == "Brachyptera"), "family"] <- "Taeniopterygidae"
# genus_families[which(genus_families$query == "Hygrobia"), "family"] <- "Hygrobiidae"
# genus_families[which(genus_families$query == "Phalacrocera"), "family"] <- "Cylindrotomidae"
# genus_families[which(genus_families$query == "Trocheta"), "family"] <- "Erpobdellidae"
# genus_families[which(genus_families$query == "Zonitoides"), "family"] <- "Gastrodontidae"
# genus_families[which(genus_families$query == "Cordylophora"), "family"] <- "Cordylophoridae"
# 

# save data set
# save(genus_families, file = "data/genus_families_TREAM.Rdata")
load("data/genus_families_TREAM.Rdata")

# These three entries can not confidently be linked to one family, either because the genus is from a different family, a synonym or doesn't even exist
TREAM[which(TREAM$genus %in% c("Amphimelania", "Crangonyx/Niphargus", "Holostomis")),]

# clean the family level
TREAM[which(TREAM$Family == "Enchytraeidae\xa0"), "Family"] <- "Enchytraeidae"
TREAM[which(TREAM$Family == "Naididae (formerly Tubificidae)"), "Family"] <- "Naididae"
TREAM[which(TREAM$Family == "Naididae/Tubificidae"), "Family"] <- "Naididae"
TREAM[which(TREAM$Family == "Sciomyzidae (=Tetanoceridae)"), "Family"] <- "Sciomyzidae"
TREAM[which(TREAM$Family == "Scirtidae (=Helodidae)"), "Family"] <- "Scirtidae"
TREAM[which(TREAM$Family == "Thaumaleidae (=Orphnephilidae)"), "Family"] <- "Thaumaleidae"
TREAM[which(TREAM$Family == "Anthomyiidae "), "Family"] <- "Anthomyiidae"
TREAM[which(TREAM$Family == "Ancylidae"), "Family"] <- "Planorbidae"
TREAM[which(TREAM$Family == "Corbiculidae"), "Family"] <- "Cyrenidae"
TREAM[which(TREAM$Family == "Tubificidae"), "Family"] <- "Naididae"
TREAM[which(TREAM$Family == "Clavidae"), "Family"] <- "Cordylophoridae"
TREAM[which(TREAM$Family == "Nabididae"), "Family"] <- "Naididae"

# merge data sets and check if the families are different
TREAM <- merge(TREAM, genus_families, by.x = "genus", by.y = "query", all = T)

#replace old family values
TREAM[which(TREAM$Family != TREAM$family), "Family"] <- TREAM[which(TREAM$Family != TREAM$family), "family"]

# remove additional columns
TREAM <- TREAM[, -which(names(TREAM) %in% c("db", "family"))]

# clean genus level
TREAM[which(TREAM$genus %in% c("Anax/Hemianax", "Annitella/Chaetopteryx", "Chelifera/Hemerodromia", "Coleoptera", "Habroleptoides/Paraleptophlebia", "Orthocladiini")) , "genus"] <- NA
TREAM[which(TREAM$genus == "Atherix/Ibisia"), "genus"] <- "Atherix"
TREAM[which(TREAM$genus == "Bezzia-Gr."), "genus"] <- "Bezzia"
TREAM[which(TREAM$genus == "Chironomus (Chironomus)"), "genus"] <- "Chironomus"
TREAM[which(TREAM$genus == "Dicranota (Dicranota)"), "genus"] <- "Dicranota"
TREAM[which(TREAM$genus == "Polypedilum (Polypedilum)"), "genus"] <- "Polypedilum"
TREAM[which(TREAM$genus == "Tipula (Yamatotipula)"), "genus"] <- "Tipula"
TREAM[which(TREAM$genus == "Vejdovskiella"), "genus"] <- "Vejdovskyella"
TREAM[which(TREAM$genus %in% c("Simulium (Boophthora)", "Simulium (Eusimulium)", "Simulium (Nevermannia)", "Simulium (Odagmia)", "Simulium (Simulium)", "Simulium (Wilhelmia)")), "genus"] <- "Simulium"

# Obtain order level ------------
families <- sort(unique(TREAM$Family))

# obtain the order level for every single identification
# families_order <- data.frame()
# for (i in 1:length(families)){
#   temp <- tax_name(families[i], get = "order")
#   families_order <- rbind(families_order, temp)
# }
# 
# #check manually the families for which no order could be found
# families_order[which(families_order$query == "Leuctridae/Capniidae"), "order"] <- "Plecoptera"
# families_order[which(families_order$query == "Platycnemididae"), "order"] <- "Odonata"
# families_order[which(families_order$query == "Enchytraeidae"), "order"] <- "Enchytraeida"
# families_order[which(families_order$query %in% c("Cylindrotomidae", "Limoniidae", "Pediciidae")), "order"] <- "Diptera"
# families_order[which(families_order$query == "Cordylophoridae"), "order"] <- "Anthoathecata"

#save the data set
#save(families_order, file="data/families_orders_TREAM.Rdata")
load("data/families_orders_TREAM.Rdata")

# merge both data sets
TREAM <- merge(TREAM, families_order, by.x = "Family", by.y = "query", all = T)

TREAM <- TREAM[, -which(names(TREAM) %in% c("db"))]

#remove an unclear family name
TREAM[which(TREAM$Family == "Leuctridae/Capniidae"), "Family"] <- NA

# reorder columns in data frame
TREAM <- TREAM %>% relocate(c(Family, genus), .after = Group) %>% relocate(order, .after = Group)

# rename column
names(TREAM)[names(TREAM) == "Family"] <- "family"
names(TREAM)[names(TREAM) == "Group"] <- "group"

# remove rows with NA (almost all cells are empty in this rows)
TREAM <- TREAM[which(!is.na(TREAM$site_id)),]

# save data set
write.csv(TREAM, "data/TREAM_preprocessed.csv", row.names = F)