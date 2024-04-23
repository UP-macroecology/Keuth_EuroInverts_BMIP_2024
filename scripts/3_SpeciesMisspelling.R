# Control for misspelled species names or synonyms

# Load packages
library(taxize)
#library(help = taxize)

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

Euro_Invert[which(Euro_Invert$species == "Vejdovskiella comata"), "species"] <- "Vejdovskyella comata"
Euro_Invert[which(Euro_Invert$species == "Vejdovskiella intermedia"), "species"] <- "Vejdovskyella intermedia"
Euro_Invert[which(Euro_Invert$species == "Orthocladiini cop"), "coarser"] <- "Chironomidae"
Euro_Invert[which(Euro_Invert$species == "Orthocladiini cop"), "species"] <- "NA"
Euro_Invert[which(Euro_Invert$species == "Orthocladiini cop"), "genus"] <- "NA"
Euro_Invert[which(Euro_Invert$species == "Holostomis atrata"), "genus"] <- "NA"
Euro_Invert[which(Euro_Invert$species == "Holostomis atrata"), "species"] <- "NA"
Euro_Invert[which(Euro_Invert$species == "Holostomis phalaenoides"), "genus"] <- "NA"
Euro_Invert[which(Euro_Invert$species == "Holostomis phalaenoides"), "species"] <- "NA"


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

write.table(tmp, "data/species_na.txt", sep = ",", col.names = T, row.names = F)
# correcting names if Basionym is a different one
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
