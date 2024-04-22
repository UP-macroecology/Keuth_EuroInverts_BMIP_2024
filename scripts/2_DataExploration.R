# 2. Exploring Data set
# Getting an overview of the coverage of taxa, countries, study sites and the time

# Loading packages
library(RColorBrewer)
library(rworldmap)
library(scales)
library(maps)
library(stringr)
library(ggplot2)
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

# loading in data
Euro_Invert <- read.csv("data/Euro_FreshInv_all_sites.csv")
# study countries
countries <- unique(Euro_Invert$country)
# study sites
study_sites <- unique(Euro_Invert$study_site)

# Countries ------------
# number of countries
length(unique(Euro_Invert$country)) #22

# Plotting study countries (code obtained from the paper)
world_map <- map("world", plot=FALSE)

ddf = read.table(text="
country
'France' 
'Estonia'
'Cyprus'
'N. Cyprus'
'Spain'
'Sweden'
'Luxembourg'
'Bulgaria'
'Denmark'
'Germany'
'United Kingdom'
'Ireland'
'Norway'
'Czech Rep.'
'Italy'
'Hungary'
'Portugal'
'Austria'
'Netherlands'
'Switzerland'
'Finland'
'Latvia'
'Belgium'", header=TRUE)

par(mar=c(0,0,0,0))

newmap <- getMap(resolution = "high")

col <- rep("grey85", length(newmap@data$NAME))
col[match(ddf$country, newmap@data$NAME)] <- c("black") #or pal instead of c("grey80")

col2 <- rep("#00000000", length(newmap@data$NAME)) #or alpha("white",0) instead of #00000000
col2[match(ddf$country, newmap@data$NAME)] <- c("grey50")

col2[58:59] <- c("black")#change borders for N.Cyprus
wv<-seq(1, 1, length.out=253) 
wv[58:59] <- c(0)

plot(newmap,col=col,
     bg="white",border=col2,
     xlim = c(-10, 34),
     ylim = c(35, 70),
     asp = 1,lwd=wv
)

# Study Sites ------

# number of study sites in total and per country
length(unique(Euro_Invert$study_site)) #1816

# Create list with the different countries as elements
Euro_Invert_list <- split(Euro_Invert, Euro_Invert$country)

#number of study sites per country
Euro_Invert_info_countries <- lapply(Euro_Invert_list, function(x){length(unique(x$study_site))})
Euro_Invert_info_countries <- do.call(rbind, Euro_Invert_info_countries)
Euro_Invert_info_countries <- as.data.frame(Euro_Invert_info_countries)
Euro_Invert_info_countries <- tibble::rownames_to_column(Euro_Invert_info_countries, "country")
colnames(Euro_Invert_info_countries) <- c("country", "no_studysites")

# Plot number of study sites per country
ggplot(Euro_Invert_info_countries, aes(x=country, y= no_studysites))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))

# Time span and length ------

# time span and length of observation years
range(Euro_Invert$year) #start and end year
max(Euro_Invert$year) - min(Euro_Invert$year) #number of years
# same for every country
lapply(Euro_Invert_list, function(x){r_years <- range(x$year); l_years <- max(x$year) - min(x$year); return(c(r_years, l_years))})

# number of years per country
lapply(Euro_Invert_list, function(x){(length(unique(x$year)))})

# Completeness of time series ---------

# Sampled years in every country
sampling_years_countries <- lapply(Euro_Invert_list, function(x){unique(x$year)})

# data frame for completeness of time series for every year
completeness_timeseries_countries <- data.frame(Year = c(1968:2020))

# add a column for every study country
for (i in 1:length(countries)){
  completeness_timeseries_countries$tmp <- NA
  names(completeness_timeseries_countries)[names(completeness_timeseries_countries) == "tmp"] <- countries[i]
}

# obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
for (k in 1:length(countries)){
  for (i in 1968:2020) {
    if(i %in% sampling_years_countries[[countries[k]]] == T){
      completeness_timeseries_countries[completeness_timeseries_countries$Year == i, countries[k]] <- "Yes"
    }
  }
}

# Plotting completeness of time series for every country
# code for the plot modified from the visdat package 
# actual function: vis_dat(df)

# reorder dataframe to long format (kind of appending columns)
completeness_timeseries_countries_long <- completeness_timeseries_countries %>%
    pivot_longer(
      cols = -Year,
      names_to = "country",
      values_to = "sampled",
      values_transform = list(sampled = as.character)
    ) %>%
    arrange(Year, country, sampled)

#Plot the completeness of sampled years per country
ggplot(data = completeness_timeseries_countries_long, aes(x = country, y = Year)) +
  geom_raster(aes(fill = sampled)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Year") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
  ggtitle("Coverage of sampled years per country")

  
# Proportion of completeness for every country
Euro_Invert_info_countries$p_years_miss <- NA
Euro_Invert_info_countries$p_years_pres <- NA

# Calculate proportion of present years and proportion of missing years
for (i in 1:length(countries)) {
  tmp <- subset(completeness_timeseries_countries_long, completeness_timeseries_countries_long$country == countries[i])
  p_miss <- (mean(is.na(tmp$sampled)) * 100)
  Euro_Invert_info_countries[Euro_Invert_info_countries$country == countries[i], "p_years_miss"] <- round(p_miss, 1)
  Euro_Invert_info_countries[Euro_Invert_info_countries$country == countries[i], "p_years_pres"] <- round(100 - p_miss,1)
}

# change data frame to long format
proportion_completeness_timeseries_countries_long <- data.frame(country = rep(Euro_Invert_info_countries$country,2), 
                                proportion = c(Euro_Invert_info_countries$p_years_miss, Euro_Invert_info_countries$p_years_pres),
                                type = c(rep("p_years_miss", nrow(Euro_Invert_info_countries)), rep("p_years_pres", nrow(Euro_Invert_info_countries))))

# plot results as bar plot
ggplot(data = proportion_completeness_timeseries_countries_long, aes(x = country, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of missing/ sampled years per country")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF"), labels = c("Missing", "Sampled"))

# Completeness of time series per study site
# Create list with the different study sites as elements
Euro_Invert_list_studysite <- split(Euro_Invert, Euro_Invert$study_site)

# Sampled years in every study site
sampling_years_studysite <- lapply(Euro_Invert_list_studysite, function(x){unique(x$year)})

# data frame for completeness of time series for every year
completeness_timeseries_studysite <- data.frame(Year = c(1968:2020))

# add a column for every study site
for (i in 1:length(study_sites)){
  completeness_timeseries_studysite$tmp <- NA
  names(completeness_timeseries_studysite)[names(completeness_timeseries_studysite) == "tmp"] <- study_sites[i]
}

# obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
for (k in 1:length(study_sites)){
  for (i in 1968:2020) {
    if(i %in% sampling_years_studysite[[study_sites[k]]] == T){
      completeness_timeseries_studysite[completeness_timeseries_studysite$Year == i, study_sites[k]] <- "Yes"
    }
  }
}

# reorder dataframe to long format (kind of appending columns)
completeness_timeseries_studysites_long <- completeness_timeseries_studysite %>%
  pivot_longer(
    cols = -Year,
    names_to = "study_sites",
    values_to = "sampled",
    values_transform = list(sampled = as.character)
  ) %>%
  arrange(Year, study_sites, sampled)

completeness_timeseries_studysites_long <- as.data.frame(completeness_timeseries_studysites_long)

# add column with country
completeness_timeseries_studysites_long$country <- NA
for (k in 1:length(countries)) {
    row_index <- grep(countries[k],completeness_timeseries_studysites_long[,"study_sites"])
    completeness_timeseries_studysites_long[row_index, "country"] <- countries[k]
}

# Plot results of completeness
ggplot(data = completeness_timeseries_studysites_long, aes(x = study_sites, y = Year)) +
  geom_raster(aes(fill = sampled)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Year") +
  theme(axis.text.x = element_text(hjust = 1), strip.text = element_text(size = 12))+
  scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
  ggtitle("Coverage of sampled years per study site")+
  facet_wrap(~country, scales = "free_x")

# Proportion of completeness for every study site
Euro_Invert_info_studysites <- data.frame(study_site = study_sites, p_years_miss = NA, p_years_pres = NA)
#Calculate proportion of missing and sampled years
for (i in 1:length(study_sites)) {
  index_studysite <- study_sites[i]
  tmp <- subset(completeness_timeseries_studysites_long, completeness_timeseries_studysites_long$study_site == index_studysite)
  p_miss <- (mean(is.na(tmp$sampled)) * 100)
  Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i], "p_years_miss"] <- round(p_miss, 1)
  Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i], "p_years_pres"] <- round(100 - p_miss,1)
  rm(p_miss, index_studysite, tmp)
}

# change data frame to long format
proportion_completeness_timeseries_studysites_long <- data.frame(study_site = rep(Euro_Invert_info_studysites$study_site,2), 
                                proportion = c(Euro_Invert_info_studysites$p_years_miss, Euro_Invert_info_studysites$p_years_pres),
                                type = c(rep("p.miss", nrow(Euro_Invert_info_studysites)), rep("p.pres", nrow(Euro_Invert_info_studysites))))

# add column with country
proportion_completeness_timeseries_studysites_long$country <- NA
for (k in 1:length(countries)) {
  row_index <- grep(countries[k], proportion_completeness_timeseries_studysites_long[,"study_site"])
  proportion_completeness_timeseries_studysites_long[row_index, "country"] <- countries[k]
}

# plot results as bar plot
ggplot(data = proportion_completeness_timeseries_studysites_long, aes(x = study_site, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of missing/ sampled years per country")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF"), labels = c("Missing", "Sampled"))+
  facet_wrap(~country, scales = "free_x")

# Number of records ---------

# for every country
Euro_Invert_info_countries <- merge(Euro_Invert_info_countries, table(Euro_Invert$country), by.x = "country", by.y = "Var1")
colnames(Euro_Invert_info_countries)[colnames(Euro_Invert_info_countries) == "Freq"] <- "no_records"

# Plot number of records
ggplot(Euro_Invert_info_countries, aes(x=country, y= no_records))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of records per country")

# for every study site
Euro_Invert_info_studysites <- merge(Euro_Invert_info_studysites, table(Euro_Invert$study_site), by.x = "study_site", by.y = "Var1")
colnames(Euro_Invert_info_studysites)[colnames(Euro_Invert_info_studysites) == "Freq"] <- "no_records"

# add column with country
Euro_Invert_info_studysites$country <- NA
for (k in 1:length(countries)) {
  row_index <- grep(countries[k],Euro_Invert_info_studysites[,"study_site"])
  Euro_Invert_info_studysites[row_index, "country"] <- countries[k]
}

# Plot results
ggplot(Euro_Invert_info_studysites, aes(x=study_sites, y= no_records))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of records per study site")+
  facet_wrap(~country, scales = "free_x")

# Taxa ----------

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

#try <- as.data.frame(sort(unique(Euro_Invert$coarser)))

# Overall number of species & genus
length(unique(Euro_Invert$genus))
length(unique(Euro_Invert$species))

#Number of species & genus per country
Euro_Invert_list <- split(Euro_Invert, Euro_Invert$country)

# number of species per country
no_species <- lapply(Euro_Invert_list, function(x){length(unique(x$species))})
no_species <- do.call(rbind, no_species)
no_species <- as.data.frame(no_species)
no_species <- tibble::rownames_to_column(no_species, "country")
Euro_Invert_info_countries <- merge(Euro_Invert_info_countries, no_species, by = "country")
colnames(Euro_Invert_info_countries)[colnames(Euro_Invert_info_countries) == "V1"] <- "no_species"

# Plot number of species per country
ggplot(Euro_Invert_info_countries, aes(x=country, y= no_species))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of species per country")

# number of genus per country
no_genus <- lapply(Euro_Invert_list, function(x){length(unique(x$genus))})
no_genus <- do.call(rbind, no_genus)
no_genus <- as.data.frame(no_genus)
no_genus <- tibble::rownames_to_column(no_genus, "country")
Euro_Invert_info_countries <- merge(Euro_Invert_info_countries, no_genus, by = "country")
colnames(Euro_Invert_info_countries)[colnames(Euro_Invert_info_countries) == "V1"] <- "no_genus"

# Plot number of genus per country
ggplot(Euro_Invert_info_countries, aes(x=country, y= no_genus))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of genus per study site")

#Number of species & genus per study_site
Euro_Invert_list_studysite <- split(Euro_Invert, Euro_Invert$study_site)

# number of species per study site
no_species <- lapply(Euro_Invert_list_studysite, function(x){length(unique(x$species))})
no_species <- do.call(rbind, no_species)
no_species <- as.data.frame(no_species)
no_species <- tibble::rownames_to_column(no_species, "study_site")
Euro_Invert_info_studysites <- merge(Euro_Invert_info_studysites, no_species, by = "study_site")
colnames(Euro_Invert_info_studysites)[colnames(Euro_Invert_info_studysites) == "V1"] <- "no_species"

# Plot number of species per study site
ggplot(Euro_Invert_info_studysites, aes(x=study_sites, y= no_species))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of species per study site")+
  facet_wrap(~country, scales = "free_x")

# number of genus per study site
no_genus <- lapply(Euro_Invert_list_studysite, function(x){length(unique(x$genus))})
no_genus <- do.call(rbind, no_genus)
no_genus <- as.data.frame(no_genus)
no_genus <- tibble::rownames_to_column(no_genus, "study_site")
Euro_Invert_info_studysites <- merge(Euro_Invert_info_studysites, no_genus, by = "study_site")
colnames(Euro_Invert_info_studysites)[colnames(Euro_Invert_info_studysites) == "V1"] <- "no_genus"

# Plot number of genus per study site
ggplot(Euro_Invert_info_studysites, aes(x=study_sites, y= no_genus))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of genus per study site")+
  facet_wrap(~country, scales = "free_x")

# Proportion of taxon level
# extract level of taxon determination
Euro_Invert$taxon_level <- NA
Euro_Invert[which(!is.na(Euro_Invert$species)), "taxon_level"] <- "s"
Euro_Invert[which(is.na(Euro_Invert$taxon_level) & !is.na(Euro_Invert$genus)), "taxon_level"] <- "g"
Euro_Invert[which(is.na(Euro_Invert$taxon_level)), "taxon_level"] <- "c"

# Calculate proportion of species, genus, coarser identification per country
Euro_Invert_info_countries$p_spec <- NA
Euro_Invert_info_countries$p_gen <- NA
Euro_Invert_info_countries$p_cors <- NA
for (i in 1:length(countries)) {
  tmp <- subset(Euro_Invert, Euro_Invert$country == countries[i])
  Euro_Invert_info_countries[Euro_Invert_info_countries$country == countries[i],"p_spec"] <- (length(which(tmp$taxon_level == "s"))/ nrow(tmp))*100
  Euro_Invert_info_countries[Euro_Invert_info_countries$country == countries[i],"p_gen"] <- (length(which(tmp$taxon_level == "g"))/ nrow(tmp))*100
  Euro_Invert_info_countries[Euro_Invert_info_countries$country == countries[i],"p_cors"] <- (length(which(tmp$taxon_level == "c"))/ nrow(tmp))*100
}

# change data frame to long format
proportion_completeness_taxon_countries_long <- data.frame(country = rep(Euro_Invert_info_countries$country,3), 
                                proportion = c(Euro_Invert_info_countries$p_spec, Euro_Invert_info_countries$p_gen, Euro_Invert_info_countries$p_cors),
                                type = c(rep("per.species", nrow(Euro_Invert_info_countries)), rep("per.genus", nrow(Euro_Invert_info_countries)), 
                                         rep("per.coarser", nrow(Euro_Invert_info_countries))))

# plot results as bar plot
ggplot(data = proportion_completeness_taxon_countries_long, aes(x = country, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of taxon level per country")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF", "darkgreen"), labels = c("Coarser", "Species", "Genus"))

# Calculate proportion of species, genus, coarser identification per study site
Euro_Invert_info_studysites$p_spec <- NA
Euro_Invert_info_studysites$p_gen <- NA
Euro_Invert_info_studysites$p_cors <- NA
for (i in 1:length(study_sites)) {
  tmp <- subset(Euro_Invert, Euro_Invert$study_site == study_sites[i])
  Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i],"p_spec"] <- (length(which(tmp$taxon_level == "s"))/ nrow(tmp))*100
  Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i],"p_gen"] <- (length(which(tmp$taxon_level == "g"))/ nrow(tmp))*100
  Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i],"p_cors"] <- (length(which(tmp$taxon_level == "c"))/ nrow(tmp))*100
}

# change data frame to long format
proportion_completeness_taxon_studysites_long <- data.frame(study_site = rep(Euro_Invert_info_studysites$study_site,3), 
                                      proportion = c(Euro_Invert_info_studysites$p_spec, Euro_Invert_info_studysites$p_gen, Euro_Invert_info_studysites$p_cors),
                                      type = c(rep("per.species", nrow(Euro_Invert_info_studysites)), rep("per.genus", nrow(Euro_Invert_info_studysites)), 
                                               rep("per.coarser", nrow(Euro_Invert_info_studysites))))

# add column with country
proportion_completeness_taxon_studysites_long$country <- NA
for (k in 1:length(countries)) {
  row_index <- grep(countries[k], proportion_completeness_taxon_studysites_long[,"study_site"])
  proportion_completeness_taxon_studysites_long[row_index, "country"] <- countries[k]
}

# plot results as bar plot
ggplot(data = proportion_completeness_taxon_studysites_long, aes(x = study_site, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of taxon level per country")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF", "darkgreen"), labels = c("Coarser", "Species", "Genus"))+
  facet_wrap(~country, scales = "free_x")

#save information data sets
write.csv(Euro_Invert_info_countries, "data/Euro_FreshInv_information_country.csv", row.names = F)
write.csv(Euro_Invert_info_studysites, "data/Euro_FreshInv_information_studysite.csv", row.names = F)
write.csv(Euro_Invert, "data/Euro_FreshInv_all_sites_split_taxa.csv", row.names = F)
