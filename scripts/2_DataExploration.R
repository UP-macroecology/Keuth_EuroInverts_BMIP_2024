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

# loading in data
Euro_Invert <- read.csv("data/Euro_FreshInv_preprocessed.csv")
# study countries
countries <- unique(Euro_Invert$country)
# study sites
#study_sites <- unique(Euro_Invert$study_site)

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

# # Completeness of time series per study site
# # Create list with the different study sites as elements
# Euro_Invert_list_studysite <- split(Euro_Invert, Euro_Invert$study_site)
# 
# # Sampled years in every study site
# sampling_years_studysite <- lapply(Euro_Invert_list_studysite, function(x){unique(x$year)})
# 
# # data frame for completeness of time series for every year
# completeness_timeseries_studysite <- data.frame(Year = c(1968:2020))
# 
# # add a column for every study site
# for (i in 1:length(study_sites)){
#   completeness_timeseries_studysite$tmp <- NA
#   names(completeness_timeseries_studysite)[names(completeness_timeseries_studysite) == "tmp"] <- study_sites[i]
# }
# 
# # obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
# for (k in 1:length(study_sites)){
#   for (i in 1968:2020) {
#     if(i %in% sampling_years_studysite[[study_sites[k]]] == T){
#       completeness_timeseries_studysite[completeness_timeseries_studysite$Year == i, study_sites[k]] <- "Yes"
#     }
#   }
# }
# 
# # reorder dataframe to long format (kind of appending columns)
# completeness_timeseries_studysites_long <- completeness_timeseries_studysite %>%
#   pivot_longer(
#     cols = -Year,
#     names_to = "study_sites",
#     values_to = "sampled",
#     values_transform = list(sampled = as.character)
#   ) %>%
#   arrange(Year, study_sites, sampled)
# 
# completeness_timeseries_studysites_long <- as.data.frame(completeness_timeseries_studysites_long)
# 
# # add column with country
# completeness_timeseries_studysites_long$country <- NA
# for (k in 1:length(countries)) {
#     row_index <- grep(countries[k],completeness_timeseries_studysites_long[,"study_sites"])
#     completeness_timeseries_studysites_long[row_index, "country"] <- countries[k]
# }
# 
# # Plot results of completeness
# ggplot(data = completeness_timeseries_studysites_long, aes(x = study_sites, y = Year)) +
#   geom_raster(aes(fill = sampled)) + 
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
#   labs(x = "", y = "Year") +
#   theme(axis.text.x = element_text(hjust = 1), strip.text = element_text(size = 12))+
#   scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
#   ggtitle("Coverage of sampled years per study site")+
#   facet_wrap(~country, scales = "free_x")
# 
# # Proportion of completeness for every study site
# Euro_Invert_info_studysites <- data.frame(study_site = study_sites, p_years_miss = NA, p_years_pres = NA)
# #Calculate proportion of missing and sampled years
# for (i in 1:length(study_sites)) {
#   index_studysite <- study_sites[i]
#   tmp <- subset(completeness_timeseries_studysites_long, completeness_timeseries_studysites_long$study_site == index_studysite)
#   p_miss <- (mean(is.na(tmp$sampled)) * 100)
#   Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i], "p_years_miss"] <- round(p_miss, 1)
#   Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i], "p_years_pres"] <- round(100 - p_miss,1)
#   rm(p_miss, index_studysite, tmp)
# }
# 
# # change data frame to long format
# proportion_completeness_timeseries_studysites_long <- data.frame(study_site = rep(Euro_Invert_info_studysites$study_site,2), 
#                                 proportion = c(Euro_Invert_info_studysites$p_years_miss, Euro_Invert_info_studysites$p_years_pres),
#                                 type = c(rep("p.miss", nrow(Euro_Invert_info_studysites)), rep("p.pres", nrow(Euro_Invert_info_studysites))))
# 
# # add column with country
# proportion_completeness_timeseries_studysites_long$country <- NA
# for (k in 1:length(countries)) {
#   row_index <- grep(countries[k], proportion_completeness_timeseries_studysites_long[,"study_site"])
#   proportion_completeness_timeseries_studysites_long[row_index, "country"] <- countries[k]
# }
# 
# # plot results as bar plot
# ggplot(data = proportion_completeness_timeseries_studysites_long, aes(x = study_site, y=proportion, fill = type))+
#   geom_bar(position = "stack", stat = "identity")+
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
#   labs(x = "", y = "Proportion") +
#   theme(axis.text.x = element_text(hjust = 0.5))+
#   ggtitle("Proportion of missing/ sampled years per country")+
#   scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF"), labels = c("Missing", "Sampled"))+
#   facet_wrap(~country, scales = "free_x")

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

# # for every study site
# Euro_Invert_info_studysites <- merge(Euro_Invert_info_studysites, table(Euro_Invert$study_site), by.x = "study_site", by.y = "Var1")
# colnames(Euro_Invert_info_studysites)[colnames(Euro_Invert_info_studysites) == "Freq"] <- "no_records"
# 
# # add column with country
# Euro_Invert_info_studysites$country <- NA
# for (k in 1:length(countries)) {
#   row_index <- grep(countries[k],Euro_Invert_info_studysites[,"study_site"])
#   Euro_Invert_info_studysites[row_index, "country"] <- countries[k]
# }
# 
# # Plot results
# ggplot(Euro_Invert_info_studysites, aes(x=study_sites, y= no_records))+
#   geom_bar(stat = "identity")+
#   labs(x = "", y = "")+
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
#   theme(axis.text.x = element_text(hjust = 0.5))+
#   ggtitle("Number of records per study site")+
#   facet_wrap(~country, scales = "free_x")

# Taxa ----------
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

# #Number of species & genus per study_site
# Euro_Invert_list_studysite <- split(Euro_Invert, Euro_Invert$study_site)
# 
# # number of species per study site
# no_species <- lapply(Euro_Invert_list_studysite, function(x){length(unique(x$species))})
# no_species <- do.call(rbind, no_species)
# no_species <- as.data.frame(no_species)
# no_species <- tibble::rownames_to_column(no_species, "study_site")
# Euro_Invert_info_studysites <- merge(Euro_Invert_info_studysites, no_species, by = "study_site")
# colnames(Euro_Invert_info_studysites)[colnames(Euro_Invert_info_studysites) == "V1"] <- "no_species"
# 
# # Plot number of species per study site
# ggplot(Euro_Invert_info_studysites, aes(x=study_sites, y= no_species))+
#   geom_bar(stat = "identity")+
#   labs(x = "", y = "")+
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
#   theme(axis.text.x = element_text(hjust = 0.5))+
#   ggtitle("Number of species per study site")+
#   facet_wrap(~country, scales = "free_x")
# 
# # number of genus per study site
# no_genus <- lapply(Euro_Invert_list_studysite, function(x){length(unique(x$genus))})
# no_genus <- do.call(rbind, no_genus)
# no_genus <- as.data.frame(no_genus)
# no_genus <- tibble::rownames_to_column(no_genus, "study_site")
# Euro_Invert_info_studysites <- merge(Euro_Invert_info_studysites, no_genus, by = "study_site")
# colnames(Euro_Invert_info_studysites)[colnames(Euro_Invert_info_studysites) == "V1"] <- "no_genus"
# 
# # Plot number of genus per study site
# ggplot(Euro_Invert_info_studysites, aes(x=study_sites, y= no_genus))+
#   geom_bar(stat = "identity")+
#   labs(x = "", y = "")+
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
#   theme(axis.text.x = element_text(hjust = 0.5))+
#   ggtitle("Number of genus per study site")+
#   facet_wrap(~country, scales = "free_x")

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

# # Calculate proportion of species, genus, coarser identification per study site
# Euro_Invert_info_studysites$p_spec <- NA
# Euro_Invert_info_studysites$p_gen <- NA
# Euro_Invert_info_studysites$p_cors <- NA
# for (i in 1:length(study_sites)) {
#   tmp <- subset(Euro_Invert, Euro_Invert$study_site == study_sites[i])
#   Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i],"p_spec"] <- (length(which(tmp$taxon_level == "s"))/ nrow(tmp))*100
#   Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i],"p_gen"] <- (length(which(tmp$taxon_level == "g"))/ nrow(tmp))*100
#   Euro_Invert_info_studysites[Euro_Invert_info_studysites$study_site == study_sites[i],"p_cors"] <- (length(which(tmp$taxon_level == "c"))/ nrow(tmp))*100
# }
# 
# # change data frame to long format
# proportion_completeness_taxon_studysites_long <- data.frame(study_site = rep(Euro_Invert_info_studysites$study_site,3), 
#                                       proportion = c(Euro_Invert_info_studysites$p_spec, Euro_Invert_info_studysites$p_gen, Euro_Invert_info_studysites$p_cors),
#                                       type = c(rep("per.species", nrow(Euro_Invert_info_studysites)), rep("per.genus", nrow(Euro_Invert_info_studysites)), 
#                                                rep("per.coarser", nrow(Euro_Invert_info_studysites))))
# 
# # add column with country
# proportion_completeness_taxon_studysites_long$country <- NA
# for (k in 1:length(countries)) {
#   row_index <- grep(countries[k], proportion_completeness_taxon_studysites_long[,"study_site"])
#   proportion_completeness_taxon_studysites_long[row_index, "country"] <- countries[k]
# }
# 
# # plot results as bar plot
# ggplot(data = proportion_completeness_taxon_studysites_long, aes(x = study_site, y=proportion, fill = type))+
#   geom_bar(position = "stack", stat = "identity")+
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
#   labs(x = "", y = "Proportion") +
#   theme(axis.text.x = element_text(hjust = 0.5))+
#   ggtitle("Proportion of taxon level per country")+
#   scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF", "darkgreen"), labels = c("Coarser", "Species", "Genus"))+
#   facet_wrap(~country, scales = "free_x")

#save information data sets
write.csv(Euro_Invert_info_countries, "data/Euro_FreshInv_information_country.csv", row.names = F)
#write.csv(Euro_Invert_info_studysites, "data/Euro_FreshInv_information_studysite.csv", row.names = F)
#write.csv(Euro_Invert, "data/Euro_FreshInv_all_sites_split_taxa.csv", row.names = F)

# Species level completeness ----

# number of detections per species
Euro_Invert_info_species <- table(Euro_Invert$species)
Euro_Invert_info_species <- as.data.frame(Euro_Invert_info_species)
colnames(Euro_Invert_info_species) <- c("species", "no_detections")

# Plot number of records
ggplot(Euro_Invert_info_species, aes(x=species, y= no_detections))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of records per species")

# obtain range and quantile of detections
hist(Euro_Invert_info_species$no_detections)
boxplot(Euro_Invert_info_species$no_detections)
summary(Euro_Invert_info_species$no_detections)

# remove species with just one detection
#Euro_Invert_info_species <- subset(Euro_Invert_info_species, no_detections > 15)

# number of countries per species (spatial coverage of species)
Euro_Invert_list_species <- split(Euro_Invert, Euro_Invert$species)
no_countries_species <- lapply(Euro_Invert_list_species, function(x){length(unique(x$country))})
no_countries_species <- do.call(rbind, no_countries_species)
no_countries_species <- as.data.frame(no_countries_species)
no_countries_species <- tibble::rownames_to_column(no_countries_species, "species")
Euro_Invert_info_species <- merge(Euro_Invert_info_species, no_countries_species, by = "species")
colnames(Euro_Invert_info_species)[colnames(Euro_Invert_info_species) == "V1"] <- "no_countries"

# Plot number of countries per species (how often was the species detected spatially)
ggplot(Euro_Invert_info_species, aes(x=species, y= no_countries))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of countries per species")

# time coverage of species (temporal coverage of species)
# Sampled years for every species
sampling_years_species <- lapply(Euro_Invert_list_species, function(x){unique(x$year)})

# data frame for completeness of time series for every year
completeness_timeseries_species <- data.frame(Year = c(1968:2020))

Euro_Invert_info_species$species <- as.character(Euro_Invert_info_species$species)
species <- unique(Euro_Invert_info_species$species)
# add a column for every study country
for (i in 1:length(species)){
  completeness_timeseries_species$tmp <- NA
  names(completeness_timeseries_species)[names(completeness_timeseries_species) == "tmp"] <- species[i]
}

# obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
for (k in 1:length(species)){
  for (i in 1968:2020) {
    if(i %in% sampling_years_species[[species[k]]] == T){
      completeness_timeseries_species[completeness_timeseries_species$Year == i, species[k]] <- "Yes"
    }
  }
}

# Plotting completeness of time series for every species
# code for the plot modified from the visdat package 
# actual function: vis_dat(df)

# reorder dataframe to long format (kind of appending columns)
completeness_timeseries_species_long <- completeness_timeseries_species %>%
  pivot_longer(
    cols = -Year,
    names_to = "species",
    values_to = "sampled",
    values_transform = list(sampled = as.character)
  ) %>%
  arrange(Year, species, sampled)

#Plot the completeness of sampled years per species
ggplot(data = completeness_timeseries_species_long, aes(x = species, y = Year)) +
  geom_raster(aes(fill = sampled)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Year") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
  ggtitle("Coverage of sampled years per species")


# Proportion of completeness for every species
Euro_Invert_info_species$p_years_miss <- NA
Euro_Invert_info_species$p_years_pres <- NA

# Calculate proportion of present years and proportion of missing years
for (i in 1:length(species)) {
  tmp <- subset(completeness_timeseries_species_long, completeness_timeseries_species_long$species == species[i])
  p_miss <- (mean(is.na(tmp$sampled)) * 100)
  Euro_Invert_info_species[Euro_Invert_info_species$species == species[i], "p_years_miss"] <- round(p_miss, 1)
  Euro_Invert_info_species[Euro_Invert_info_species$species == species[i], "p_years_pres"] <- round(100 - p_miss,1)
}

# change data frame to long format
proportion_completeness_timeseries_species_long <- data.frame(species = rep(Euro_Invert_info_species$species,2), 
                                                                proportion = c(Euro_Invert_info_species$p_years_miss, Euro_Invert_info_species$p_years_pres),
                                                                type = c(rep("p_years_miss", nrow(Euro_Invert_info_species)), rep("p_years_pres", nrow(Euro_Invert_info_species))))

# plot results as bar plot
ggplot(data = proportion_completeness_timeseries_species_long, aes(x = species, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of missing/ sampled years per species")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF"), labels = c("Missing", "Sampled"))

# add number of sampled years per species
completeness_timeseries_species_long_list <- split(completeness_timeseries_species_long, completeness_timeseries_species_long$species)
no_sampledyears_species <- lapply(completeness_timeseries_species_long_list, function(x){length(which(!is.na(x$sampled)))})
no_sampledyears_species <- do.call(rbind, no_sampledyears_species)
no_sampledyears_species <- as.data.frame(no_sampledyears_species)
no_sampledyears_species <- tibble::rownames_to_column(no_sampledyears_species, "species")
Euro_Invert_info_species <- merge(Euro_Invert_info_species, no_sampledyears_species, by = "species")
colnames(Euro_Invert_info_species)[colnames(Euro_Invert_info_species) == "V1"] <- "no_sampledyears"

# create presentable and meaningful plots
boxplot(Euro_Invert_info_species$no_detections, ylab = "Number of detections per species")
boxplot(Euro_Invert_info_species$no_countries, ylab = "Number of countries per species")
boxplot(Euro_Invert_info_species$no_sampledyears, ylab = "Number of sampled years per species")

#save information data sets
write.csv(Euro_Invert_info_species, "data/Euro_FreshInv_information_species.csv", row.names = F)
