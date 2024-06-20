# 2. Exploring Data set
# Getting an overview of the coverage of taxa, countries, study sites and the time
# Code is similar to the old data set but adapted to the TREAM data set

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

# load in data
TREAM <- read.csv("data/TREAM_preprocessed.csv")

# obtain countries
countries <- unique(TREAM$country)

# Create list with the different countries as elements
TREAM_list <- split(TREAM, TREAM$country)

# Study Sites ------

# number of study sites in total and per country
length(unique(TREAM$site_id)) #1816

#number of study sites per country
TREAM_info_countries <- TREAM %>% group_by(country) %>% summarize(no_studysites = n_distinct(site_id))

# Plot number of study sites per country
ggplot(TREAM_info_countries, aes(x=country, y= no_studysites))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))

# Time span and length ------

# time span and length of observation years
range(TREAM$year) #start and end year
max(TREAM$year) - min(TREAM$year) #number of years
# same for every country
lapply(TREAM_list, function(x){r_years <- range(x$year); l_years <- max(x$year) - min(x$year); return(c(r_years, l_years))})

# number of years per country
TREAM %>% group_by(country) %>% summarize(no_years = n_distinct(year))

# Completeness of time series ---------
# Sampled years in every country
sampling_years_countries <- lapply(TREAM_list, function(x){unique(x$year)})

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
TREAM_info_countries$p_years_miss <- NA
TREAM_info_countries$p_years_pres <- NA

# Calculate proportion of present years and proportion of missing years
for (i in 1:length(countries)) {
  tmp <- subset(completeness_timeseries_countries_long, completeness_timeseries_countries_long$country == countries[i])
  p_miss <- (mean(is.na(tmp$sampled)) * 100)
  TREAM_info_countries[TREAM_info_countries$country == countries[i], "p_years_miss"] <- round(p_miss, 1)
  TREAM_info_countries[TREAM_info_countries$country == countries[i], "p_years_pres"] <- round(100 - p_miss,1)
}

# change data frame to long format
proportion_completeness_timeseries_countries_long <- data.frame(country = rep(TREAM_info_countries$country,2), 
                                                                proportion = c(TREAM_info_countries$p_years_miss, TREAM_info_countries$p_years_pres),
                                                                type = c(rep("p_years_miss", nrow(TREAM_info_countries)), rep("p_years_pres", nrow(TREAM_info_countries))))

# plot results as bar plot
ggplot(data = proportion_completeness_timeseries_countries_long, aes(x = country, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of missing/ sampled years per country")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF"), labels = c("Missing", "Sampled"))

# Number of records ---------

# for every country
TREAM_info_countries <- merge(TREAM_info_countries, table(TREAM$country), by.x = "country", by.y = "Var1")
colnames(TREAM_info_countries)[colnames(TREAM_info_countries) == "Freq"] <- "no_records"

# Plot number of records
ggplot(TREAM_info_countries, aes(x=country, y= no_records))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of records per country")

# Number of taxa -------------
# Calculate proportion of species, genus, coarser identification per country
TREAM_info_countries$p_spec <- NA
TREAM_info_countries$p_gen <- NA
TREAM_info_countries$p_cors <- NA
for (i in 1:length(countries)) {
  tmp <- subset(TREAM, TREAM$country == countries[i])
  TREAM_info_countries[TREAM_info_countries$country == countries[i],"p_spec"] <- (length(which(tmp$taxon_level == "s"))/ nrow(tmp))*100
  TREAM_info_countries[TREAM_info_countries$country == countries[i],"p_gen"] <- (length(which(tmp$taxon_level == "g"))/ nrow(tmp))*100
  TREAM_info_countries[TREAM_info_countries$country == countries[i],"p_cors"] <- (length(which(tmp$taxon_level == "c"))/ nrow(tmp))*100
}

# change data frame to long format
proportion_completeness_taxon_countries_long <- data.frame(country = rep(TREAM_info_countries$country,3), 
                                                           proportion = c(TREAM_info_countries$p_spec, TREAM_info_countries$p_gen, TREAM_info_countries$p_cors),
                                                           type = c(rep("per.species", nrow(TREAM_info_countries)), rep("per.genus", nrow(TREAM_info_countries)), 
                                                                    rep("per.coarser", nrow(TREAM_info_countries))))

# plot results as bar plot
ggplot(data = proportion_completeness_taxon_countries_long, aes(x = country, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of taxon level per country")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF", "darkgreen"), labels = c("Coarser", "Genus", "Species"))

# study site turnover per country -----
TREAM <- read.csv("data/TREAM_preprocessed.csv")
TREAM$date <- paste(TREAM$year, TREAM$month,TREAM$day,  sep = "-")
TREAM$date <- as.Date(TREAM$date, format = "%Y-%m-%d")
TREAM <- TREAM[!is.na(TREAM$date),]
TREAM_list <- split(TREAM, TREAM$site_id)

TREAM_long <- lapply(TREAM_list, function(x){
  df_long <- as.data.frame(sort(unique(x$date)));
  names(df_long) <- c("Date");
  species <- na.exclude(unique(x$taxon));
  if(length(species) != 0){
  for (i in 1:length(species)){
    df_long$tmp <- NA
    names(df_long)[names(df_long) == "tmp"] <- species[i]
  };
  for (k in 1:length(species)){
    for (i in 1:length(unique(df_long$Date))) {
      tmp <- x[which(x$taxon == species[k]), ]
        if(length(x[which(x$date == unique(df_long$Date)[i] & x$taxon == species[k]), "abundance"]) == 1)
        df_long[df_long$Date == unique(df_long$Date)[i], species[k]] <- sum(x[which(x$date == unique(df_long$Date)[i] & x$taxon == species[k]), "abundance"])
    }
  };
  df_long[is.na(df_long)] <- 0};
  return(df_long)
})

TREAM_long <- lapply(TREAM_long, function(x){
  if(ncol(x) > 1){
    x <- x %>% pivot_longer(!Date, names_to = "taxon", values_to = "abundances");
    x <- as.data.frame(x)};
  return(x)
})

tmp <- lapply(TREAM_long, function(x){which(ncol(x) == 1)})
test <- as.data.frame(do.call(rbind, tmp))
test <- tibble::rownames_to_column(test, "site_id")

test2 <- TREAM_long[-c(test$site_id)]
TREAM_long[ncol(TREAM_long)>1]

#stopped working on the problem of removing certain elements of the list

# TREAM_long <- vector("list", length(TREAM_list))
# for(pos in 1:length(TREAM_list)){
#   x <- TREAM_list[[pos]]
#   df_long <- as.data.frame(sort(unique(x$date)))
#   names(df_long) <- c("Date")
#   species <- na.exclude(unique(x$taxon))
#   if(length(species) != 0){
#     for (i in 1:length(species)){
#       df_long$tmp <- NA
#       names(df_long)[names(df_long) == "tmp"] <- species[i]
#     }
#     for (k in 1:length(species)){
#       for (i in 1:length(unique(df_long$Date))) {
#         tmp <- x[which(x$taxon == species[k]), ]
#         if(unique(df_long$Date)[i] %in% unique(tmp$date) == T){
#           print(unique(df_long$Date)[i])
#           print(species[k])
#           df_long[df_long$Date == unique(df_long$Date)[i], species[k]] <- x[which(x$date == unique(df_long$Date)[i] & x$taxon == species[k]), "abundance"]
#         }
#       }
#     }
#     df_long[is.na(df_long)] <- 0}
#   TREAM_long[[pos]] <- df_long
# }
# 
 x[which(x$date == "2013-10-01" & x$taxon == "Polycentropus flavomaculatus"), "abundance"]






study_site_full <- as.data.frame(sort(unique(study_site1$date)))
names(study_site_full) <- c("Date")

species <- unique(study_site1$taxon)
species <- na.exclude(species)
for (i in 1:length(species)){
  study_site_full$tmp <- NA
  names(study_site_full)[names(study_site_full) == "tmp"] <- species[i]
}

for (k in 1:length(species)){
  for (i in 1:length(unique(study_site_full$Date))) {
    tmp <- study_site1[which(study_site1$taxon == species[k]), ]
    if(unique(study_site_full$Date)[i] %in% unique(tmp$date) == T){
      study_site_full[study_site_full$Date == unique(study_site_full$Date)[i], species[k]] <- "1"
    }
  }
}

study_site_full[is.na(study_site_full)] <- 0



tmp <- TREAM %>% group_by(site_id) %>% do(as.data.frame(sort(unique(.$date)))) 
tmp <- TREAM %>% group_by(site_id, date, taxon) %>% do(as.data.frame(sort(.$abundance))) 
tmp <- TREAM[, c("site_id", "date", "taxon", "abundance")] %>% filter(!is.na(taxon))

tmp <- tmp[order(tmp$site_id, tmp$date),]
tmp2 <- tmp %>%
  pivot_wider(names_from = taxon, values_from = abundance)

tmp2 <- TREAM %>% group_by(site_id) %>% pivot_wider(names_from = taxon, values_from = abundance)

tmp <- study_site1 %>% group_by(date) %>% pivot_wider(names_from = taxon, values_from = abundance)

rename("Date" = "sort(unique(.$date))") %>% 
  do(for (i in 1:length(species)){.$tmp <- NA
  names(study_site_full)[names(study_site_full) == "tmp"] <- species[i]})

study_site1 <- TREAM_list[[1]]



study_site_long <- study_site_full %>%
  pivot_longer(cols = 2:38, names_to = "taxon", values_to = "abundance")

# continue to work on the expansion of the data set and including structural 0
