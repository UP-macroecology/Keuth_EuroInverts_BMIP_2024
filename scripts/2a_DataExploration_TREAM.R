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
# calculate number of years per country and study site and merge them
no_years_countries <- TREAM %>% group_by(country) %>% summarise(no_years_c = n_distinct(year))
no_years_studysites <- TREAM %>% group_by(country, site_id) %>% summarise(no_years = n_distinct(year)) %>% full_join(no_years_countries, by = join_by(country)) %>% 
  mutate(per_cov_years = (no_years/no_years_c) * 100, site_id = as.character(site_id))


# Plot the proportion of sampled years per study sites for each country
ggplot(data=no_years_studysites, aes(x = reorder(site_id, no_years), y = no_years))+
  geom_bar(stat = "identity")+
  facet_wrap(~ country, scales = "free_x")+
  xlab("Study Site")+
  ggtitle("Sampled years per study site")+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  ylab("")
