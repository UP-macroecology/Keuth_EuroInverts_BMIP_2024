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
studysites <- unique(Euro_Invert$study_site)

# Countries ------------
# number of countries
length(unique(Euro_Invert$country)) #22

# Plotting study countries (code obtained from the paper)
world_map <- map("world", plot=FALSE)
world_map$names
getMap()$NAME

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
length(unique(Euro_Invert$study_site)) #43

# Create list with the different countries as elements
Euro_Invert_list <- split(Euro_Invert, Euro_Invert$country)

#number of study sites per country
no_studysites <- lapply(Euro_Invert_list, function(x){length(unique(x$study_site))})
no_studysites <- do.call(rbind, no_studysites)
no_studysites <- as.data.frame(no_studysites)
no_studysites <- tibble::rownames_to_column(no_studysites, "country")

# Plot number of study sites per country
ggplot(no_studysites, aes(x=country, y= V1))+
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

# number of years per study site
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
completeness_plot_country <- completeness_timeseries_countries %>%
    pivot_longer(
      cols = -Year,
      names_to = "country",
      values_to = "sampled",
      values_transform = list(sampled = as.character)
    ) %>%
    arrange(Year, country, sampled)

#Plot the completeness of sampled years per country
ggplot(data = completeness_plot_country, aes(x = country, y = Year)) +
  geom_raster(aes(fill = sampled)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Year") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
  ggtitle("Coverage of sampled years per country")

  
# Proportion of completeness for every country
proportion_completeness_country <- data.frame(country = countries, p.miss = NA, p.pres = NA)
# Calculate proportion of present years and proportion of missing years
for (i in 1:length(countries)) {
  tmp <- subset(completeness_plot, completeness_plot$country == countries[i])
  p_miss <- (mean(is.na(tmp$sampled)) * 100)
  proportion_completeness_country[proportion_completeness_country$country == countries[i], "p.miss"] <- round(p_miss, 1)
  proportion_completeness_country[proportion_completeness_country$country == countries[i], "p.pres"] <- round(100 - p_miss,1)
}

# change data frame to long format
completeness_long <- data.frame(country = rep(proportion_completeness_country$country,2), 
                                proportion = c(proportion_completeness_country$p.miss, proportion_completeness_country$p.pres),
                                type = c(rep("p.miss", nrow(proportion_completeness_country)), rep("p.pres", nrow(proportion_completeness_country))))

# plot results as bar plot
ggplot(data = completeness_long, aes(x = country, y=proportion, fill = type))+
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
for (i in 1:length(studysites)){
  completeness_timeseries_studysite$tmp <- NA
  names(completeness_timeseries_studysite)[names(completeness_timeseries_studysite) == "tmp"] <- studysites[i]
}

# obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
for (k in 1:length(studysites)){
  for (i in 1968:2020) {
    if(i %in% sampling_years_studysite[[studysites[k]]] == T){
      completeness_timeseries_studysite[completeness_timeseries_studysite$Year == i, studysites[k]] <- "Yes"
    }
  }
}

# reorder dataframe to long format (kind of appending columns)
completeness_plot_studysites <- completeness_timeseries_studysite %>%
  pivot_longer(
    cols = -Year,
    names_to = "studysites",
    values_to = "sampled",
    values_transform = list(sampled = as.character)
  ) %>%
  arrange(Year, studysites, sampled)

completeness_plot_studysites <- as.data.frame(completeness_plot_studysites)

# add column with country
completeness_plot_studysites$country <- NA
for (k in 1:length(countries)) {
    row_index <- grep(countries[k],completeness_plot_studysites[,"studysites"])
    completeness_plot_studysites[row_index, "country"] <- countries[k]
}

# Plot results of completeness
ggplot(data = completeness_plot_studysites, aes(x = studysites, y = Year)) +
  geom_raster(aes(fill = sampled)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Year") +
  theme(axis.text.x = element_text(hjust = 1), strip.text = element_text(size = 12))+
  scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
  ggtitle("Coverage of sampled years per study site")+
  facet_wrap(~country, scales = "free_x")

# Proportion of completeness for every study site
proportion_completeness_studysite <- data.frame(studysite = studysites, p.miss = NA, p.pres = NA)
#Calculate proportion of missing and sampled years
for (i in 1:length(studysites)) {
  index_studysite <- studysites[i]
  tmp <- subset(completeness_plot_studysites, completeness_plot_studysites$studysite == index_studysite)
  p_miss <- (mean(is.na(tmp$sampled)) * 100)
  proportion_completeness_studysite[proportion_completeness_studysite$studysite == studysites[i], "p.miss"] <- round(p_miss, 1)
  proportion_completeness_studysite[proportion_completeness_studysite$studysite == studysites[i], "p.pres"] <- round(100 - p_miss,1)
}

# change data frame to long format
completeness_long_studysite <- data.frame(studysite = rep(proportion_completeness_studysite$studysite,2), 
                                proportion = c(proportion_completeness_studysite$p.miss, proportion_completeness_studysite$p.pres),
                                type = c(rep("p.miss", nrow(proportion_completeness_studysite)), rep("p.pres", nrow(proportion_completeness_studysite))))

# add column with country
completeness_long_studysite$country <- NA
for (k in 1:length(countries)) {
  row_index <- grep(countries[k],completeness_long_studysite[,"studysite"])
  completeness_long_studysite[row_index, "country"] <- countries[k]
}

# plot results as bar plot
ggplot(data = completeness_long_studysite, aes(x = studysite, y=proportion, fill = type))+
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
no_records_country <- as.data.frame(table(Euro_Invert$country))

# Plot number of records
ggplot(no_records_country, aes(x=Var1, y= Freq))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of records per country")

# for every study site
no_records_studysite <- as.data.frame(table(Euro_Invert$study_site))

# add column with country
no_records_studysite$country <- NA
for (k in 1:length(countries)) {
  row_index <- grep(countries[k],no_records_studysite[,"Var1"])
  no_records_studysite[row_index, "country"] <- countries[k]
}

# Plot results
ggplot(no_records_studysite, aes(x=Var1, y= Freq))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of records per country")+
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
Euro_Invert$coarser <- str_remove(Euro_Invert$coarser, " ")

#try <- as.data.frame(sort(unique(Euro_Invert$genus)))

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

# Plot number of species per country
ggplot(no_species, aes(x=country, y= V1))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of species")

# number of genus per country
no_genus <- lapply(Euro_Invert_list, function(x){length(unique(x$genus))})
no_genus <- do.call(rbind, no_genus)
no_genus <- as.data.frame(no_genus)
no_genus <- tibble::rownames_to_column(no_genus, "country")

# Plot number of genus per country
ggplot(no_genus, aes(x=country, y= V1))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of genus")

# Proportion of taxon level
# extract level of taxon determination
Euro_Invert$taxon_level <- NA
Euro_Invert[which(!is.na(Euro_Invert$species)), "taxon_level"] <- "s"
Euro_Invert[which(is.na(Euro_Invert$taxon_level) & !is.na(Euro_Invert$genus)), "taxon_level"] <- "g"
Euro_Invert[which(is.na(Euro_Invert$taxon_level)), "taxon_level"] <- "c"

# Calculate proportion of species, genus, coarser identification per country
proportion_taxon_level <- data.frame(country = countries, per_sp = NA, per_g = NA, per_c = NA)
for (i in 1:length(countries)) {
  tmp <- subset(Euro_Invert, Euro_Invert$country == countries[i])
  proportion_taxon_level[proportion_taxon_level$country == countries[i],"per_sp"] <- (length(which(tmp$taxon_level == "s"))/ nrow(tmp))*100
  proportion_taxon_level[proportion_taxon_level$country == countries[i],"per_g"] <- (length(which(tmp$taxon_level == "g"))/ nrow(tmp))*100
  proportion_taxon_level[proportion_taxon_level$country == countries[i],"per_c"] <- (length(which(tmp$taxon_level == "c"))/ nrow(tmp))*100
}

# change data frame to long format
completeness.long_taxon <- data.frame(country = rep(proportion_taxon_level$country,3), 
                                proportion = c(proportion_taxon_level$per_sp, proportion_taxon_level$per_g, proportion_taxon_level$per_c),
                                type = c(rep("per.species", nrow(proportion_taxon_level)), rep("per.genus", nrow(proportion_taxon_level)), 
                                         rep("per.coarser", nrow(proportion_taxon_level))))

# plot results as bar plot
ggplot(data = completeness.long_taxon, aes(x = country, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of taxon level per country")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF", "darkgreen"), labels = c("Coarser", "Species", "Genus"))
