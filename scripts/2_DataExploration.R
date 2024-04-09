# 2. Exploring Data set
# Getting an overview of the coverage of taxa, countries, study sites and the time frame

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
# Create list with the different countries as elements
Euro_Invert_list <- split(Euro_Invert, Euro_Invert$country)

# Study Sites and Countries ------------
# number of countries
length(unique(Euro_Invert$country)) #22

# Plotting study countries (code obtained from the paper)
unique(Euro_Invert$country)

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
     bg="white",border=col2, #bg="lightblue",border="grey70",
     xlim = c(-10, 34),
     ylim = c(35, 70),
     asp = 1,lwd=wv
)

# number of study sites in total and per country
length(unique(Euro_Invert$study_site)) #43
no_studysites <- lapply(Euro_Invert_list, function(x){length(unique(x$study_site))})
no_studysites <- do.call(rbind, no_studysites)
no_studysites <- as.data.frame(no_studysites)
no_studysites <- tibble::rownames_to_column(no_studysites, "country")

ggplot(no_studysites, aes(x=country, y= V1))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))

# Time coverage -------------
# time span and length of observation years
range(Euro_Invert$year) #start and end year
max(Euro_Invert$year) - min(Euro_Invert$year) #number of years
lapply(Euro_Invert_list, function(x){r_years <- range(x$year); l_years <- max(x$year) - min(x$year); return(c(r_years, l_years))}) #same for each country

# Completeness of time series ---------
# number of years per study site
lapply(Euro_Invert_list, function(x){(length(unique(x$year)))})

# Sampled years in every country
sampling_years_countries <- lapply(Euro_Invert_list, function(x){unique(x$year)})

# data frame for completeness of time series for every year
completeness_timeseries_countries <- data.frame(Year = c(1968:2020))

# study countries
countries <- unique(Euro_Invert$country)

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

# reorder dataframe (i.e. kind of appending columns)
completeness_plot <- completeness_timeseries_countries %>%
    pivot_longer(
      cols = -Year,
      names_to = "country",
      values_to = "sampled",
      values_transform = list(sampled = as.character)
    ) %>%
    arrange(Year, country, sampled)

ggplot(data = completeness_plot, aes(x = country, y = Year)) +
    geom_raster(aes(fill = sampled)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
    labs(x = "", y = "Year") +
    theme(axis.text.x = element_text(hjust = 0.5))+
    scale_fill_manual(values = c("#FF6666", "#33CCFF"))

  
# Proportion of completeness for every country
proportion_completeness_country <- data.frame(country = countries, p.miss = NA, p.pres = NA)
for (i in 1:length(countries)) {
  tmp <- subset(completeness_plot, completeness_plot$country == countries[i])
  p_miss <- (mean(is.na(tmp$sampled)) * 100)
  proportion_completeness_country[proportion_completeness_country$country == countries[i], "p.miss"] <- round(p_miss, 1)
  proportion_completeness_country[proportion_completeness_country$country == countries[i], "p.pres"] <- round(100 - p_miss,1)
}

#present results as bar plot
# change data frame to long format
completeness.long <- data.frame(country = rep(proportion_completeness_country$country,2), 
                                proportion = c(proportion_completeness_country$p.miss, proportion_completeness_country$p.pres),
                                type = c(rep("p.miss", nrow(proportion_completeness_country)), rep("p.pres", nrow(proportion_completeness_country))))

# plot results as bar plot
ggplot(data = completeness.long, aes(x = country, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))

# Taxa ----------

# prepare species, genus and family
Euro_Invert[which(Euro_Invert$taxon == "\"Ancylidae\" Gen. sp."),"taxon"] <- "Ancylidae Gen. sp." #fix misspelling"
Euro_Invert[which(Euro_Invert$taxon == "Eukiefferiella ceigr."),"taxon"] <- "Eukiefferiella sp." #fix misspelling"
Euro_Invert[which(Euro_Invert$taxon == "Thienemannimyia , gen. indet."),"taxon"] <- "Thienemannimyia sp." #fix misspelling"
Euro_Invert[which(Euro_Invert$taxon == "Corbicula \"fluminalis\""),"taxon"] <- "Corbicula fluminalis"

#Euro_Invert[which(Euro_Invert$taxon == "Eukiefferiella ceigr,"),"taxon"] <- "Ancylidae Gen. sp." #fix misspelling"
#remove abbreviations ?to subspecies?
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " Lv.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " Ad.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " ssp.")
Euro_Invert$taxon <- str_replace(Euro_Invert$taxon, "-", " -")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " -gr.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " -agg.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " -Gr.")
Euro_Invert$taxon <- str_remove(Euro_Invert$taxon, " -Agg.")

#try <- as.data.frame(sort(unique(Euro_Invert$taxon)))

#remove wrong capitalization
Euro_Invert$taxon <- str_to_lower(Euro_Invert$taxon)
Euro_Invert$taxon <- firstup(Euro_Invert$taxon)

#replicate taxa column to species, genus, family and order
Euro_Invert$species <- Euro_Invert$taxon
Euro_Invert$genus <- Euro_Invert$taxon
Euro_Invert$family <- Euro_Invert$taxon
Euro_Invert$order <- Euro_Invert$taxon

# extract cells with "sp."
row_index <- grep("sp.",Euro_Invert[,"species"])
Euro_Invert[row_index, "species"] <- NA

#extract cells with / in species
row_index <- grep("/",Euro_Invert[,"species"])
Euro_Invert[row_index, "species"] <- NA

#extract cells with gen. sp. in species
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

#remove families
row_index <- grep("gen. sp.",Euro_Invert[,"genus"])
Euro_Invert[row_index, "genus"] <- NA

#remove 2nd word
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

#try <- as.data.frame(sort(unique(Euro_Invert$genus)))

# Overall number of species & genus
length(unique(Euro_Invert$genus))
length(unique(Euro_Invert$species))

#Number of species & genus per country
Euro_Invert_list <- split(Euro_Invert, Euro_Invert$country)
no_species <- lapply(Euro_Invert_list, function(x){length(unique(x$species))})
no_species <- do.call(rbind, no_species)
no_species <- as.data.frame(no_species)
no_species <- tibble::rownames_to_column(no_species, "country")
ggplot(no_species, aes(x=country, y= V1))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of species")

no_genus <- lapply(Euro_Invert_list, function(x){length(unique(x$genus))})
no_genus <- do.call(rbind, no_genus)
no_genus <- as.data.frame(no_genus)
no_genus <- tibble::rownames_to_column(no_genus, "country")
ggplot(no_genus, aes(x=country, y= V1))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Number of genus")

