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
lapply(Euro_Invert_list, function(x){length(unique(x$study_site))})

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
    if(i %in% years_countries[[countries[k]]] == T){
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
# split species and genus
Euro_Invert[,c("genus", "species")] <- str_split_fixed(Euro_Invert$taxon, " ", 2)
unique(Euro_Invert$genus)
