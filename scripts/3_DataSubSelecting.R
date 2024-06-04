# Working on the subselection of the data
# Goal: Obtain a selection of countries which are as spatially continous as possible while also having as much temporal cover as possible

# Loading packages
library(rworldmap)
library(scales)
library(maps)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Loading the data set
Euro_Invert <- read.csv("data/Euro_FreshInv_preprocessed.csv")

# Extract only the data at species-level identification
Euro_Invert <- Euro_Invert[which(!is.na(Euro_Invert$species)),]
Euro_Invert <- Euro_Invert[which(Euro_Invert$year >= 1980),]
countries <- unique(Euro_Invert$country)

#Obtain information regarding temporal cover & species number per country
Euro_Invert_list <- split(Euro_Invert, Euro_Invert$country)

# number of species per country
Euro_Invert_info <- lapply(Euro_Invert_list, function(x){length(unique(x$species))})
Euro_Invert_info <- do.call(rbind, Euro_Invert_info)
Euro_Invert_info <- as.data.frame(Euro_Invert_info)
Euro_Invert_info <- tibble::rownames_to_column(Euro_Invert_info, "country")
colnames(Euro_Invert_info) <- c("country", "no_species")

# temporal cover
sampling_years_countries <- lapply(Euro_Invert_list, function(x){unique(x$year)})

# data frame for completeness of time series for every year
completeness_timeseries <- data.frame(Year = c(1990:2020))

# add a column for every study country
for (i in 1:length(countries)){
  completeness_timeseries$tmp <- NA
  names(completeness_timeseries)[names(completeness_timeseries) == "tmp"] <- countries[i]
}

# obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
for (k in 1:length(countries)){
  for (i in 1990:2020) {
    if(i %in% sampling_years_countries[[countries[k]]] == T){
      completeness_timeseries[completeness_timeseries$Year == i, countries[k]] <- "Yes"
    }
  }
}

# reorder dataframe to long format (kind of appending columns)
completeness_timeseries_long <- completeness_timeseries %>%
  pivot_longer(
    cols = -Year,
    names_to = "country",
    values_to = "sampled",
    values_transform = list(sampled = as.character)
  ) %>%
  arrange(Year, country, sampled)

# Proportion of completeness for every country
Euro_Invert_info$p_years_miss <- NA
Euro_Invert_info$p_years_pres <- NA

# Calculate proportion of present years and proportion of missing years
for (i in 1:length(countries)) {
  tmp <- subset(completeness_timeseries_long, completeness_timeseries_long$country == countries[i])
  p_miss <- (mean(is.na(tmp$sampled)) * 100)
  Euro_Invert_info[Euro_Invert_info$country == countries[i], "p_years_miss"] <- round(p_miss, 1)
  Euro_Invert_info[Euro_Invert_info$country == countries[i], "p_years_pres"] <- round(100 - p_miss,1)
}

proportion_completeness_timeseries_long <- data.frame(country = rep(Euro_Invert_info$country,2), 
                                                                proportion = c(Euro_Invert_info$p_years_miss, Euro_Invert_info$p_years_pres),
                                                                type = c(rep("p_years_miss", nrow(Euro_Invert_info)), rep("p_years_pres", nrow(Euro_Invert_info))))
#calculate coverage value per country
coverage <- lapply(Euro_Invert_list, function(x){(round((nrow(x))/(with(x, length(unique(species)) * length(unique(site_id)) * 30)),2))*100})
coverage <- do.call(rbind, coverage)
coverage <- as.data.frame(coverage)
coverage <- tibble::rownames_to_column(coverage, "country")
colnames(coverage) <- c("country", "coverage")
Euro_Invert_info <- merge(coverage, Euro_Invert_info, by = "country")

# Plots
ggplot(Euro_Invert_info, aes(x=country, y= no_species))+
  geom_bar(stat = "identity")+
  labs(x = "", y = "")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  theme(axis.text.x = element_text(hjust = 0.5))

ggplot(data = completeness_timeseries_long, aes(x = country, y = Year)) +
  geom_raster(aes(fill = sampled)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Year") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
  ggtitle("Coverage of sampled years per country")

ggplot(data = proportion_completeness_timeseries_long, aes(x = country, y=proportion, fill = type))+
  geom_bar(position = "stack", stat = "identity")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
  labs(x = "", y = "Proportion") +
  theme(axis.text.x = element_text(hjust = 0.5))+
  ggtitle("Proportion of missing/ sampled years per country")+
  scale_fill_manual("Proportion of", values = c("#FF6666", "#33CCFF"), labels = c("Missing", "Sampled"))

# Create a map which contains the countries, the time coverage and number of species
world_map <- maps::map("world", plot=FALSE)

ddf <- data.frame(country = c(countries, "United Kingdom", 'Czech Rep.', 'Luxembourg'))

par(mar=c(0,0,0,0))

newmap <- getMap(resolution = "low")

col <- rep("grey85", length(newmap@data$NAME))
col[match(ddf$country, newmap@data$NAME)] <- c("black") #or pal instead of c("grey80")

col2 <- rep("#00000000", length(newmap@data$NAME)) #or alpha("white",0) instead of #00000000
col2[match(ddf$country, newmap@data$NAME)] <- c("grey50")

col2[58:59] <- c("black")#change borders for N.Cyprus
wv<-seq(1, 1, length.out=253) 
wv[58:59] <- c(0)

# extract coordinates for countries for plotting text in the worldmap
countries_points <- newmap@data[which(newmap@data$SOVEREIGNT %in% c(countries, "United Kingdom", 'Czech Republic', 'Luxembourg')), c("SOVEREIGNT", "LON", "LAT")]
countries_points <- subset(countries_points, countries_points$LON > -10 & countries_points$LON < 34)
countries_points <- subset(countries_points, countries_points$LAT > 35 & countries_points$LAT < 70)
countries_points <- countries_points[-c(1,13,15,17,20),]
countries_points$SOVEREIGNT <- as.character(countries_points$SOVEREIGNT)
countries_points[which(countries_points$SOVEREIGNT == "Czech Republic"), "SOVEREIGNT"] <- "CzechRepublic"
countries_points[which(countries_points$SOVEREIGNT == "Luxembourg"), "SOVEREIGNT"] <- "Luxemburg"
countries_points[which(countries_points$SOVEREIGNT == "United Kingdom"), "SOVEREIGNT"] <- "UK"
Euro_Invert_info <- merge(countries_points, Euro_Invert_info, by.x = "SOVEREIGNT", by.y = "country")

# Plot map with text
plot(newmap,col=col,
     bg="white",border=col2,
     xlim = c(-10, 34),
     ylim = c(35, 70),
     asp = 1,lwd=wv
)

text(Euro_Invert_info$LON, Euro_Invert_info$LAT, paste0("sp:", Euro_Invert_info$no_species), col = "red", cex = 0.7)
text(Euro_Invert_info$LON, Euro_Invert_info$LAT, paste0("time:", Euro_Invert_info$p_years_pres, "%"), col = "red", pos = 3, cex = 0.7)
text(Euro_Invert_info$LON, Euro_Invert_info$LAT, paste0("cov:", Euro_Invert_info$coverage, "%"), col = "red", pos = 1, cex = 0.7)

# Work on preselection of the countries





# Select those countries, which have a amount of identification at species level which is above 15 % and a sampling rate of above 20%
countries <- Euro_Invert_info_countries[which(Euro_Invert_info_countries$p_spec >= 15 & Euro_Invert_info_countries$p_years_pres >= 20), "country"]

# Plot the country selection on a map to make a further pre selection geographicwise
# Plotting study countries (code obtained from the paper)
world_map <- maps::map("world", plot=FALSE)

ddf <- data.frame(country = c(countries, "United Kingdom"))

par(mar=c(0,0,0,0))

newmap <- getMap(resolution = "low")

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
text(x = -17, y = 70, "1980 = 0.003", cex = 2)

# Looking spatially I would test if I should remove them Portugal, Bulgaria, Lativa, Estonia

countries_spatial_remove1 <- data.frame(V1 = "Portugal", V2 = "Bulgaria", V3 = "Latvia", V4 = "Estonia")
countries_spatial_remove2 <- as.data.frame(combn(countries_spatial_remove1, 2))
countries_spatial_remove2[nrow(countries_spatial_remove2)+2,] <- NA
countries_spatial_remove3 <- as.data.frame(combn(countries_spatial_remove1, 3))
countries_spatial_remove3[nrow(countries_spatial_remove3)+1,] <- NA
countries_spatial_remove4 <- as.data.frame(combn(countries_spatial_remove1, 4))
countries_spatial_remove1[nrow(countries_spatial_remove1)+3,] <- NA
countries_spatial_remove <- cbind(countries_spatial_remove1, countries_spatial_remove2)
countries_spatial_remove <- cbind(countries_spatial_remove, countries_spatial_remove3)
countries_spatial_remove <- cbind(countries_spatial_remove, countries_spatial_remove4)

startYears <- c(1980, 1985, 1990)

# Testing data completeness

# loading in data
Euro_Invert <- read.csv("data/Euro_FreshInv_preprocessed.csv")

# coverage list
df_list <- vector("list", ncol(countries_spatial_remove))
#df_list <- lapply(df_list, function(x){x <- data.frame("countries" = rep(NA, 14), "1980" = rep(NA, 14), "1985" = rep(NA, 14), "1990" = rep(NA, 14)); return(x) })

for (k in 1:ncol(countries_spatial_remove)) {
  selection <- countries[!countries %in% countries_spatial_remove[,k]]
  df_list[[k]] <- data.frame("countries" = selection)
  for (i in 1:3) {
    startYear <- startYears[i]
    subData <- subset(Euro_Invert, country %in% selection & year >= startYear)
    coverage <- (nrow(subData))/(with(subData, length(unique(species)) * length(unique(site_id)) * length(unique(year))))
    if(startYear == 1980){
      df_list[[k]][, "X1980"] <- coverage
    } else if(startYear == 1985) {
      df_list[[k]][, "X1985"] <- coverage
    } else {
      df_list[[k]][, "X1990"] <- coverage
    }
  }
}
pdf("plots/Maps_coverage.pdf")
for (i in 1:15) {
ddf <- data.frame(country = c(df_list[[i]]$countries, "United Kingdom"))

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
text(x = -5, y = 69, paste0("1980: ", round(unique(df_list[[i]]$X1980), 4), "\n1985: ", round(unique(df_list[[i]]$X1985), 4) , "\n1990: ", round(unique(df_list[[i]]$X1990), 4)), cex = 1)
}
dev.off()

library(dplyr)
test2 <- Euro_Invert %>% group_by(species) %>% count()
test <- Euro_Invert %>% group_by(species) %>% summarise(no_years = n_distinct(year), no_countries = n_distinct(country)) %>% 
  full_join(test2, by = join_by(species)) %>% mutate(time_cov = no_years/31, spatial_cov = no_countries/21, coverage = n/(31 * 21))

full_join(test, test2)
