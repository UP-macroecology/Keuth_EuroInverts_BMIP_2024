# 2. Exploring Data set
# Getting an overview of the coverage of taxa, countries, study sites and the time frame

# Loading packages
library(RColorBrewer)
library(rworldmap)
library(scales)
library(maps)
library(stringr)
library(ggplot2))

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

# Sampled years for every country
sampling_years_countries <- lapply(Euro_Invert_list, function(x){unique(x$year)})


completeness_years_countries <- data.frame(Year = c(1968:2020))

countries <- unique(Euro_Invert$country)
for (i in 1:length(countries)){
  completeness$test <- NA
  names(completeness)[names(completeness) == "test"] <- countries[i]
}

for (k in 1:length(countries)){
for (i in 1968:2020) {
  if(i %in% years_countries[[countries[k]]] == T){
    completeness[completeness$Year == i, countries[k]] <- "Yes"
  }
  }
}

# code for the plot modified from the visdat package
#vis_dat(completeness)

try <- completeness %>%
    tidyr::pivot_longer(
      cols = -Year,
      names_to = "variable",
      values_to = "valueType",
      values_transform = list(valueType = as.character)
    ) %>%
    dplyr::arrange(Year, variable, valueType)

  ggplot2::ggplot(data = try,
                  ggplot2::aes(x = variable,
                               y = Year,
                               )) +
    ggplot2::geom_raster(ggplot2::aes(fill = valueType)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       vjust = 1,
                                                       hjust = 1)) +
    ggplot2::labs(x = "",
                  y = "Year") +
    # flip the axes
    ggplot2::scale_y_reverse() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::guides(colour = "none")



# Taxa ----------
# split species and genus
Euro_Invert[,c("genus", "species")] <- str_split_fixed(Euro_Invert$taxon, " ", 2)
unique(Euro_Invert$genus)
