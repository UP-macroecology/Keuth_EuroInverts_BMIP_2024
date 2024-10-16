# maps for presentation

#Load packages
library(rworldmap)
library(RColorBrewer)

# Create overall plot of the study area (country and study sites)

study_sites <- read.csv("data/TREAM/TREAM_siteLevel.csv")

# Plotting study countries (code obtained from the paper)
world_map <- maps::map("world", plot=FALSE)

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
points(study_sites$Longitude_X, study_sites$Latitude_Y, col='deepskyblue1',  pch=19, cex = 1.2)

# Plot with just the countries and study sites in our good data set

TREAM <- read.csv("data/TREAM_gooddata.csv")

# extract countries to adapt in the plot
length(unique(TREAM$binomial))

study_sites_good <- data.frame(site_id = unique(TREAM$site_id))

study_sites_good <- merge(study_sites_good, study_sites[,c("site_id", "Longitude_X", "Latitude_Y")], by = "site_id")

# Plotting study countries (code obtained from the paper)
world_map <- maps::map("world", plot=FALSE)

ddf = read.table(text="
country
'France' 
'Spain'
'Sweden'
'Denmark'
'United Kingdom'
'Netherlands'
'Latvia'
'Portugal'", header=TRUE)

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
points(study_sites_good$Longitude_X, study_sites_good$Latitude_Y, col='deepskyblue1',  pch=19, cex = 1.2)
