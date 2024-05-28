# Working on the subselection of the data
# Goal: obtain a complete dataset as possible

# Loading packages
library(rworldmap)
library(scales)
library(maps)

# Making pre selections regarding potential countries, that might be suitable for the data selection
Euro_Invert_info_countries <- read.csv("data/Euro_FreshInv_information_country.csv")

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
