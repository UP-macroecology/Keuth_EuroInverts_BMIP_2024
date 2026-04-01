# Prepare the data from the pilot species for BMIP
# at 1 and 10km resolution

# Load packages
library(terra)
library(rworldmap)

# Read in data
TREAM <- read.csv("data/TREAM_gooddata.csv", header = T)
site_GPS <- read.csv("data/TREAM/TREAM_siteLevel.csv")

# extract modelled species
pilot_data <- subset(TREAM, TREAM$binomial == "Agapetus ochripes")

# add GPS locations to the data
pilot_data <- merge(pilot_data, site_GPS[,c("site_id", "Longitude_X", "Latitude_Y", "unit")], by = "site_id")

test <- vect(pilot_data[,c(17:18,14)],geom=c("Longitude_X", "Latitude_Y"))
test2 <- project(test, "EPSG:3035")

plot(test)

newmap <- getMap(resolution = "low")
new_extent <- ext(-10, 34, 28, 70.06484375)
europe_vect_cropped <- crop(r, new_extent)
plot(newmap, bg="white", xlim = c(-10, 34), ylim = c(35, 70), asp = 1,lwd=wv)

r <- vect(newmap)

v <- project(europe_vect_cropped, "EPSG:3035")
plot(v)

r2 <- rast(
  ext(v),        # match extent of vector
  resolution = 1000,  # 1 km grid
  crs = crs(v)
)

r_out <- rasterize(test2, r2, field = "abundance")
summary(r_out)
plot(r_out)
plot(!is.na(r_out))

plot(ext(r_out), col="red")
plot(ext(test2), add=TRUE, col="blue")

crs(test2)
crs(r_out)


r <- rast(test2, resolution = 1000)
r_out <- rasterize(r, r2, field = "abundance")
