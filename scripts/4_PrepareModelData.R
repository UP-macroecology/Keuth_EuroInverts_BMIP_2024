# Preparation of abundance time series of the pilot species for BMIP
# Data should be given in two spatial resolutions (matching to the climate data): 1km, 10km

# Load packages
library(terra)
library(dplyr)
#library(rnaturalearth)
#library(rnaturalearthdata)

# Read in data
TREAM <- read.csv("data/TREAM_gooddata_harmonised_unit.csv", header = T) #only data after 1990 and study sites with more than 20 years of sampling

# extract pilot species
pilot_species_df <- subset(TREAM, TREAM$binomial == "Agapetus ochripes")

# Transform the data into a spatial object to reproject it
pilot_species <- vect(pilot_species_df, geom=c("Longitude_X", "Latitude_Y"))
pilot_species <- project(pilot_species, "EPSG:3035")

# extract the reprojected coordinates from the spatial data frame
coords <- as.data.frame(crds(pilot_species))

# merge with original data frame
pilot_species_df <- cbind(pilot_species_df, coords)

# create a spatraster object that has the same extent and crs as europe
mask_1km <- rast(ext(2479357.46078533, 6496360.77332511, 999661.06552765, 5337308.89627766), resolution = 1000, 
             crs = "EPSG:3035") # 1km resolution
mask_10km <- rast(ext(2479357.46078533, 6496360.77332511, 999661.06552765, 5337308.89627766), resolution = 10000, 
                 crs = "EPSG:3035") # 10km resolution

# Extract the cell numbers & coordinates from the study sites for the 1km resolution
cells_1km <- cellFromXY(mask_1km, cbind(pilot_species_df$x, pilot_species_df$y))
cell_coords_1km <- xyFromCell(mask_1km, cells_1km)

# merge the data to the abundance data set
pilot_species_df_1km <- pilot_species_df %>% mutate(cell = cells_1km) %>% 
  mutate(cell_x = cell_coords_1km[,1],
         cell_y = cell_coords_1km[,2]) %>%
  # clean data set
  select(year, Species = binomial, siteID = site_id, country, abundance = abundance_new, unit = unit_new, cell, cell_x, cell_y)

# check if different study sites are within the same cell
result <- pilot_species_df_1km %>%
  group_by(cell) %>%
  summarise(
    n_unique_studies = n_distinct(siteID),
    siteIDs = paste(unique(siteID), collapse = ", ")
  ) %>%
  filter(n_unique_studies > 1)

print(result)

# split data into training and validation data
pilot_species_training <- pilot_species_df_1km %>% 
  filter(year <= 2011)

pilot_species_testing <- pilot_species_df_1km %>% 
  filter(year > 2012)

# save both data sets
write.csv(pilot_species_training, file = "data/European_aquatic_invert_Agapetus_ochripes_1990_2011_1km.csv", row.names = F)
write.csv(pilot_species_testing, file = "data/European_aquatic_invert_Agapetus_ochripes_2012_2020_1km.csv", row.names = F)

# Extract the cell numbers & coordinates from the study sites for the 10km resolution
cells_10km <- cellFromXY(mask_10km, cbind(pilot_species_df$x, pilot_species_df$y))
cell_coords_10km <- xyFromCell(mask_10km, cells_10km)

# merge the data to the abundance data set
pilot_species_df_10km <- pilot_species_df %>% mutate(cell = cells_10km) %>% 
  mutate(cell_x = cell_coords_10km[,1],
         cell_y = cell_coords_10km[,2]) %>%
  # clean data set
  select(year, Species = binomial, siteID = site_id, country, abundance = abundance_new, unit = unit_new, cell, cell_x, cell_y)

# check if different study sites are within the same cell
result <- pilot_species_df_10km %>%
  group_by(cell) %>%
  summarise(
    n_unique_studies = n_distinct(siteID),
    siteIDs = paste(unique(siteID), collapse = ", ")
  ) %>%
  filter(n_unique_studies > 1)

print(result)

# split data into training and validation data
pilot_species_training <- pilot_species_df_10km %>% 
  filter(year <= 2011)

pilot_species_testing <- pilot_species_df_10km %>% 
  filter(year > 2012)

# save both data sets
write.csv(pilot_species_training, file = "data/European_aquatic_invert_Agapetus_ochripes_1990_2011_10km.csv", row.names = F)
write.csv(pilot_species_testing, file = "data/European_aquatic_invert_Agapetus_ochripes_2012_2020_10km.csv", row.names = F)

# 
# # I now transform my spatvector object with the points into a spatraster
# r_points <- rasterize(pilot_species, mask, field = "abundance", fun = sum, na.rm = TRUE)
# 
# plot(europe)
# points(as.points(r_points), col = "red", cex = 0.3)
# 
# # create the 10km raster
# # I now create a spatraster object that has the same extent and crs as europe
# mask_10km <- rast(ext(europe), resolution = 10000, crs = crs(europe)) # 10km resolution
# 
# # I now transform my spatvector object with the points into a spatraster
# r_points_10km <- rasterize(pilot_species, mask_10km, field = "abundance", fun = sum, na.rm = TRUE)
# 
# plot(europe)
# points(as.points(r_points_10km), col = "red", cex = 0.3)
# 
# # I download data of the world, transform it into a spatvector object and crop it and project it into equal area projection
# world_map <- ne_countries(scale = 50, returnclass = 'sf')
# world_map <- vect(world_map)
# new_extent <- ext(-10, 34, 32, 70.06484375)
# europe <- crop(world_map, new_extent)
# europe <- project(europe, "EPSG:3035")
# 
# v_lonlat <- as.polygons(e_lonlat, crs = "EPSG:4326")
# e_proj <- project(new_extent, "EPSG:3035")
# ext(europe)
# 
# # I now have the pilot_species and the map of europe both as spatvector and with the same crs
