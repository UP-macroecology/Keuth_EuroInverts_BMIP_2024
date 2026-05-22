# Preparation of abundance time series of the pilot species for BMIP
# Data should be given in two spatial resolutions (matching to the climate data): 1km, 10km

# Load packages
library(terra)
library(dplyr)
#library(rnaturalearth)
#library(rnaturalearthdata)

# Read in data
TREAM <- read.csv("data/TREAM_gooddata_harmonised_unit.csv", header = T) #only data after 1990 and study sites with more than 20 years of sampling

# Selected 18 species
## Small declining: Leuctra geniculata, Simulium noelleri

## Medium declining: Athripsodes aterrimus, Prodiamesa olivacea

## Large declining: Baetis vernus, Hydropsyche angustipennis

## Small increasing: Simulium angustitarse, Simulium trifasciatum

## Medium increasing: Brachyptera risi, Nigrobaetis niger

## Large increasing: Baetis rhodani, Ephemera danica

## Small stable: Anabolia furcata, Sialis nigripes

## Medium stable: Anabolia nervosa, Kageronia fuscogrisea

## Large stable: Platambus maculatus, Polycentropus flavomaculatus


# prepare vector with selected 18 species
selected_species <- c("Leuctra geniculata", "Simulium noelleri", "Athripsodes aterrimus", "Prodiamesa olivacea", "Baetis vernus", "Hydropsyche angustipennis", 
                      "Simulium angustitarse", "Simulium trifasciatum", "Brachyptera risi", "Nigrobaetis niger", "Baetis rhodani", "Ephemera danica", "Anabolia furcata", 
                      "Sialis nigripes", "Anabolia nervosa", "Kageronia fuscogrisea", "Platambus maculatus", "Polycentropus flavomaculatus")


# Investigate the selected species
for (i in 1:length(selected_species)){
  print(selected_species[i])
  
  TREAM_sub <- subset(TREAM, TREAM$binomial == selected_species[i])
  print("Range of Years:")
  print(range(TREAM_sub$year))
  print(paste("Number of occurrences:", nrow(TREAM_sub)))
  print(paste("Number of abundance counts", nrow(TREAM_sub[TREAM_sub$abundance_new > 0, ])))
}

  # extract pilot species
  species_df <- subset(TREAM, TREAM$binomial %in% selected_species)
  
  # Transform the data into a spatial object to reproject it
  species <- vect(species_df, geom=c("Longitude_X", "Latitude_Y"))
  crs(species) <- "EPSG:4326"
  species <- terra::project(species, "EPSG:3035")
  
  # extract the reprojected coordinates from the spatial data frame
  coords <- as.data.frame(crds(species))
  
  # merge with original data frame
  species_df <- cbind(species_df, coords)
  
  # create a spatraster object that has the same extent and crs as europe
  mask_1km <- rast(ext(2479357.46078533, 6496360.77332511, 999661.06552765, 5337308.89627766), resolution = 1000, 
                   crs = "EPSG:3035") # 1km resolution
  mask_10km <- rast(ext(2479357.46078533, 6496360.77332511, 999661.06552765, 5337308.89627766), resolution = 10000, 
                    crs = "EPSG:3035") # 10km resolution
  
  # Extract the cell numbers & coordinates from the study sites for the 1km resolution
  cells_1km <- cellFromXY(mask_1km, cbind(species_df$x, species_df$y))
  cell_coords_1km <- xyFromCell(mask_1km, cells_1km)
  
  # merge the data to the abundance data set
  species_df_1km <- species_df %>% mutate(cell = cells_1km) %>% 
    mutate(cell_x = cell_coords_1km[,1],
           cell_y = cell_coords_1km[,2]) %>%
    # clean data set
    select(year, Species = binomial, siteID = site_id, country, abundance = abundance_new, unit = unit_new, cell, cell_x, cell_y)
  
  # split data into training and validation data
  species_training <- species_df_1km %>% 
    filter(year <= 2011)
  
  species_testing <- species_df_1km %>% 
    filter(year > 2012)
  
  # save both data sets
  write.csv(species_training, file = "data/European_aquatic_invert_1990_2011_1km_site_level.csv", row.names = F)
  write.csv(species_testing, file = "data/European_aquatic_invert_2012_2020_1km.csv_site_level", row.names = F)
  
  # check if different study sites are within the same cell
  result <- species_df_1km %>%
    group_by(cell, year) %>%
    summarise(
      n_unique_studies = n_distinct(siteID),
      siteIDs = paste(unique(siteID), collapse = ", ")
    ) %>%
    filter(n_unique_studies > 1)
  
  print(result)
  # one cell contains several studyIDs, we will therefore provide aggregated data as well as separate data
  
  for(s in 1:length(selected_species)){
    # Log
    print(selected_species[s])
    
    # subset dataset to extract species
    tmp <- subset(species_df_1km, species_df_1km$Species == selected_species[s])
    
    #Test if there are duplicated cells
    result <- tmp %>%
      group_by(cell, year) %>%
      summarise(
        n_unique_studies = n_distinct(siteID),
        siteIDs = paste(unique(siteID), collapse = ", ")
      ) %>%
      filter(n_unique_studies > 1)

    print(result)

    # create the aggregated abundances per cell & number of site IDs
    tmp_agg <- tmp %>% group_by(cell, year) %>%
      mutate(abundance_cell = sum(abundance), sites_per_cell = n_distinct(siteID)) %>%
      select(-c(abundance, siteID)) %>%
      distinct(.keep_all = T)

    if(s == 1){
      species_df_1km_agg <- tmp_agg
    } else {
      species_df_1km_agg <- rbind(species_df_1km_agg, tmp_agg)
    }
  }
  
  # split data into training and validation data
  species_training_agg <- species_df_1km_agg %>% 
    filter(year <= 2011)
  
  species_testing_agg <- species_df_1km_agg %>% 
    filter(year > 2012)
  
  # save both data sets
  write.csv(species_training_agg, file = "data/European_aquatic_invert_1990_2011_1km_cell_level.csv", row.names = F)
  write.csv(species_testing_agg, file = "data/European_aquatic_invert_2012_2020_1km.csv_cell_level", row.names = F)
  
  
  # Extract the cell numbers & coordinates from the study sites for the 10km resolution
  cells_10km <- cellFromXY(mask_10km, cbind(species_df$x, species_df$y))
  cell_coords_10km <- xyFromCell(mask_10km, cells_10km)
  
  # merge the data to the abundance data set
  species_df_10km <- species_df %>% mutate(cell = cells_10km) %>% 
    mutate(cell_x = cell_coords_10km[,1],
           cell_y = cell_coords_10km[,2]) %>%
    # clean data set
    select(year, Species = binomial, siteID = site_id, country, abundance = abundance_new, unit = unit_new, cell, cell_x, cell_y)
  
# split data into training and validation data
species_training <- species_df_10km %>% 
  filter(year <= 2011)
  
species_testing <- species_df_10km %>% 
  filter(year > 2012)
  
# save both data sets
write.csv(species_training, file = "data/European_aquatic_invert_1990_2011_10km_site_level.csv", row.names = F)
write.csv(species_testing, file = "data/European_aquatic_invert_2012_2020_10km_site_level.csv", row.names = F)

# one cell contains several studyIDs, we will therefore provide aggregated data as well as separate data

for(s in 1:length(selected_species)){
  # Log
  print(selected_species[s])
  
  # subset dataset to extract species
  tmp <- subset(species_df_10km, species_df_10km$Species == selected_species[s])
  
  #Test if there are duplicated cells
  result <- tmp %>%
    group_by(cell, year) %>%
    summarise(
      n_unique_studies = n_distinct(siteID),
      siteIDs = paste(unique(siteID), collapse = ", ")
    ) %>%
    filter(n_unique_studies > 1)
  
  print(result)
  
  # create the aggregated abundances per cell & number of site IDs
  tmp_agg <- tmp %>% group_by(cell, year) %>%
    mutate(abundance_cell = sum(abundance), sites_per_cell = n_distinct(siteID)) %>%
    select(-c(abundance, siteID)) %>%
    distinct(.keep_all = T)
  
  if(s == 1){
    species_df_10km_agg <- tmp_agg
  } else {
    species_df_10km_agg <- rbind(species_df_10km_agg, tmp_agg)
  }
}

# split data into training and validation data
species_training_agg <- species_df_10km_agg %>% 
  filter(year <= 2011)

species_testing_agg <- species_df_10km_agg %>% 
  filter(year > 2012)

# save both data sets
write.csv(species_training_agg, file = "data/European_aquatic_invert_1990_2011_10km_cell_level.csv", row.names = F)
write.csv(species_testing_agg, file = "data/European_aquatic_invert_2012_2020_1km.csv_cell_level", row.names = F)

