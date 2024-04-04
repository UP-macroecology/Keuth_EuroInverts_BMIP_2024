# 1. Preprocessing Data
# Aim of this script is to combine the single csv files for the different study sites into one large data set

# Load packages
library(stringr)
library(dplyr)

# preprocess Bulgaria_007.csv to match columns of the other data sets ---------
Bulgaria_007 <- read.csv("Paper + Information/GitHub repository/raw-data/csvs_each_dataset/Bulgaria_007.csv")

#split code and taxa into several columns
Bulgaria_007[c('site_id', 'day', "month", "year")] <- str_split_fixed(Bulgaria_007$code, '_', 4)
Bulgaria_007[c('taxon_id', 'taxon')] <- str_split_fixed(Bulgaria_007$taxa, '_', 2)

# create the column sample_id
Bulgaria_007$date <- paste(Bulgaria_007$day, Bulgaria_007$month, Bulgaria_007$year, sep=".")
Bulgaria_007$sample_id <- paste(Bulgaria_007$site_id, Bulgaria_007$date, sep = "_")

#reorder columns and remove old columns
Bulgaria_007 <- Bulgaria_007[, c("sample_id", "site_id", "day", "month", "year", "taxon_id", "taxon", "abundance")]

#save adjusted data frame
write.csv(Bulgaria_007, "data/Bulgaria_007.csv", row.names = F)

#Load in all other csv files in a list ----------

# list all files of the rawdata folder of the github repository
raw_freshinv_names <- list.files("Paper + Information/GitHub repository/raw-data/csvs_each_dataset")
raw_freshinv_names <- raw_freshinv_names[! raw_freshinv_names %in% c("Bulgaria_007.csv")] #remove the adjusted Bulgaria csv file from the file list

# Import data sets, modify and save in a list
raw_freshinv_list <- vector("list", length(raw_freshinv_names))
for (i in 1:length(raw_freshinv_names)) {
  raw_freshinv_list[[i]] <- read.csv(paste0("Paper + Information/GitHub repository/raw-data/csvs_each_dataset/", raw_freshinv_names[i])) # Importing file
  if(any(colnames(raw_freshinv_list[[i]]) == "X")){raw_freshinv_list[[i]] <- raw_freshinv_list[[i]] %>% select(-X)} #removing empty columns
  if(any(colnames(raw_freshinv_list[[i]]) == "X.1")){raw_freshinv_list[[i]] <- raw_freshinv_list[[i]] %>% select(-X.1)}
  if(raw_freshinv_names[i] == "UK_001.csv"){raw_freshinv_list[[i]]$listed.in.freshwaterecology.com <- "NA"}#add missing column to one specific dataset
  study_site <- str_remove(raw_freshinv_names[i], ".csv") #Extract country and study site from file name
  study_site <- str_remove(study_site, "00")
  country <- str_remove(study_site, "\\d+" )
  raw_freshinv_list[[i]]$country <- str_remove(country, "_" ) #add country as new column
  raw_freshinv_list[[i]]$study_site <- study_site #add study site of each country as a new column
  names(raw_freshinv_list)[i] <- study_site #change name of the list element
}

#add Bulgaria_007 additionally to the list
raw_freshinv_list[[43]] <- read.csv("data/Bulgaria_007.csv")
raw_freshinv_list[[43]]$listed.in.freshwaterecology.com <- "NA" #add missing column
raw_freshinv_list[[43]]$country <- "Bulgaria" #add country
raw_freshinv_list[[43]]$study_site <- "Bulgaria_7" #add study site
names(raw_freshinv_list)[43] <- "Bulgaria_7" #change name of the element

#combine elements of the list to a large data frame --------
raw_freshinv <- do.call(rbind, raw_freshinv_list)
rownames(raw_freshinv) <- NULL #remove row names

# save large data frame
write.csv(raw_freshinv, "data/Euro_FreshInv_raw.csv", row.names = F)
