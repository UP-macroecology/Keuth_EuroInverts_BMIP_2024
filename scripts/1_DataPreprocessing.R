# 1. Preprocessing Data
#Create large data set

# Load packages
library(stringr)
library(dplyr)

# list all files in the rawdata folder of the paper
raw_freshinv_names <- list.files("Paper + Information/GitHub repository/raw-data/csvs_each_dataset")

# Load data set in a list
raw_freshinv_list <- vector("list", length(raw_freshinv_names))
for (i in 1:length(raw_freshinv_names)) {
  raw_freshinv_list[[i]] <- read.csv(paste0("Paper + Information/GitHub repository/raw-data/csvs_each_dataset/", raw_freshinv_names[i]))
  if(any(colnames(raw_freshinv_list[[i]]) == "X")){raw_freshinv_list[[i]] <- raw_freshinv_list[[i]] %>% select(-X)}
  if(any(colnames(raw_freshinv_list[[i]]) == "X.1")){raw_freshinv_list[[i]] <- raw_freshinv_list[[i]] %>% select(-X.1)}
  study_site <- str_remove(raw_freshinv_names[i], ".csv")
  study_site <- str_remove(study_site, "00")
  country <- str_remove(study_site, "\\d+" )
  raw_freshinv_list[[i]]$country <- str_remove(country, "_" )
  raw_freshinv_list[[i]]$study_site <- study_site
  names(raw_freshinv_list)[i] <- study_site
}

#rbind all the data
raw_freshinv <- do.call(rbind, raw_freshinv_list)
ifelse(any(colnames(raw_freshinv_list[["CzechRepublic_1"]]) == "X.1"), print("yes"), print("no"))
  