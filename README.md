# Keuth_EuroInverts_BMIP_2024

This repository contains the code for exploring the data on European Aquatic Freshwater Invertebrates, which was published by [Haase et al. 2023](https://www.nature.com/articles/s41586-023-06400-1#Sec24). This data will be used for the 2nd BMIP workshop in October 2024.
The data can be found in the [Github repository of the study](https://github.com/Ewelti/EuroAquaticMacroInverts/tree/main).

## Workflow

### Preprocessing data
*Script:* 1_DataPreprocessing.R

In this script the different files from the different countries were joined to one data set. To the data set the columns "country" and "study site" were added.
The column with the taxonomic information was modified to determine which data point was specified to which taxonomic level. Furthermore, if the specification was not clear the closest mutual taxonomic level of the options presented was chosen and typos were removed. Every species detection was checked for its correctnes (misspelling or synonyms). For this the package *taxonize* was used and the species names were first checked using the taxnomic serial number (TSN) and the [Integrated Taxonomic Information System (ITIS)](https://www.itis.gov/). I chose this option since the other functions did not work but produced an error message. All species names that could not be found by the function were manually checked using [GBIF](https://www.gbif.org/).

### Data exploration
*Script:* 2_DataExploration.R

The data set was investigated based on temporal and spatial extent for every country as well as species. The corresponding plots can also be found in the Rmarkdown *Exploration_EuroInv.Rmd/.html*.
