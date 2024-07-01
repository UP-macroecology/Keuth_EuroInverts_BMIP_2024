# Keuth_EuroInverts_BMIP_2024

This repository contains the code for exploring the data on European Aquatic Freshwater Invertebrates, which was published by [Haase et al. 2023](https://www.nature.com/articles/s41586-023-06400-1#Sec24). This data will be used for the 2nd BMIP workshop in October 2024.
The data can be found in the [Github repository of the study](https://github.com/Ewelti/EuroAquaticMacroInverts/tree/main). For the analysis we used a cleaned version of data set, which was provided to us by the authors. This data is refered to as TREAM (Time series of freshwater macroinvertebrate abundances and site characteristics of European streams and rivers) by the authors.

## Workflow

### Preprocessing data
*Script:* 1_DataPreprocessing.R

In this script the different files from the different countries were joined to one data set. To the data set the columns "country" and "study site" were added.
The column with the taxonomic information was modified to determine which data point was specified to which taxonomic level. Furthermore, if the specification was not clear the closest mutual taxonomic level of the options named was chosen and typos were removed. Every species detection was checked for its correctnes (misspelling or synonyms). For this the package *taxonize* was used and the species names were first checked using the taxnomic serial number (TSN) and the [Integrated Taxonomic Information System (ITIS)](https://www.itis.gov/). I chose this option since the other functions did not work but produced an error message. All species names that could not be found by the function were manually checked using [GBIF](https://www.gbif.org/).

*Script:* 1a_DataPreprocessing_TREAM.R

In this script the TREAM data set was cleaned: Wrong cell values were removed from different columns and the different taxonomic columns (species, genus and family) were checked for correctness (misspelling or synonyms) and cleaned. For this the package *taxonize* was used and the species names were first checked using the taxnomic serial number (TSN) and the [Integrated Taxonomic Information System (ITIS)](https://www.itis.gov/). I chose this option since the other functions did not work but produced an error message. All species names that could not be found by the function were manually checked using [GBIF](https://www.gbif.org/). I further extracted higher taxonomic levels (family, order) for each record using the package *taxonize*. If a higher taxonomic level could not be found by the function it was also checked using [GBIF](https://www.gbif.org/) and [Catalogue of Life](https://www.catalogueoflife.org/). Furthermore, for each record it was determined to which taxonomic level it was determined. If a taxonomic identification was not clear the closest mutual taxonomic level of the options named was chosen. 

### Data exploration
*Script:* 2_DataExploration.R

The data set was investigated based on temporal and spatial coverage for every country as well as species. The proportion of identification to the different taxonomic level for every country was also determined. The corresponding plots can also be found in the Rmarkdown *Exploration_EuroInv.Rmd/.html*.

*Script:* 2a_DataExploration_TREAM.R

The data set was investigated focusing on the temporal and spatial coverage for every country. Furthermore, the proportion of identification to the different taxonomic levels for every country was determined and the proportion of sampled years per study site for every country. Additionally, since the data set does not include 0 counts, structural 0 were included in the data set.

*Script:* 4_GlobalRange.R

For this script I downloaded range data for the order *Odonata* from the [IUCN Red List](https://www.iucnredlist.org/resources/spatial-data-download) and tested how many species are included in this data set. 

### ShinyApp

The folder *ShinyApp* contains the script and data required for the 
