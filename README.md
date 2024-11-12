# Keuth_EuroInverts_BMIP_2024

This repository contains the code for exploring and preprocessing the data on European Aquatic Freshwater Invertebrates (TREAM (Time series of freshwater macroinvertebrate abundances and site characteristics of European streams and rivers)), which was published by [Welti et al., 2024](https://www.nature.com/articles/s41597-024-03445-3). This data will be used for the 2nd BMIP workshop in October 2024.
The data can be found [here](https://knb.ecoinformatics.org/view/doi%3A10.5063%2FF1NG4P4R).

## Workflow

### Preprocessing data
*Script:* 1_DataCleaning_TREAM.R

In this script the data set was cleaned by removing typos from certain columns, harmonizing taxonomic names and adding structural zeros to the data set.
For the harmonization of the taxonomic names the package *taxonize* was used and the species names were first checked for correctness (misspelling or synonyms) using the taxnomic serial number (TSN) and the [Integrated Taxonomic Information System (ITIS)](https://www.itis.gov/). I chose this option since the other functions did not work but produced an error message. All species names that could not be found by the function were manually checked using [GBIF](https://www.gbif.org/). Furthermore, if the taxonomic specification was not clear the closest mutual taxonomic level of the named options was chosen and typos were removed. These steps were done with all taxonomic levels (i.e. species, genus, family, order). As the last step, structural zeros are added to the data set, since the data set does not contain any information about species absences. For this, it was assumed that if a species was recorded once at a study site, in all years without a presence of this species at this study site the species is absent.

*Script:* 1a_DataPreprocessing_TREAM.R

In this script the TREAM data set was cleaned: Wrong cell values were removed from different columns and the different taxonomic columns (species, genus and family) were checked for correctness (misspelling or synonyms) and cleaned. For this the package *taxonize* was used and the species names were first checked using the taxnomic serial number (TSN) and the [Integrated Taxonomic Information System (ITIS)](https://www.itis.gov/). I chose this option since the other functions did not work but produced an error message. All species names that could not be found by the function were manually checked using [GBIF](https://www.gbif.org/). I further extracted higher taxonomic levels (family, order) for each record using the package *taxonize*. If a higher taxonomic level could not be found by the function it was also checked using [GBIF](https://www.gbif.org/) and the [Catalogue of Life](https://www.catalogueoflife.org/). Furthermore, for each record the taxonomic level of its identification was determined. If a taxonomic identification was not clear the closest mutual taxonomic level of the options named was chosen. 

### Data exploration
*Script:* 2_DataExploration.R

The data set was investigated based on temporal and spatial coverage for every country as well as species. The proportion of identification to the different taxonomic level for every country was also determined. The corresponding plots can be found in the Rmarkdown *Exploration_EuroInv.Rmd/.html*.

*Script:* 2a_DataExploration_TREAM.R

The data set was investigated focusing on the temporal and spatial coverage for every country. Furthermore, the proportion of identification to the different taxonomic levels for every country was determined and the proportion of sampled years per study site for every country. Additionally, since the data set does not include 0 counts, structural 0 were included in the data set.

*Script:* 4_GlobalRange.R

For this analysis I want to obtain the proportion of global range that is covered by our data for every single species. For this, I downloaded range data for the order *Odonata* from the [IUCN Red List](https://www.iucnredlist.org/resources/spatial-data-download) and tested how many species are included in this data set. After that no further analysis were down until now.

### ShinyApp

The folder *ShinyApp* contains the script and data required for the ShinyApp to run. The ShinyApp is supposed to help with finding a good subset of the data set, which has a good spatial and temporal cover for different species. Different criteria can be chosen to subselect the data set.
