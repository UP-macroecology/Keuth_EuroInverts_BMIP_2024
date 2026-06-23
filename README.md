# Keuth_EuroInverts_BMIP_2024

This repository contains the code for exploring and preprocessing the data on European Aquatic Freshwater Invertebrates (TREAM (Time series of freshwater macroinvertebrate abundances and site characteristics of European streams and rivers)), which was published by [Welti et al., 2024](https://www.nature.com/articles/s41597-024-03445-3). This data will be used for the 2nd BMIP workshop in October 2024. The data can be found [here](https://knb.ecoinformatics.org/view/doi%3A10.5063%2FF1NG4P4R).

## Workflow

### Preprocessing data
*Script:* 1_DataCleaning_TREAM.R

In this script the data set was cleaned by removing typos from certain columns, harmonizing taxonomic names and adding structural zeros to the data set.
For the harmonization of the taxonomic names the package *taxonize* was used and the species names were first checked for correctness (misspelling or synonyms) using the taxnomic serial number (TSN) and the [Integrated Taxonomic Information System (ITIS)](https://www.itis.gov/). I chose this option since the other functions did not work but produced an error message. All species names that could not be found by the function were manually checked using [GBIF](https://www.gbif.org/). Furthermore, if the taxonomic specification was not clear the closest mutual taxonomic level of the named options was chosen and typos were removed. These steps were done with all taxonomic levels (i.e. species, genus, family, order). For the last step, structural zeros were added to the data set, since the data set does not contain any information about species absences. For this, it was assumed that if a species was recorded once at a study site, in all the years without a recorded presence of this species at this study site the species is absent (i.e. abundance = 0).

*Script:* 2_DataPreparation_TREAM.R

In this script different steps were applied to find a subset of good data. The "good data" should contain 30 years of sampling with at least 20 years of continuous sampling years per study site. As a first step all data before 1990 was removed and all study sites with less than 20 years of sampling. This left us with 762 species from 202 differen study sites. After that a few analysis steps were included to provide information about the single species for further selection processes. For this the number of study sites for the "good data" and the full data set of the species was obtained. A simple trend in abundance as log(N) ~ factor(site) + year was calculated and the range extent as a Minimum Convex Polygon for each species was calculated. In this script this was done using all the sites in the survey data set. Additionally, the global range of species was calculated by downloading the occurrence points from GBIF, cleaning the coordinates and placing a miminum convex polygon around it. As a last step the data is filtered again with the species needing to be present on more than 10 sites and their range in the data set to cover at least 25% of the global range.

*Script:* 3_SpeciesSelection_TREAM.R

The final species and the pilot species are selected based on their range class (small, medium, large) and trend class (increasing, stable, decreasing). For the BMIP project 18 different species are selected to have 2 species per range class and trend class combination.

*Script:* 4a_PrepareModelData_pilotspecies_TREAM

The Abundance data sets are prepared. These data sets are required for the models for the BMIP project. Here, the data for the pilot species is prepared. Data files are prepared at a 1km and 10km resolution and at cell level (aggregated abundance counts over several study sites) and at site level.

*Script:* 4b_PrepareModelData_allspecies_TREAM

The Abundance data sets are prepared. These data sets are required for the models for the BMIP project. Here, the data for the 18 species is prepared. Data files are prepared at a 1km and 10km resolution and at cell level (aggregated abundance counts over several study sites) and at site level.

### Data exploration
*Script:* 4_DataExploration_TREAM.R

In this script the data set was explored based on temporal and spatial coverage for every country, the proportion of identification to the different taxonomic level for every country. The corresponding plots can be found in the Rmarkdown *Exploration_EuroInv.Rmd/.html*.

*Script:* ExploreSpeciesData.Rmd

This Rmarkdown further deals with the species selection (finding 16? species with a good data coverage). For this different exploration steps are applied based on the range size, range coverage, abundance trends etc.

### ShinyApp

The folder *ShinyApp* contains the script and data required for the ShinyApp to run. The ShinyApp is supposed to help with finding a good subset of the data set, which has a good spatial and temporal cover for different species. Different criteria can be chosen to subselect the data set.

### misc. scripts

In this folder several different scripts are included that contain different scripts that are used to explore the data set and test out different methods and take decisions. Here, one can find the the Rmarkdown *Exploration_EuroInv.Rmd/.html*. This folder also includes scripts for plotting the spatial coverage of the data, testing the effect of the number of downloaded occurrences as well as trying to find the errors when calculating the MCP using the GBIF data set.
