# ShinyApp "Subselecting BMIP Freshwater data"
# Goal: Playing around with subselecting the different countries and thus data sets

# Loading packages --------------------

library(rworldmap)
library(scales)
library(maps)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)

# Code to run the app ------------------------------

# library(shiny)
# runApp("ShinyApp/app.R")

# Setup for the app -----------------------

# Loading the data
#TREAM_raw <- read.csv("data/TREAM_preprocessed.csv")
TREAM_raw <- read.csv("data/TREAM_zeros.csv")

# Prepare data for map
world_map <- maps::map("world", plot=FALSE)
newmap <- getMap(resolution = "low")
col <- rep("grey85", length(newmap@data$NAME))
col2 <- rep("#00000000", length(newmap@data$NAME)) #or alpha("white",0) instead of #00000000
wv<-seq(1, 1, length.out=253) 
wv[58:59] <- c(0)

# Define UI -----------------

ui <- fluidPage(
  fluidRow(
    column(10, div(h1(strong('BMIP Freshwater data'), style="font-size:30px"), h4('Subselecting data set', style="margin: 0;")))), #Add Title of App
  sidebarLayout(
    sidebarPanel(
      # Slider to select the start Year of the Data
      sliderInput("StartYear", "Select Start Year", 1980, 1990, 1980, step = 1, sep = ""),
      # Slider to select the threshold for the proportion of sampled years per study site
      uiOutput("SelectThreshold_Slider"),
      # Picker to select the countries the data should include (default is all countries in the data set)
      uiOutput("SelectCountries_Picker"),
      # Picker to select the orders the data should include (default is all orders in the data set)
      uiOutput("SelectOrders_Picker"),
      # Check box to include only data with identifications on species level
      checkboxInput("SpeciesLevelData", "Use only data with identification on species level"),
      # Plot for the sampled years of the different countries
      plotOutput("TimeCoverage"),
      # Plot for the species number for the different countries
      plotOutput("SpeciesNumber"),
      # Plot for the coverage of all possible combinations in 3D option (spatial, temporal, taxonomic)
      plotOutput("Coverage"),
      #Plot for proportion of sampled years per study site
      plotOutput("SiteTimeCoverage", height = "700px")
    ),
    mainPanel(
      # Metrics for the current selected data set
      tableOutput("Metrics"),
      # Map of the selected countries
      plotOutput("Map", height = "800px", width = "1100px"),
      # Metrics for the current selected data set for the first 10 species
      tableOutput("SpeciesMetrics"),
    )
  )
)

# Define Server --------------
server <- function(input, output){
  
  # subsetting data set based on selected start time and countries
  TREAM_sub_year <- reactive({subset(TREAM_raw, TREAM_raw$year >= input$StartYear)})
  
  output$SelectThreshold_Slider <- renderUI({
  sliderInput("ThresholdStudySite", "Set threshold for sampled years per study site", 0, (max(TREAM_sub_year()$year) - min(TREAM_sub_year()$year)), 0, step = 5, sep = "")
  })
  
  # Obtain percentage of sampled years per study site
  no_years_countries <- reactive({TREAM_sub_year() %>% group_by(country) %>% summarise(no_years_c = n_distinct(year))})
  no_years_studysites <- reactive({TREAM_sub_year() %>% group_by(country, site_id) %>% summarise(no_years = n_distinct(year)) %>% full_join(no_years_countries(), by = join_by(country)) %>% 
      mutate(per_cov_years = (no_years/no_years_c) * 100, site_id = as.character(site_id))})
  
  # merge information with large dataset
  TREAM_joined <- reactive({TREAM_sub_year() %>% mutate(site_id = as.character(site_id)) %>% full_join(no_years_studysites(), by = join_by(site_id))})
  
  # subset based on threshold selections
  TREAM_sub_sitecov <- reactive({subset(TREAM_joined(), TREAM_joined()$no_years >= input$ThresholdStudySite)})
  
  # Picker to select the countries the data should include (default is all countries in the data set)
  output$SelectCountries_Picker <- renderUI({
    pickerInput("SelectCountries", "Select countries", choices = sort(unique(TREAM_sub_sitecov()$country.y)), selected = sort(unique(TREAM_sub_sitecov()$country.y)), 
                options = list(`actions-box` = TRUE), multiple = T)
  })
  
  # subset based on country selection
  TREAM_sub_country <- reactive({subset(TREAM_sub_sitecov(), TREAM_sub_sitecov()$country.y %in% input$SelectCountries)})
  
  # subset data if only species level identification should be displayed
  TREAM_sl <- reactive({if(input$SpeciesLevelData == T){
    TREAM_sub_country()[which(!is.na(TREAM_sub_country()$binomial)),]
  } else {
    TREAM_sub_country()
  }
  })
  
  #Picker input for the different taxonomic orders
  output$SelectOrders_Picker <- renderUI({
    pickerInput("SelectOrders", "Which orders do you want to investigate?", na.exclude(sort(unique(TREAM_sl()$order))), selected = na.exclude(sort(unique(TREAM_sl()$order))),
                options = list(`actions-box` = TRUE), multiple = T)
  })
  
  # subsetting data set based on order selection and renamed column
  TREAM <- reactive({
    tmp <- TREAM_sl()
    tmp <- subset(tmp, tmp$order %in% input$SelectOrders)
    names(tmp)[names(tmp) == "country.y"] <- "country"
    tmp
    })
  
  # Extract countries
  countries <- reactive({unique(TREAM()$country)})

  # Splitting dataframe into a list based on country selection
  TREAM_list <- reactive({(split(TREAM(), TREAM()$country))})

  # Obtain sampled years per country
  sampling_years_countries <- reactive({lapply(TREAM_list(), function(x){unique(x$year)})})

  # create data frame which contains the sampled years for every country
  completeness_timeseries <- reactive({
    dat <- data.frame(Year = c(input$StartYear:2020))
    # add a column for every country
    for (k in 1:length(countries())){
      dat$tmp <- NA
      names(dat)[names(dat) == "tmp"] <- countries()[k]
      # obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
      for (i in input$StartYear:2020) {
        if(i %in% sampling_years_countries()[[countries()[k]]] == T){
          dat[dat$Year == i, countries()[k]] <- "Yes"
        }
      }
    }
    dat
  })

  # reorder dataframe to long format (kind of appending columns)
  completeness_timeseries_long <- reactive({
    completeness_timeseries() %>%
    pivot_longer(
      cols = -Year,
      names_to = "country",
      values_to = "sampled",
      values_transform = list(sampled = as.character)
    ) %>%
    arrange(Year, country, sampled)})
  
  # create list of countries for the map (some countries have different spelling in the rworldmap package)
  ddf <- reactive({
    tmp <- data.frame(country = c(countries()))
    if ("UK" %in% countries()) {
      tmp <- rbind(tmp, "United Kingdom")
    } 
    if ("Czech Republic" %in% countries()){
      tmp <- rbind(tmp, c("Czech Rep."))
    } 
    if ("Luxemburg" %in% countries()){
      tmp <- rbind(tmp, "Luxembourg")
    } 
    tmp
  })
  
  # create colours for background and the countries and their borders
  col.1 <- reactive({
    tmp <- col
    tmp[match(ddf()$country, newmap@data$NAME)] <- c("black")
    tmp
  })
  
  col2.1 <- reactive({
    tmp <- col2
    reactive({tmp[match(ddf()$country, newmap@data$NAME)] <- c("grey50")})
    tmp[58:59] <- c("black")#change borders for N.Cyprus
    tmp
  })
  
  # create data set which contains specific information for every country
  TREAM_info <- reactive({
    # number of species identified for every country
    dat <- TREAM() %>% group_by(country) %>% summarise(no_species = n_distinct(species))
    # Calculate proportion of present years and proportion of missing years
    dat$p_years_miss <- NA
    dat$p_years_pres <- NA
    for (i in 1:length(input$SelectCountries)) {
      tmp <- subset(completeness_timeseries_long(), completeness_timeseries_long()$country == input$SelectCountries[i])
      p_miss <- (mean(is.na(tmp$sampled)) * 100)
      dat[dat$country == input$SelectCountries[i], "p_years_miss"] <- round(p_miss, 1)
      dat[dat$country == input$SelectCountries[i], "p_years_pres"] <- round(100 - p_miss,1)
    }
    #calculate coverage value per country (Coverage represents the completeness of the data set regarding three dimensions: space, time, taxon, 100% = perfect sampling)
    coverage <- lapply(TREAM_list(), function(x){
      (round(nrow(x)/(length(unique(x$binomial))*length(unique(x$site_id))*length(input$StartYear:2020)),2))
      })
    coverage <- as.data.frame(do.call(rbind, coverage))
    coverage <- tibble::rownames_to_column(coverage, "country")
    colnames(coverage) <- c("country", "coverage")
    dat <- merge(coverage, dat, by = "country")
    dat
  })
  
  #calculate metrics (proportion of sampled years, number of species, coverage) for the subset data
  metrics <- reactive({
    nYr <- length(input$StartYear:2020)
    nSite <- length(unique(TREAM()$site_id))
    nSp <- c(length(unique(TREAM()$binomial)))
    nObs <- nrow(TREAM()) 
    tmp <-data.frame(
      TimeCoverage = c(round(length(unique(TREAM()$year))/nYr, 4) *100),
      no_years = nYr,
      no_species = nSp,
      no_Sites = nSite,
      no_Obs = nObs,
      no_Combos = nSp * nSite * nYr,
      DataCoverage = c((nObs/(nSp * nSite * nYr)) * 100))
    colnames(tmp) <- c("Time Coverage [%]",
                       "No. years",
                       "No. species",
                       "No. sites",
                       "No. observations",
                       "No. combos",
                       "Data Coverage [%]")
    tmp
    })
  
  # calculate metrics (proportion of sampled years, coverage of countries) per species level
  species_counts <- reactive({TREAM() %>% group_by(binomial) %>% count()})
  species_metrics <- reactive({TREAM() %>% group_by(binomial) %>% summarise(no_years = n_distinct(year), no_countries = n_distinct(country), no_studysites = n_distinct(site_id)) %>%
      full_join(species_counts(), by = join_by(binomial)) %>% mutate(time_cov = (no_years/(length(input$StartYear:2020)))*100,
                                                                  spatial_cov = (no_countries/(length(input$SelectCountries)))*100,
                                                                  coverage = (n/((length(input$StartYear:2020)) * no_studysites))*100) %>%
      arrange(desc(coverage))})
  # rename the columns
  species_metrics2 <- reactive({
    tmp <- species_metrics()
    tmp <- tmp[which(!is.na(tmp$binomial)),]
    colnames(tmp) <- c("species", "No. years", "No. countries", "No. sites", "No. observations", "Time coverage [%]", "Spatial coverage [%]", "Data Coverage [%]")
    tmp
  })
  
  # Table to display the metrics above the map and below the map
  output$Metrics <- renderTable(metrics())
  output$SpeciesMetrics <- renderTable(head(species_metrics2(),10))
  
  # Plot for sampled years for the different countries
  output$TimeCoverage <- renderPlot({
    ggplot(data = completeness_timeseries_long(), aes(x = country, y = Year)) +
    geom_raster(aes(fill = sampled)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
    labs(x = "", y = "Year") +
    theme(axis.text.x = element_text(hjust = 0.5))+
    scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
    ggtitle("Sampled years per country")
  })
  
  # Plot for the species number per country
  output$SpeciesNumber <- renderPlot({
    ggplot(data=TREAM_info(), aes(x = country, y = no_species))+
      geom_bar(stat = "identity")+
      labs(x = "", y = "")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
      theme(axis.text.x = element_text(hjust = 0.5))+
      ggtitle("Number of species per country")
  })
  
  # Plot for the coverage per country
  output$Coverage <- renderPlot({
    ggplot(data=TREAM_info(), aes(x = country, y = coverage))+
      geom_bar(stat = "identity")+
      labs(x = "", y = "")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
      theme(axis.text.x = element_text(hjust = 0.5))+
      ggtitle("Coverage per country [%]")
  })
  
  # Plot for sampled years per study site
  output$SiteTimeCoverage <- renderPlot({
    ggplot(data=no_years_studysites() %>% filter(no_years >= input$ThresholdStudySite), aes(x = reorder(site_id, no_years), y = no_years))+
      geom_bar(stat = "identity")+
      facet_wrap(~ country, scales = "free_x")+
      xlab("Study Site")+
      ggtitle("Sampled years per study site")+
      theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())+
      ylab("")
  })
  
  # Map with the selected countries
  output$Map <- renderPlot({
  # Plot map with text
  plot(newmap,col=col.1(),
       bg="white",border=col2.1(),
       xlim = c(-10, 34),
       ylim = c(35, 70),
       asp = 1,lwd=wv
  )
 })
}

# Run App -------------
shinyApp(ui = ui, server = server)