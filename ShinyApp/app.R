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
Euro_Invert_raw <- read.csv("data/TREAM_preprocessed.csv")

# Extract only the data at species-level identification
countries <- unique(Euro_Invert_raw$country)

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
      sliderInput("ThresholdStudySite", "Set threshold for covered years per country [%]", 0, 100, 0, step = 5, sep = ""),
      # Picker to select the countries the data should include (default is all countries in the data set)
      pickerInput("SelectCountries", "Select countries", choices = sort(unique(Euro_Invert_raw$country)), selected = sort(unique(Euro_Invert_raw$country)), options = list(`actions-box` = TRUE), multiple = T),
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
  Euro_Invert_sub <- reactive({subset(Euro_Invert_raw, Euro_Invert_raw$country %in% input$SelectCountries & Euro_Invert_raw$year >= input$StartYear)})
  
  # Obtain percentage of sampled years per study site
  no_years_countries <- reactive({Euro_Invert_sub() %>% group_by(country) %>% summarise(no_years_c = n_distinct(year))})
  no_years_studysites <- reactive({Euro_Invert_sub() %>% group_by(country, site_id) %>% summarise(no_years = n_distinct(year)) %>% full_join(no_years_countries(), by = join_by(country)) %>% 
      mutate(per_cov_years = (no_years/no_years_c) * 100, site_id = as.character(site_id))})
  
  # merge information with large dataset
  Euro_Invert_joined <- reactive({Euro_Invert_sub() %>% mutate(site_id = as.character(site_id)) %>% full_join(no_years_studysites(), by = join_by(site_id))})
  
  # subset based on threshold selections
  Euro_Invert_sub2 <- reactive({subset(Euro_Invert_joined(), Euro_Invert_joined()$per_cov_years >= input$ThresholdStudySite)})
  
  # subset data if only species level identification should be displayed
  Euro_Invert_sl <- reactive({if(input$SpeciesLevelData == T){
    Euro_Invert_sub2()[which(!is.na(Euro_Invert_sub2()$taxon)),]
  } else {
    Euro_Invert_sub2()
  }
  })
  
  #Picker input for the different taxonomic orders
  output$SelectOrders_Picker <- renderUI({
    pickerInput("SelectOrders", "Which orders do you want to investigate?", na.exclude(sort(unique(Euro_Invert_sl()$order))), selected = na.exclude(sort(unique(Euro_Invert_sl()$order))),
                options = list(`actions-box` = TRUE), multiple = T)
  })
  
  # subsetting data set based on order selection
  Euro_Invert <- reactive({subset(Euro_Invert_sl(), Euro_Invert_sl()$order %in% input$SelectOrders)})

  # Splitting dataframe into a list based on country selection
  Euro_Invert_list <- reactive({(split(Euro_Invert_raw, Euro_Invert_raw$country))})

  # Obtain sampled years per country
  sampling_years_countries <- reactive({lapply(Euro_Invert_list(), function(x){unique(x$year)})})

  # create data frame which contains the sampled years for every country
  completeness_timeseries <- reactive({
    dat <- data.frame(Year = c(input$StartYear:2020))
    # add a column for every country
    for (k in 1:length(countries)){
      dat$tmp <- NA
      names(dat)[names(dat) == "tmp"] <- countries[k]
      # obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
      for (i in input$StartYear:2020) {
        if(i %in% sampling_years_countries()[[countries[k]]] == T){
          dat[dat$Year == i, countries[k]] <- "Yes"
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
    tmp <- data.frame(country = c(input$SelectCountries))
    if ("UK" %in% input$SelectCountries) {
      tmp <- rbind(tmp, "United Kingdom")
    } 
    if ("CzechRepublic" %in% input$SelectCountries){
      tmp <- rbind(tmp, "Czech Rep.")
    } 
    if ("Luxemburg" %in% input$SelectCountries){
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
  Euro_Invert_info <- reactive({
    # number of species identified for every country
    dat <- Euro_Invert_raw %>% group_by(country) %>% summarise(no_species = n_distinct(species))
    # Calculate proportion of present years and proportion of missing years
    dat$p_years_miss <- NA
    dat$p_years_pres <- NA
    for (i in 1:length(countries)) {
      tmp <- subset(completeness_timeseries_long(), completeness_timeseries_long()$country == countries[i])
      p_miss <- (mean(is.na(tmp$sampled)) * 100)
      dat[dat$country == countries[i], "p_years_miss"] <- round(p_miss, 1)
      dat[dat$country == countries[i], "p_years_pres"] <- round(100 - p_miss,1)
    }
    #calculate coverage value per country (Coverage represents the completeness of the data set regarding three dimensions: space, time, taxon, 100% = perfect sampling)
    coverage <- lapply(Euro_Invert_list(), function(x){
      (round(nrow(x)/(length(unique(x$taxon))*length(unique(x$site_id))*length(input$StartYear:2020)),2))
      })
    coverage <- as.data.frame(do.call(rbind, coverage))
    coverage <- tibble::rownames_to_column(coverage, "country")
    colnames(coverage) <- c("country", "coverage")
    dat <- merge(coverage, dat, by = "country")
    dat
  })
  
  #calculate metrics (proportion of sampled years, number of species, coverage) for the subset data
  metrics <- reactive({
    tmp <-data.frame(
    TimeCoverage = c(round(length(unique(Euro_Invert()$year))/length(input$StartYear:2020), 4) *100),
    no_species = c(length(unique(Euro_Invert()$taxon))),
    Coverage = c(
        (nrow(Euro_Invert())/(length(unique(Euro_Invert()$taxon))*length(unique(Euro_Invert()$site_id))*length(input$StartYear:2020)))*100)
      )
    colnames(tmp) <- c("Time Coverage [%]", "No. Species", "Coverage [%]")
    tmp
    })
  
  # calculate metrics (proportion of sampled years, coverage of countries) per species level
  species_counts <- reactive({Euro_Invert() %>% group_by(taxon) %>% count()})
  species_metrics <- reactive({Euro_Invert() %>% group_by(taxon) %>% summarise(no_years = n_distinct(year), no_countries = n_distinct(country.y), no_studysites = n_distinct(site_id)) %>%
      full_join(species_counts(), by = join_by(taxon)) %>% mutate(time_cov = (no_years/(length(input$StartYear:2020)))*100,
                                                                  spatial_cov = (no_countries/(length(input$SelectCountries)))*100,
                                                                  coverage = (n/((length(input$StartYear:2020)) * no_studysites))*100) %>%
      arrange(desc(coverage))})
  # rename the columns
  species_metrics2 <- reactive({
    tmp <- species_metrics()
    tmp <- tmp[which(!is.na(tmp$taxon)),]
    colnames(tmp) <- c("species", "No. Years", "No. Countries", "No. Studysites", "No. Detections", "Time coverage [%]", "Spatial coverage [%]", "Coverage [%]")
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
    ggplot(data=Euro_Invert_info(), aes(x = country, y = no_species))+
      geom_bar(stat = "identity")+
      labs(x = "", y = "")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
      theme(axis.text.x = element_text(hjust = 0.5))+
      ggtitle("Number of species per country")
  })
  
  # Plot for the coverage per country
  output$Coverage <- renderPlot({
    ggplot(data=Euro_Invert_info(), aes(x = country, y = coverage))+
      geom_bar(stat = "identity")+
      labs(x = "", y = "")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
      theme(axis.text.x = element_text(hjust = 0.5))+
      ggtitle("Coverage per country [%]")
  })
  
  # Plot for sampled years per study site
  output$SiteTimeCoverage <- renderPlot({
    ggplot(data=no_years_studysites(), aes(x = reorder(site_id, per_cov_years), y = per_cov_years))+
      geom_bar(stat = "identity")+
      facet_wrap(~ country, scales = "free_x")+
      xlab("Study Site")+
      ggtitle("Proportion of sampled years per study site [%]")+
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