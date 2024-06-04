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
Euro_Invert <- read.csv("data/Euro_FreshInv_preprocessed.csv")

# Extract only the data at species-level identification
Euro_Invert <- Euro_Invert[which(!is.na(Euro_Invert$species)),]

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
      sliderInput("StartYear", "Select start Year", 1980, 1990, 1980, step = 1, sep = ""),
      # Picker to select the countries the data should include (default is all countries in the data set)
      pickerInput("SelectCountries", "Select countries", choices = unique(Euro_Invert$country), selected = unique(Euro_Invert$country), options = list(`actions-box` = TRUE), multiple = T),
      # Plot for the sampled years of the different countries
      plotOutput("TimeCoverage"),
      # Plot for the species number for the different countries
      plotOutput("SpeciesNumber"),
      # Plot for the coverage of all possible combinations in 3D option (spatial, temporal, taxonomic)
      plotOutput("Coverage")
    ),
    mainPanel(
      # Metrics for the current selected data set
      tableOutput("Metrics"),
      # Map of the selected countries
      plotOutput("Map", height = "800px", width = "1100px")
    )
  )
)

# Define Server --------------
server <- function(input, output){
  # subsetting data set based on selected start time and countries
  Euro_Invert_sub <- reactive({subset(Euro_Invert, Euro_Invert$country %in% input$SelectCountries & Euro_Invert$year >= input$StartYear)})

  # Splitting dataframe into a list based on country selection
  Euro_Invert_list <- reactive({(split(Euro_Invert_sub(), Euro_Invert_sub()$country))})

  # Obtain sampled years per country
  sampling_years_countries <- reactive({lapply(Euro_Invert_list(), function(x){unique(x$year)})})

  # create data frame which contains the sampled years for every country
  completeness_timeseries <- reactive({
    dat <- data.frame(Year = c(input$StartYear:2020))
    # add a column for every country
    for (k in 1:length(input$SelectCountries)){
      dat$tmp <- NA
      names(dat)[names(dat) == "tmp"] <- input$SelectCountries[k]
      # obtain completeness of time series (non sampled years are marked as NA, sampled years as "Yes")
      for (i in input$StartYear:2020) {
        if(i %in% sampling_years_countries()[[input$SelectCountries[k]]] == T){
          dat[dat$Year == i, input$SelectCountries[k]] <- "Yes"
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
    dat <- Euro_Invert_list()
    # number of species identified for every country
    dat <- lapply(dat, function(x){length(unique(x$species))})
    dat <- as.data.frame(do.call(rbind, dat))
    dat <- tibble::rownames_to_column(dat, "country")
    colnames(dat) <- c("country", "no_species")
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
    coverage <- lapply(Euro_Invert_list(), function(x){
      (round(nrow(x)/(length(unique(x$species))*length(unique(x$site_id))*length(input$StartYear:2020)),2))
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
    TimeCoverage = c(round(length(unique(Euro_Invert_sub()$year))/length(input$StartYear:2020), 4) *100),
    no_species = c(length(unique(Euro_Invert_sub()$species))),
    Coverage = c(
        (nrow(Euro_Invert_sub())/(length(unique(Euro_Invert_sub()$species))*length(unique(Euro_Invert_sub()$site_id))*length(input$StartYear:2020)))*100)
      )
    colnames(tmp) <- c("Time Coverage [%]", "No. Species", "Coverage [%]")
    tmp
    })
  
  # Table to display the metrics above the map
  output$Metrics <- renderTable(metrics())
  
  # Plot for sampled years for the different countries
  output$TimeCoverage <- renderPlot({
    ggplot(data = completeness_timeseries_long(), aes(x = country, y = Year)) +
    geom_raster(aes(fill = sampled)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
    labs(x = "", y = "Year") +
    theme(axis.text.x = element_text(hjust = 0.5))+
    scale_fill_manual("Sampled?", values = "#FF6666", labels = c("Yes", "No"))+
    ggtitle("Coverage of sampled years per country")
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
      ggtitle("Coverage per country")
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

# Idea create a plot/ table that shows the 10 best performing species (time cover, spatial cover (number of countries present of all))
# create a list and use lapply to obtain sampled years divided by all years and present in countries divided by all countries
# look for other option? group by?
# could I use a different metric like nrow(species)/time*countries? or this one as an additional one? and select the species based on this one

# Make country info code more efficient using dplyr and group_by

# Run App -------------
shinyApp(ui = ui, server = server)