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

# setup for the app:

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
      sliderInput("StartYear", "Select start Year", 1980, 1990, 1980, step = 1, sep = ""),
      pickerInput("SelectCountries", "Select countries", choices = unique(Euro_Invert$country), selected = unique(Euro_Invert$country), options = list(`actions-box` = TRUE), multiple = T),
      plotOutput("TimeCoverage"),
      plotOutput("SpeciesNumber"),
      plotOutput("Coverage")
      
    ),
    mainPanel(
      plotOutput("Map", width = "80%")
    )
  )
)

# Define Server --------------
server <- function(input, output){
  countries <- reactive({input$SelectCountries})
  Euro_Invert_sub <- reactive({subset(Euro_Invert, Euro_Invert$country %in% input$SelectCountries & Euro_Invert$year >= input$StartYear)})

  #Obtain information regarding temporal cover & species number per country
  Euro_Invert_list <- reactive({(split(Euro_Invert_sub(), Euro_Invert_sub()$country))})

  # temporal cover
  sampling_years_countries <- reactive({lapply(Euro_Invert_list(), function(x){unique(x$year)})})

  # data frame for completeness of time series for every year
  completeness_timeseries <- reactive({
    dat <- data.frame(Year = c(input$StartYear:2020))
    # add a column for every study country
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
  

  col.1 <- reactive({
    tmp <- col
    ddf <- data.frame(country = c(countries(), "United Kingdom", 'Czech Rep.', 'Luxembourg'))
    tmp[match(ddf$country, newmap@data$NAME)] <- c("black")
    tmp
  })
  
  col2.1 <- reactive({
    tmp <- col2
    reactive({tmp[match(ddf$country, newmap@data$NAME)] <- c("grey50")})
    tmp[58:59] <- c("black")#change borders for N.Cyprus
    tmp
  })
  
  Euro_Invert_info <- reactive({
    dat <- Euro_Invert_list()
    dat <- lapply(dat, function(x){length(unique(x$species))})
    dat <- as.data.frame(do.call(rbind, dat))
    dat <- tibble::rownames_to_column(dat, "country")
    colnames(dat) <- c("country", "no_species")
    # Calculate proportion of present years and proportion of missing years
    dat$p_years_miss <- NA
    dat$p_years_pres <- NA
    for (i in 1:length(countries())) {
      tmp <- subset(completeness_timeseries_long(), completeness_timeseries_long()$country == countries()[i])
      p_miss <- (mean(is.na(tmp$sampled)) * 100)
      dat[dat$country == countries()[i], "p_years_miss"] <- round(p_miss, 1)
      dat[dat$country == countries()[i], "p_years_pres"] <- round(100 - p_miss,1)
    }
    #calculate coverage value per country
    coverage <- lapply(Euro_Invert_list(), function(x){
      (round(nrow(x)/(length(unique(x$species))*length(unique(x$site_id))*length(input$StartYear:2020)),2))
      })
    coverage <- as.data.frame(do.call(rbind, coverage))
    coverage <- tibble::rownames_to_column(coverage, "country")
    colnames(coverage) <- c("country", "coverage")
    dat <- merge(coverage, dat, by = "country")
    dat
  })
  

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
  
  output$SpeciesNumber <- renderPlot({
    ggplot(data=Euro_Invert_info(), aes(x = country, y = no_species))+
      geom_bar(stat = "identity")+
      labs(x = "", y = "")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
      theme(axis.text.x = element_text(hjust = 0.5))+
      ggtitle("Number of species per country")
  })
  
  output$Coverage <- renderPlot({
    ggplot(data=Euro_Invert_info(), aes(x = country, y = coverage))+
      geom_bar(stat = "identity")+
      labs(x = "", y = "")+
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1)) +
      theme(axis.text.x = element_text(hjust = 0.5))+
      ggtitle("Coverage per country")
  })
  
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