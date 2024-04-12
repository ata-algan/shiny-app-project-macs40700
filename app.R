library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(readxl)
library(rnaturalearth)
library(countrycode)
library(ggplot2)
library(leaflet)

import_gapminder <- function(filename, inc_name = NA){
  # Import file
  indicator <- read_excel(filename)
  
  # Rename first column to country, store indicator name for later
  inc_fullname <- names(indicator)[[1]]
  names(indicator)[[1]] <- "country"
  
  # Tidy data frame and add indicator name as variable
  indicator <- indicator %>%
    gather(year, value, -1, convert = TRUE) %>%
    # excel file has weird formatting so fix it for large numbers.
    mutate(value = gsub(",", "", value),
           value = ifelse(grepl("k", value, ignore.case = TRUE),
                          as.numeric(gsub("k", "", value, ignore.case = TRUE)) * 1000,
                          as.numeric(value)),
           variable = ifelse(!is.na(inc_name), inc_name, inc_fullname)) %>%
    dplyr::select(country, year, value)
  
  return(indicator) # Return the processed data frame
}

gdp_pc_df <- import_gapminder("gdp_pcap.xlsx")
gdp_pc_df <- gdp_pc_df %>%
  mutate(continent = countrycode(country, "country.name", "continent"))

ui <- dashboardPage(
  dashboardHeader(title = "Assignment 4"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Trends (by Country)", tabName = "TimeTrendsCountry", icon = icon("calendar")),
      menuItem("Country Comparison (by Year)", tabName = "CountryComparisonYear", icon = icon("table")),
      menuItem("Geographical Trends", tabName = "GeographicalTrends", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", href = "custom.css")),
    tabItems(
      tabItem(tabName = "TimeTrendsCountry",
              fluidPage(
                fluidRow(h2("Time Trends: Country-by-Country")),
                textInput("names", "Lookup countries (comma separated):",
                          value="Turkey, Australia, Palestine"),
                fluidRow(
                  column(6, 
                         sliderInput("yearRange", "Select a Year Range",
                                     value = c(2010, 2020), 
                                     min = 2010,  
                                     max = 2020,
                                     step = 1,
                                     sep = ""),
                         textOutput("yearRangeText")
                  )
                ),
                plotOutput("density"),
                width = 11
              )
      ),
      tabItem(tabName = "CountryComparisonYear",
              fluidPage(
                fluidRow(h2("Country Comparison (by Year)")),
                textInput("namesCC", "Lookup countries (comma separated):",
                          value="Turkey, Australia, Palestine"),
                fluidRow(
                  column(6, 
                         sliderInput("selectedYearRange", "Select a Year Range",
                                     value = c(2010, 2020), 
                                     min = 1800,  
                                     max = 2100,
                                     step = 1,
                                     sep = ""),
                         textOutput("selectedYearRangeText")
                  )
                ),
                tableOutput('meanGdpTable'),
                width = 11
              )
      ),
      tabItem(tabName = "GeographicalTrends",
              fluidPage(
                h2("Geographic GDP Per Capita: (by Country)"),
                fluidRow( 
                  column(6, numericInput("selectedYear", "Select a Year",
                                         value = 2010, min = 2010, max = 2020)),
                  column(6, selectInput("continent", "Select a Continent", 
                                        choices = c("Americas",
                                                    "Africa", "Asia", "Europe", "Oceania"),
                                        selected = "Europe"))
                ),
                fluidRow(
                  column(12, uiOutput("countrySelect"))
                ),
                leafletOutput("mapPlot")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    updateSliderInput(session, "yearRange",
                      min = min(gdp_pc_df$year),
                      max = max(gdp_pc_df$year))
    updateNumericInput(session, "selectedYear", 
                       min = min(gdp_pc_df$year),
                       max = max(gdp_pc_df$year))
  })
  
  # For TimeTrendsCountry tab
  output$density <- renderPlot({
    selected_countries <- unlist(strsplit(input$names, ",\\s*"))
    timetrendscountry_data <- gdp_pc_df %>%
      dplyr::filter(country %in% selected_countries & 
                      year >= input$yearRange[1] & 
                      year <= input$yearRange[2])
    # dynamically decide on the tick labels, otherwise they become
    # squished to each other.
    year_range <- range(timetrendscountry_data$year)
    data_range <- diff(year_range)
    
    tick_interval <- ifelse(data_range <= 20, 1, 
                            ifelse(data_range <= 50, 5, 10))
    
    ggplot(timetrendscountry_data, aes(x = year, y = value, color = country)) +
      geom_line(size = 1.2) +
      scale_x_continuous(breaks = seq(year_range[1], year_range[2], by = tick_interval)) +
      labs(
        title = "GDP per Capita Trends",
        x = "Year",
        y = "GDP per Capita",
        color = "Country"
      ) +
      theme_bw(base_size = 14) +
      theme(
        plot.title = element_text(lineheight = 1),
        legend.title = element_text(face = "bold"),
        legend.position = "right"
      )
  })
  output$yearRangeText <- renderText({
    paste("Selected Year Range: ", input$yearRange[1], " - ", input$yearRange[2])
  })
  
  # For CountryComparisonYear tab
  output$meanGdpTable <- renderTable({
    selected_countries <- unlist(strsplit(input$namesCC, ",\\s*"))
    selected_year_range <- input$selectedYearRange
    
    filtered_data <- gdp_pc_df %>%
      filter(country %in% selected_countries,
             year >= selected_year_range[1],
             year <= selected_year_range[2])
    mean_gdp_data <- filtered_data %>%
      rename(Country = country) %>%
      group_by(Country) %>%
      summarise(`Mean GDP` = mean(value, na.rm = TRUE)) %>%
      arrange(desc(`Mean GDP`))
  
    return(mean_gdp_data)
  })
  
  # For GeographicalTrends tab
  output$countrySelect <- renderUI({
    req(input$continent)
    countries_in_continent <- gdp_pc_df %>%
      filter(continent == input$continent) %>%
      pull(country) %>%
      unique()
    selectInput("selectedCountries", "Select Countries",
                choices = countries_in_continent, multiple = TRUE)
  })
  
  # Rendering the Leaflet map
  output$mapPlot <- renderLeaflet({
    req(input$selectedCountries, input$selectedYear)
    selected_data <- gdp_pc_df %>%
      filter(country %in% input$selectedCountries & year == input$selectedYear) %>%
      select(country, value) %>%
      distinct()
    
    world <- ne_countries(scale = "medium", returnclass = "sf") %>%
      mutate(continent = countrycode(iso_a2, "iso2c", "continent"),
             country = countrycode(name, "country.name", "country.name")) %>%
      filter(continent == input$continent)
    
    world_gdp <- merge(world, selected_data, by = "country", all.x = TRUE)
    
    pal <- colorBin(palette = "viridis", domain = world_gdp$value, bins = 5, na.color = "black")
    
    leaflet(world_gdp) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(value),
                  color = "white", weight = 1,
                  opacity = 1, fillOpacity = 0.8,
                  highlightOptions = highlightOptions(weight = 2,
                                                      color = "#666",
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE),
                  label = ~as.character(country)) %>%
      addLegend(pal = pal, values = ~value, opacity = 0.7,
                title = "GDP per Capita",
                position = "bottomright")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)