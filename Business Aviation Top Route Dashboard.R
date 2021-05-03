library(shiny)
library(tidyverse)
library(leaflet)
library(shinyalert)

ui <- bootstrapPage(
  theme = shinythemes::shinytheme('simplex'),
  leaflet::leafletOutput('map', width = '100%', height = '100%'),
  absolutePanel(top = 10, right = 10, id = 'controls',
                h5(strong("Business Aviation Top Route Dashboard")),
                selectInput(inputId = "aircraft_type_selection",
                            label = strong("Select aircraft types"),
                            choices = c("_ALL_", "E545 - Embraer Legacy 450", "E550 - Embraer Legacy 500", "E50P - Embraer Phenom 100", 
                                        "E55P - Embraer Phenom 300", "G650 - Gulfstream G650", "GA6C - Gulfstream G600", 
                                        "G550 - Gulfstream G550", "GLEX - Bombardier Global Express", 
                                        "GL7T - Bombardier Global 7500", "GL5T - Bombardier Global 5000", 
                                        "CL60 - Bombardier Challenger 600/601/604", "CL35 - Bombardier Challenger 350", 
                                        "CL30 - Bombardier Challenger 300", "C56X - Cessna Citation Excel/XLS", 
                                        "C525 - Cessna Citation CJ1", "C25A - Cessna Citation CJ2", 
                                        "C25B - Cessna Citation CJ3", "C25C - Cessna Citation CJ4", 
                                        "C25M - Cessna Citation M2", "C510 - Cessna Ciation Mustang", 
                                        "C52B - Cessna Cittion 525/526", "C55B - Cessna Citation Bravo", 
                                        "C560 - Cessna Cittion V/Ultra/Encore", "C680 - Cessna Citation Sovereign", 
                                        "C68A - Cessna Citation Latitude", "C700 - Cessna Citation Longitude", 
                                        "C750 - Cessna Citation X", "H25 - Raytheon Hawker 800", "BE40 - Raytheon Hawker 400", 
                                        "LJ - Bombardier Learjet", "G280 - Gulfstream G280", "F900 - Dassault Falcon 900", 
                                        "FA7X - Dassault Falcon 7X", "FA8X - Dassault Falcon 8X", "F2TH - Dassault Falcon 2000", 
                                        "F900 - Dassault Falcon 900", "G150 - Gulfstream G150", "PRM1 - Raytheon Premier 1", "FA50 - Dassault Falcon 50"),
                            selected = c("_ALL_"),
                            multiple = TRUE,
                            selectize = TRUE),
                selectInput(inputId = "airport_selection",
                            label = strong("Select airport"),
                            choices = c("_ALL_", "TEB", "HPN", "IAD", "DAL", "PBI", "MDW", "VNY", "LAS", "SJC", 
                                        "SNA", "HOU", "SFO", "OAK", "BOS", "AUS", "LAX", "PHL", "MIA", "BNA", 
                                        "SLC", "FLL", "RDU", "SAN", "BUR", "SAT", "STL", "OMA", "BED", "IND", 
                                        "SDL", "PIT", "BWI", "CLT", "APF", "BHM", "PDK", "PSP", "APA", "MSP", 
                                        "TPA", "BFI", "MSY", "ASE", "MCO", "PWK", "PHX", "SDF", "JAX", "CMH", 
                                        "MKE", "ORD", "BCT", "CLE", "MRY", "PDX", "BUF", "ACK", "MMU", "SBA", 
                                        "LGA", "BDL", "RSW", "EGE", "DEN", "DFW", "OPF", "CRQ", "MKC", "APC", 
                                        "TUS", "MEM", "ATL", "JAC", "PVD", "IAH", "FTW", "JFK", "LUK", "SGR", 
                                        "SUA", "SRQ", "EWR", "TRM", "RNO", "DTW", "SUS", "LGB", "SUN", "BZN", 
                                        "FRG", "CHS", "LBE", "CHO", "VRB", "CMA", "GSO", "ADS", "FXE", "BJC", 
                                        "FOK"),
                            #selected = NULL,
                            #multiple = FALSE,
                            selectize = TRUE),
                sliderInput(inputId = "top_n",
                            label = strong("Top n routes"),
                            min = 1,
                            max = 20,
                            value = 10,
                            ticks = FALSE),
                # CODE BELOW: Add an action button named show_about
                actionButton('show_user_guide', 'User Guide')
  ),
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
  "),
  useShinyalert()
)
server <- function(input, output, session) {
  
  user_guide_text <- "<b>Dashboard shows most frequently flown routes per selected aircraft and airport</b><br><br>
                      <b>Reporting year: 2019</b> (last full year of traffic before COVID-19 pandemic)<br><br>
                      Color of lines indicates flight volume (darkest shade of grey for most flown route)<br><br>
                      <b>Limitations</b><br>
                      Only condsiders flights of aircraft that are listed in aircraft selection<br>
                      Only top 100 airports are considered for individual views<br>
                      Slow initial load time because all data needs to be read in from data source<br>
                      If chosen selection of airport and aircraft is too narrow to return enough data to visualize, the app automatically requires to be reloaded<br>
                      Excluding airports with less than 10 flights per year under chosen airport and aircraft selection (for reasons of increasing performance of visualization)<br>
                      <br>Source: FAA Aviation System Performance Metrics (ASPM)<br><br>
                      Contact: cweisgram2019@student.hult.edu <br><br>                           
                      © Clemens Weisgram 2021" 
  
  
  # for mere info about shinyalert refer to https://deanattali.com/blog/shinyalert-package/
  observeEvent(input$show_user_guide, shinyalert(
    title = "",
    text = user_guide_text,
    size = "m", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "",
    showConfirmButton = FALSE,
    showCancelButton = TRUE,
    cancelButtonText = "Dismiss",
    timer = 0,
    imageUrl = "",
    animation = FALSE
  ))
  
  showModal(shinyalert(
    title = "",
    text = user_guide_text,
    size = "m", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "",
    showConfirmButton = FALSE,
    showCancelButton = TRUE,
    cancelButtonText = "Dismiss",
    timer = 0,
    imageUrl = "",
    animation = FALSE
  ))
  
  #################
  library(tidyverse)
  library(readxl)
  library(dplyr)
  
  
  # setting working directory (only needed when running locally)
  #setwd("/Users/clemensweisgram/Desktop/Fleet Map")
  
  # importing master data
  #data <- read_excel("./master_data.xlsx")
  data <- read.csv("./master_data.csv")
  
  # importing airport coordinates
  airport_coordinates <- read_excel("./airport_coordinates.xlsx")
  
  # creating new column with route (with direction)
  data$route <- paste(data$departure, " - ", data$arrival)
  
  # creating new column with route (direction in alphabetical order)
  data$route2 <- apply(cbind(data$departure, data$arrival), 1, function(x) paste(sort(x), collapse=" - "))
  
  #################
  
  
  observe({
    
    #################
    
    # filter airport (if applicable)
    if (input$airport_selection != "_ALL_"){
      data <- data %>%
        filter(departure == input$airport_selection | arrival == input$airport_selection)
    }
    
    if ("_ALL_" %in% input$aircraft_type_selection) {
      input_from_selection <- c("E545 - Embraer Legacy 450", "E550 - Embraer Legacy 500", "E50P - Embraer Phenom 100", 
                                "E55P - Embraer Phenom 300", "G650 - Gulfstream G650", "GA6C - Gulfstream G600", 
                                "G550 - Gulfstream G550", "GLEX - Bombardier Global Express", 
                                "GL7T - Bombardier Global 7500", "GL5T - Bombardier Global 5000", 
                                "CL60 - Bombardier Challenger 600/601/604", "CL35 - Bombardier Challenger 350", 
                                "CL30 - Bombardier Challenger 300", "C56X - Cessna Citation Excel/XLS", 
                                "C525 - Cessna Citation CJ1", "C25A - Cessna Citation CJ2", 
                                "C25B - Cessna Citation CJ3", "C25C - Cessna Citation CJ4", 
                                "C25M - Cessna Citation M2", "C510 - Cessna Ciation Mustang", 
                                "C52B - Cessna Cittion 525/526", "C55B - Cessna Citation Bravo", 
                                "C560 - Cessna Cittion V/Ultra/Encore", "C680 - Cessna Citation Sovereign", 
                                "C68A - Cessna Citation Latitude", "C700 - Cessna Citation Longitude", 
                                "C750 - Cessna Citation X", "H25 - Raytheon Hawker 800", "BE40 - Raytheon Hawker 400", 
                                "LJ - Bombardier Learjet", "G280 - Gulfstream G280", "F900 - Dassault Falcon 900", 
                                "FA7X - Dassault Falcon 7X", "FA8X - Dassault Falcon 8X", "F2TH - Dassault Falcon 2000", 
                                "F900 - Dassault Falcon 900", "G150 - Gulfstream G150", "PRM1 - Raytheon Premier 1", "FA50 - Dassault Falcon 50")
    } else {
      input_from_selection <- input$aircraft_type_selection
    }
    
    
    
    
    # filter aircraft type
    data <- data %>%
      filter(aircraft_type %in% input_from_selection)
    
    
    data_3 <- data %>%
      group_by(route2) %>%
      summarise(sum_flight_count_route = sum(flight_count)) %>%
      arrange(desc(sum_flight_count_route)) %>%
      separate(route2, c("airport1", "airport2"), sep = " - ", remove = FALSE)
    
    data_3 <- merge(data_3, airport_coordinates, by.x = "airport1", by.y = "airport")
    
    data_3 <- merge(data_3, airport_coordinates, by.x = "airport2", by.y = "airport")
    
    # filter top n
    data_3 <- data_3 %>%
      drop_na() %>%
      filter(sum_flight_count_route >= 10) %>%
      arrange(desc(sum_flight_count_route)) %>%
      top_n(input$top_n, wt = sum_flight_count_route)
    
    colnames(data_3) <- c("airport2", "airport1", "route2", "sum_flight_count_route", "longitude1", "latitude1", "longitude2", "latitude2")
    
    
    data_3_1 <- select(data_3, "airport1", "route2", "sum_flight_count_route", "longitude1", "latitude1")
    data_3_2 <- select(data_3, "airport2", "route2", "sum_flight_count_route", "longitude2", "latitude2") 
    
    colnames(data_3_1) <- c("airport", "route2", "sum_flight_count_route", "longitude", "latitude")
    colnames(data_3_2) <- c("airport", "route2", "sum_flight_count_route", "longitude", "latitude")
    
    data_3 <- rbind(data_3_1, data_3_2)
    
    data_3$route2 <- as.factor(data_3$route2)
    
    library(leaflet)
    library(magrittr)
    library(RColorBrewer)
    
    domain <- range(data_3$sum_flight_count_route)
    pal <- colorNumeric(palette = colorRampPalette(brewer.pal(9,"Greys")[5:9])(100), domain = domain)
    
    output$map <- leaflet::renderLeaflet({
      myMap <- leaflet() %>%
        setView( -98.5795, 39.8283, zoom = 5) %>% 
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) # uses more neutral base map
      
      for(group in levels(data_3$route2)){
        myMap <- addPolylines(myMap,
                              lng= ~ longitude,
                              lat= ~ latitude,
                              data = data_3[data_3$route2 == group,],
                              color= ~ pal(sum_flight_count_route),
                              weight = 3,
                              popup = ~ route2)
      }
      
      myMap # calling the map
      
    }) # closing renderLeaflet

  }) #closing observe
}

shinyApp(ui, server)