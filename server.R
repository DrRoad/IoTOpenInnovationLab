#####################################
# Map Related Globals #
#####################################
zoomLevel = 18
defaultLong = -71.087787
defaultLat = 42.340273

tcp_map <- "https://api.mapbox.com/styles/v1/mapbox/dark-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibWF2ZXpkYWJhcyIsImEiOiJjaXExaHRpcGowMHl6ZmhubnlqZzZ1djE5In0.EdKaYkoXHl-u94z64QAB6Q"


mb_attribution <- "© Mavez Singh Dabas © <a href='https://www.linkedin.com/in/mavezdabas'>Linkedin</a>"

#####################################
# Functions #
#####################################
averageValue <- function(dataset,type) {
  if (type == "TempEnergy") {
    avgTemp <- round(mean(dataset$Temperature),2)
    avgEnergy <- round(mean(dataset$KWE),2)
  }else{
    avgTemp <- round(mean(dataset$Temperature),2)
    avgEnergy <- round(mean(dataset$KWE),2)
  }
 return(list(avgTemp,avgEnergy))
}


makePlot <- function(buildData,buildName ,newZoomLevel = zoomLevel){
  buildData <- buildData %>% filter(Building == buildName) ## Get rid of SelectedSC summary
  currentLon <- buildData$Long
  currentLat <- buildData$Lat
  
  leafletProxy("mainMap") %>%
    setView(lng=currentLon[length(currentLon)], lat = currentLat[length(currentLat)], zoom = newZoomLevel) %>%
    addMarkers(lng=currentLon, lat=currentLat)
  return(as.data.frame(cbind(currentLon, currentLat)))
}

clearMap <- function(){
  leafletProxy("mainMap") %>% clearMarkers() %>% clearShapes() %>% clearMarkerClusters()
}


##############################
# Data Frames  #
##############################


# New Temperature Data
# temperatureNew <- read.csv("./data/Temperature_Data.csv",header = TRUE)
# [NOTE: Latest data format] 

# August
august_Data <- fread('https://www.wunderground.com/history/airport/KBOS/2016/8/31/MonthlyHistory.html?req_city=Boston&req_state=MA&req_statename=Massachusetts&reqdb.zip=02120&reqdb.magic=1&reqdb.wmo=99999&format=1')
# September
september_Data <- fread('https://www.wunderground.com/history/airport/KBOS/2016/9/1/MonthlyHistory.html?req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1')
# October
october_Data <- fread('https://www.wunderground.com/history/airport/KBOS/2016/10/31/MonthlyHistory.html?req_city=Boston&req_state=MA&req_statename=Massachusetts&reqdb.zip=02120&reqdb.magic=1&reqdb.wmo=99999&format=1')

# Combining the Data 
boston_Temp <- rbind(august_Data,september_Data,october_Data)
# [NOTE: Removing the wind direction column]
temperatureNew <- boston_Temp %>%
  dplyr::select(-length(boston_Temp))
# [NOTE: Changing the events based on dates]
temperatureNew$Events <- rapply(as.list(temperatureNew$Events),function(x){ifelse(x == "","Normal",x)})
temperatureNew$Events <- rapply(as.list(temperatureNew$Events),function(x){ifelse(x == "Rain-Thunderstorm","Thunderstorm",x)})
temperatureNew$Events <- rapply(as.list(temperatureNew$Events),function(x){ifelse(x == "Fog-Rain","Rain",x)})
temperatureNew$Events <- rapply(as.list(temperatureNew$Events),function(x){ifelse(x == "Fog-Rain-Thunderstorm","Thunderstorm",x)})

# [NOTE: Adding Comfortable Zones]
temperatureNew$`MeanDew PointF`


# Building Data
buildingData <- read.csv("./data/locationBuilding.csv",header = TRUE)

##############################
# Server #
##############################
server <- function(input,output,session) {
  rValues <- reactiveValues()
  
  # Initialize as n = 1
  number <- 1
  
  ## Initial rendering of the map
  output$mainMap <- renderLeaflet({ leaflet() %>% 
      setView(lng = defaultLong, lat = defaultLat, zoom = zoomLevel) %>%
      addTiles(urlTemplate = tcp_map,attribution = mb_attribution)

  })
  
  observeEvent(input$refreshData,{
    withProgress({
      
      # Show Table
      temperatureData <- temperatureNew %>%
        dplyr::select(EDT,`Mean TemperatureF`,`MeanDew PointF`,
                      `Mean Humidity`,`Mean Sea Level PressureIn`,
                      `Mean VisibilityMiles`,`Mean VisibilityMiles`,
                      CloudCover,Events) %>%
        dplyr::rename(Date = EDT,TemperatureF = `Mean TemperatureF`,
                      DewPointF = `MeanDew PointF`,Humidity = `Mean Humidity`,
                      Sea_Pressure = `Mean Sea Level PressureIn`,
                      Visibility = `Mean VisibilityMiles`)
      
      temperatureData$Date <- as.Date.character(temperatureData$Date, "%Y-%m-%d")
      
      output$tempNew <- renderDataTable(temperatureData)
      #---------------------------------------------------------------
      # Plot Date and Temoerature
      output$plotDateTemp <- renderPlotly(
                plot_ly(temperatureData, y = ~TemperatureF, x = ~Date,
                        type = 'scatter',mode = 'line')
              )
      
      output$plotDateTemp_2 <- renderPlotly(
        plot_ly(temperatureData, x = ~Humidity, y = ~TemperatureF,
                text = ~paste("Date: ", Date, '<br>Humidity:', Humidity),
                color = ~Humidity, size = ~Humidity)
      )
      
      
      #---------------------------------------------------------------
      # output$date <- renderValueBox({
      #   valueBox(
      #     value = temperatureNew$Date[(nrow(temperatureNew))],
      #     subtitle = "Date",
      #     icon = icon("arrow-down"),
      #     color = "black"
      #   )})
      
      # Show Daily Temperature
      # output$temp <- renderValueBox({
      #   valueBox(
      #     value = sprintf("Temperature: %s", temperatureNew$Temp_Avg[(nrow(temperatureNew))]),
      #     subtitle = paste(sprintf("High: %s", temperatureNew$Temp_High[nrow(temperatureNew)]),
      #                      sprintf("Low: %s", temperatureNew$Temp_Low[nrow(temperatureNew)])),
      #     icon = icon("arrow-down"),
      #     color = "green"
      #   )})
      
      
      # output$humidity <- renderValueBox({
      #   valueBox(
      #     value = sprintf("Humidity: %s", temperatureNew$Humidity_Avg[(nrow(temperatureNew))]),
      #     subtitle = paste(sprintf("High: %s", temperatureNew$Humidity_High[nrow(temperatureNew)]),
      #                      sprintf("Low: %s", temperatureNew$Humidity_Low[nrow(temperatureNew)])),
      #     icon = icon("arrow-down"),
      #     color = "yellow"
      #   )})
      
      
      # output$wind <- renderValueBox({
      #   valueBox(
      #     value = sprintf("Wind: %s", temperatureNew$Wind_Avg[(nrow(temperatureNew))]),
      #     subtitle = paste(sprintf("High: %s", temperatureNew$Wind_High[nrow(temperatureNew)]),
      #                      sprintf("Low: %s", temperatureNew$Wind_Low[nrow(temperatureNew)])),
      #     icon = icon("arrow-down"),
      #     color = "blue"
      #   )})
    
      
    }
    )
  }
  )
}











