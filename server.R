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

# data <- fread("http://www.wunderground.com/history/airport/BOS/2011/1/1/CustomHistory.html?dayend=28&monthend=2&yearend=2017&req_city=NA&req_state=NA&req_statename=NA&format=1")

# May
may_Data <- fread("https://www.wunderground.com/history/airport/KBOS/2016/5/1/MonthlyHistory.html?req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1")
# June
june_Date <- fread("https://www.wunderground.com/history/airport/KBOS/2016/6/1/MonthlyHistory.html?req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1")
# July
july_Data <- fread("https://www.wunderground.com/history/airport/KBOS/2016/7/1/MonthlyHistory.html?req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1")
# August
august_Data <- fread('https://www.wunderground.com/history/airport/KBOS/2016/8/31/MonthlyHistory.html?req_city=Boston&req_state=MA&req_statename=Massachusetts&reqdb.zip=02120&reqdb.magic=1&reqdb.wmo=99999&format=1')
# September
september_Data <- fread('https://www.wunderground.com/history/airport/KBOS/2016/9/1/MonthlyHistory.html?req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1')
# October
october_Data <- fread('https://www.wunderground.com/history/airport/KBOS/2016/10/31/MonthlyHistory.html?req_city=Boston&req_state=MA&req_statename=Massachusetts&reqdb.zip=02120&reqdb.magic=1&reqdb.wmo=99999&format=1')
# November
november_Data <- fread("https://www.wunderground.com/history/airport/KBOS/2016/11/30/MonthlyHistory.html?req_city=&req_state=&req_statename=&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1")
# november_Data <- november_Data %>%
#   dplyr::rename(EDT = EST)

# Combining the Data 
boston_Temp <- rbind(may_Data,june_Date,july_Data,august_Data,september_Data,
                     october_Data,november_Data,fill=TRUE)
# [NOTE: Removing the wind direction column]
temperatureNew <- boston_Temp %>%
  dplyr::select(-length(boston_Temp)) %>%
  dplyr::rename(Date = EDT)

temperatureNew$Date <- as.Date.character(temperatureNew$Date, "%Y-%m-%d")

# [NOTE: Changing the events based on dates]
temperatureNew$Events <- rapply(as.list(temperatureNew$Events),function(x){ifelse(x == "","Normal",x)})
temperatureNew$Events <- rapply(as.list(temperatureNew$Events),function(x){ifelse(x == "Rain-Thunderstorm","Thunderstorm",x)})
temperatureNew$Events <- rapply(as.list(temperatureNew$Events),function(x){ifelse(x == "Fog-Rain","Rain",x)})
temperatureNew$Events <- rapply(as.list(temperatureNew$Events),function(x){ifelse(x == "Fog-Rain-Thunderstorm","Thunderstorm",x)})

# [NOTE: Adding Comfortable Zones]
temperatureNew$`MeanDew PointF`
temperatureNew$CZone <- "NA"
temperatureNew$CZone <- ifelse(temperatureNew$`MeanDew PointF` <= 60,"Comfortable",temperatureNew$CZone)
temperatureNew$CZone <- ifelse(temperatureNew$`MeanDew PointF` > 60 & temperatureNew$`MeanDew PointF` <= 75,
       "Sticky",temperatureNew$CZone)
temperatureNew$CZone <- ifelse(temperatureNew$`MeanDew PointF` > 75,
       "Miserable",temperatureNew$CZone)

temperatureNew$Month <- format(temperatureNew$Date, "%B")


# Building Data
buildingData <- read.csv("./data/locationBuilding.csv",header = TRUE)

# Charts Data
# data <- readRDS("./data/healthexp.Rds")
# write.csv(x = data,file = "Health.csv",row.names = FALSE)
# dataHealth <- read.csv("/Users/mavezsinghdabas/Desktop/New_Stuff/NEU/ResearchAssistant/Shiny_Portfolio/client/IoTOpenInnovationLab/data/Health.csv",header = TRUE)
# dataHealth$Region <- as.factor(dataHealth$Region)

# Dummy Data
n <- 250
x <- c(runif(n-2, 0, 4), 2, 2.1)
y <- 2*x + rnorm(n, sd=2)
draw.data <- data.frame(x=x,y=y)




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
  
  observe({
    withProgress({
      month <- input$month
      # Filter Data As per Month selected
      if (month == "August") {
        temperatureNew <- temperatureNew %>%
          dplyr::filter(Month == month)
      }else if(month == "September"){
        temperatureNew <- temperatureNew %>%
          dplyr::filter(Month == month)
      }else if(month == "October"){
        temperatureNew <- temperatureNew %>%
          dplyr::filter(Month == month)
      }else if(month == "July"){
        temperatureNew <- temperatureNew %>%
          dplyr::filter(Month == month)
      }else if(month == "June"){
        temperatureNew <- temperatureNew %>%
          dplyr::filter(Month == month)
      }else if(month == "May"){
        temperatureNew <- temperatureNew %>%
          dplyr::filter(Month == month)
      }else if(month == "November"){
        temperatureNew <- temperatureNew %>%
          dplyr::filter(Month == month)
      }else if(month == "December"){
        temperatureNew <- temperatureNew %>%
          dplyr::filter(Month == month)
      }else{
        temperatureNew <- temperatureNew
      }
      
      #---------------------------------------------------------------
      # Show Table
      temperatureData <- temperatureNew %>%
        dplyr::select(Date,`Mean TemperatureF`,`MeanDew PointF`,
                      `Mean Humidity`,`Mean Sea Level PressureIn`,
                      `Mean VisibilityMiles`,`Mean VisibilityMiles`,
                      CloudCover,Events) %>%
        dplyr::rename(TemperatureF = `Mean TemperatureF`,
                      DewPointF = `MeanDew PointF`,Humidity = `Mean Humidity`,
                      Sea_Pressure = `Mean Sea Level PressureIn`,
                      Visibility = `Mean VisibilityMiles`)
      
      output$tempNew <- renderDataTable(temperatureData)
      #---------------------------------------------------------------
      # Plot Date and Temperature
      output$plotDateTemp <- renderPlotly(
                plot_ly(temperatureData, y = ~TemperatureF, x = ~Date,
                        type = 'scatter',mode = 'line')
              )
      
      output$plotDateTemp_2 <- renderPlotly(
        plot_ly(temperatureData, x = ~Humidity, y = ~TemperatureF,
                text = ~paste("Month: ", Date, '<br>Humidity:', Humidity),
                color = ~Humidity, size = ~Humidity)
      )
      #---------------------------------------------------------------
      # Value box description
      output$date <- renderValueBox({
        valueBox(
          value = temperatureNew$Date[(nrow(temperatureNew))],
          subtitle = "Date",
          icon = icon("spinner"),
          color = "yellow"
        )})
      
      # Show Daily Temperature
      output$temp <- renderValueBox({
        valueBox(
          value = sprintf("Temp %s",round(mean(temperatureNew$`Mean TemperatureF`),2)),
          subtitle = paste(sprintf("High: %s",round(mean(temperatureNew$`Max TemperatureF`),2)),
                           sprintf("Low: %s", round(mean(temperatureNew$`Min TemperatureF`),2))),
          icon = icon("line-chart"),
          color = "green"
        )})
      #---------------------------------------------------------------
      
      #---------------------------------------------------------------
      # Download the csv files 
      output$downloadData <- downloadHandler(
        filename = function() { paste("raw_Date", '.csv', sep='') },
        content = function(file) {
          write.table(temperatureData,file,row.names = FALSE)
        }
      )
      
      # Download the entire CSV File
      output$downloadDataRaw <- downloadHandler(
        filename = function() { paste("Entire_Date", '.csv', sep='') },
        content = function(file) {
          write.table(temperatureNew,file,row.names = FALSE)
        }
        
      )
      
      
      # Dummy Rregession
      mydata <- draw.data
      
      lmResults <- reactive({
        regress.exp <- "y~x"
        lm(regress.exp, data=mydata)
      })
      
      # Show plot of points, regression line, residuals
      output$scatter <- renderPlot({
        data1 <- mydata
        x <- data1$x
        y <- data1$y

        #used for confidence interval
        xcon <- seq(min(x)-.1, max(x)+.1, .025)

        predictor <- data.frame(x=xcon)

        yhat <- predict(lmResults())
        yline <- predict(lmResults(), predictor)

        par(cex.main=1.5, cex.lab=1.5, cex.axis=1.5, mar = c(4,4,4,1))

        r.squared = round(summary(lmResults())$r.squared, 4)
        corr.coef = round(sqrt(r.squared), 4)

        plot(c(min(x),max(x))
             ,c(min(y,yline),max(y,yline)),
             type="n",
             xlab="x",
             ylab="y",
             main=paste0("Regression Model\n","(R = ", corr.coef,", ", "R-squared = ", r.squared,")"))


        newx <- seq(min(data1$x), max(data1$x), length.out=400)
        confs <- predict(lmResults(), newdata = data.frame(x=newx),
                         interval = 'confidence')
        preds <- predict(lmResults(), newdata = data.frame(x=newx),
                         interval = 'predict')

        polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = grey(.95), border = NA)
        polygon(c(rev(newx), newx), c(rev(confs[ ,3]), confs[ ,2]), col = grey(.75), border = NA)

        points(x,y,pch=19, col=COL[1,2])
        lines(xcon, yline, lwd=2, col=COL[1])
        box()
      })
    
      # Residual Plots
      output$residuals <- renderPlot({
        par(mfrow=c(1,3), cex.main=2, cex.lab=2, cex.axis=2, mar=c(4,5,2,2))
        residuals = summary(lmResults())$residuals
        predicted = predict(lmResults(), newdata = data.frame(x=mydata$x))
        plot(residuals ~ predicted, 
             main="Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals", 
             pch=19, col = COL[1,2])
        abline(h = 0, lty = 2)
        d = density(residuals)$y
        h = hist(residuals, plot = FALSE)
        hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
             col=COL[1,2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
        lines(density(residuals), col = COL[1], lwd = 2)
        qqnorm(residuals, pch=19, col = COL[1,2], main = "Normal Q-Q Plot of Residuals")
        qqline(residuals, col = COL[1], lwd = 2)
      }, height=280 )
      
      # Automated file
      # defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
      # series <- structure(
      #   lapply(defaultColors, function(color) { list(color=color) }),
      #   names = levels(dataHealth$Region)
      # )

  
      # yearData <- reactive({
      #   # Filter to the desired year, and put the columns
      #   # in the order that Google's Bubble Chart expects
      #   # them (name, x, y, color, size). Also sort by region
      #   # so that Google Charts orders and colors the regions
      #   # consistently.
      #   df <- dataHealth %>%
      #     dplyr::filter(Year == input$year) %>%
      #     dplyr::select(Country, Health.Expenditure, Life.Expectancy,
      #            Region, Population) %>%
      #     arrange(Region)
      # })
      
      # output$chart <- reactive({
      #   # Return the data and options
      #   list(
      #     data = googleDataTable(yearData()),
      #     options = list(
      #       title = sprintf(
      #         "Health expenditure vs. life expectancy, %s",
      #         input$year),
      #       series = series
      #     )
      #   )
      # })
      
    }
    )
  }
  )
}











