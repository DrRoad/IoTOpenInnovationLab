library(shiny)
library(shinydashboard)
library(httr)
library(leaflet)
library(dplyr)
library(ggmap)
library(geosphere)
library(data.table)
library(scales)
library(plotly)
library(lazyeval)
library(shinyjs)
library(mongolite)
library(plotly)
library(DT)
library(data.table)
library(dplyr)
#####################################
# UI Header #
#####################################

header <- dashboardHeader(title = "IoT OpenInnovation Lab")

  sidebar <- dashboardSidebar(
    width = 200,
    sidebarMenu(id = "sidebarmenu",
                p(),
                # Service center stats
                menuItem("Temperature", tabName = "newTempTab",icon = icon("bar-chart"), selected = FALSE),
                selectInput("month",label = "Select Month",
                            c("All","May","June","July","August","September","October","November"),
                            multiple = FALSE,selected = c("All")),
                useShinyjs(),
                # actionButton("refreshData", "Refresh", class = "customButton")
                downloadButton('downloadData', 'Download'),
                p(),
                
                downloadButton('downloadDataRaw', 'Download Raw Data'),
                p("Download the entire raw data")
                )
    )



body <- dashboardBody(
  includeScript("www/scripts.js"),
  includeScript('www/spin.min.js'),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "Control.Loading.css")
  ),
  tabItems(
    tabItem(tabName = "newTempTab",
            includeScript('www/Control.Loading.js'),
            # Value Info Box
            fluidRow(
              valueBoxOutput("date",width = 5),
              valueBoxOutput("temp",width = 6)
            ),
            h2("Temperature Change"),
            plotlyOutput("plotDateTemp"),
            h2("Humidity Check"),
            plotlyOutput("plotDateTemp_2"),
            # h2("Comfort Zone"),
            # plotlyOutput("comfort"),
            h2("API Temperature Data Boston"),
            DT::dataTableOutput("tempNew")
            
    )
  )
)

dashboardPage(header, sidebar, body)
