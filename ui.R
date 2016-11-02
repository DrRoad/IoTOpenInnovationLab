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
                            c("All","May","June","July","August","September","October"),
                            multiple = FALSE,selected = c("All")),
                useShinyjs(),
                # actionButton("refreshData", "Refresh", class = "customButton")
                downloadButton('downloadData', 'Download')
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
            p("Domain situation, Data, Visual Encoding, Algorithms"),
            p("Most common approach is top down or we can also start with 
              bottom up. Have a new visualization and create the data as per
              your needs. Working backwards."),
            p("USer study, Lab study and we can alsoohave a new user seetign"),
            p("User reactions and user inputs"),
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
