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
                # # Client Analysis MenuBar
                # menuItem("Temperature Analysis", tabName = "clientData", 
                #          icon = icon("globe"),selected = TRUE),
                # useShinyjs(),
                # selectInput("buildingName", label = "Select Building",
                #             c("Dodge Hall","Ell Hall"),
                #             multiple = FALSE, selected = c("Dodge Hall")),
                
                
                # Service center stats
                menuItem("Temperature", tabName = "newTempTab",icon = icon("bar-chart"), selected = FALSE),
                useShinyjs(),
                actionButton("refreshData", "Refresh", class = "customButton")
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
            # h2("Temperature/Air"),
            # Value Info Box
            # div(
            #   valueBoxOutput("date",width = 5),
            #   valueBoxOutput("temp",width = 5),
            #   valueBoxOutput("humidity",width = 5),
            #   valueBoxOutput("wind",width = 5)
            #   
            # ),
            h2("Temperature Change"),
            plotlyOutput("plotDateTemp"),
            h2("Humidity Check"),
            plotlyOutput("plotDateTemp_2"),
            h2("API Temperature Data Boston"),
            DT::dataTableOutput("tempNew")
            
    )
  )
)

dashboardPage(header, sidebar, body)
