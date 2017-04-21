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
# devtools::install_github("jcheng5/googleCharts",force = TRUE)
library(googleCharts)

# Use global max/min for axes so the view window stays
# constant as the user moves between years
# xlim <- list(
#   min = min(data$Health.Expenditure) - 500,
#   max = max(data$Health.Expenditure) + 500
# )
# ylim <- list(
#   min = min(data$Life.Expectancy),
#   max = max(data$Life.Expectancy) + 3
# )


if (!("plotrix" %in% names(installed.packages()[,"Package"]))) {install.packages("plotrix")}
suppressMessages(library(plotrix, quietly = TRUE))

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("openintro" %in% names(installed.packages()[,"Package"]))) {install.packages("openintro")}
suppressMessages(library(openintro, quietly = TRUE))


#####################################
# UI Header #
#####################################

header <- dashboardHeader(title = "IoT OpenInnovation Lab")

  sidebar <- dashboardSidebar(
    width = 200,
    sidebarMenu(id = "sidebarmenu",
                p(),
                # Service center stats
                menuItem("Temperature", tabName = "newTempTab",icon = icon("bar-chart"), selected = TRUE),
                conditionalPanel("input.sidebarmenu == 'newTempTab'",
                                 useShinyjs(),  
                                 selectInput("month",label = "Select Month",
                                             c("All","May","June","July","August","September","October","November"),
                                             multiple = FALSE,selected = c("All")),
                                 downloadButton('downloadData', 'Download'),
                                 p(),
                                 downloadButton('downloadDataRaw', 'Download Raw Data')
                ),
                menuItem("Analysis", tabName = "analysis",icon = icon("spinner"), selected = FALSE),
                conditionalPanel("input.sidebarmenu == 'analysis'",
                                 selectInput("month",label = "Select Month",
                                             c("All","May","June","July","August","September","October","November"),
                                             multiple = FALSE,selected = c("All"))
                ),
                menuItem("Simulation", tabName = "simulation",icon = icon("spinner"), selected = FALSE)
                # conditionalPanel("input.sidebarmenu == 'simulation'",
                #                  sliderInput("year", "Year",
                #                              min = min(dataHealth$Year), max = max(dataHealth$Year),
                #                              value = min(dataHealth$Year), animate = TRUE)
                # )
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
           
            
            
            # p("Designe project and Resarch project. Pairing professors.
            #   Also integrate IoT security."),
            # p("Having a wider range of students."),
            # p("Having a spearker series and use progessor and industrial experts
            #   to bring more people into the platform. Bring in motivatuial
            #   speakers."),
            # p("Thinking about and doing about the marketing with somethign 
            #   Actually real working on"),
            # p("Game the process and reaction from the users.")
            
    ),
    tabItem(tabName = "analysis",
            h2("Analysis tab content"),
            plotOutput("scatter"),
            br(),
            br(),
            plotOutput("residuals")
    )
    # tabItem(tabName = "simulation",
    #         h2("Simulation Example"),
    #         googleChartsInit(),
    #         # Use the Google webfont "Source Sans Pro"
    #         # tags$link(
    #         #   href=paste0("http://fonts.googleapis.com/css?",
    #         #               "family=Source+Sans+Pro:300,600,300italic"),
    #         #   rel="stylesheet", type="text/css"),
    #         tags$style(type="text/css",
    #                    "body {font-family: 'Source Sans Pro'}"
    #         ),
    #         br(),
    #         googleBubbleChart("chart",
    #                           width="100%", height = "475px",
    #                           # Set the default options for this chart; they can be
    #                           # overridden in server.R on a per-update basis. See
    #                           # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
    #                           # for option documentation.
    #                           options = list(
    #                             fontName = "Source Sans Pro",
    #                             fontSize = 13,
    #                             # Set axis labels and ranges
    #                             hAxis = list(
    #                               title = "Health expenditure, per capita ($USD)"
    #                               # viewWindow = xlim
    #                             ),
    #                             vAxis = list(
    #                               title = "Life expectancy (years)"
    #                               # viewWindow = ylim
    #                             ),
    #                             # The default padding is a little too spaced out
    #                             chartArea = list(
    #                               top = 50, left = 75,
    #                               height = "75%", width = "75%"
    #                             ),
    #                             # Allow pan/zoom
    #                             explorer = list(),
    #                             # Set bubble visual props
    #                             bubble = list(
    #                               opacity = 0.4, stroke = "none",
    #                               # Hide bubble label
    #                               textStyle = list(
    #                                 color = "none"
    #                               )
    #                             ),
    #                             # Set fonts
    #                             titleTextStyle = list(
    #                               fontSize = 16
    #                             ),
    #                             tooltip = list(
    #                               textStyle = list(
    #                                 fontSize = 12
    #                               )
    #                             )
    #                           )
    #         )
    # 
    # )
  )
)

dashboardPage(header, sidebar, body)
