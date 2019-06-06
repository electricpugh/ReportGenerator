# Packages to Load
require(RODBC)
require(tidyr)
require(ggplot2)
require(dplyr)
require(lubridate)

source("scripts.R") # Load scripts file 

ADRS_con <- odbcDriverConnect("Driver={Oracle in OraClient11g_home1};Dbq=sde8;Uid=adrs_viewer;Pwd=adrs_viewer2005;")
stationselector_list <- sqlQuery(channel = ADRS_con, query = "select * from ADRS.STATIONS")
# stationselector_list <- sqlQuery(channel = ADRS_con, query = "select STAT_NAME from ADRS.STATIONS order by STAT_NAME asc")
odbcClose(ADRS_con)

ui <- navbarPage(title = "Real-Time Water Quality Report Generator",
                 id = "tabs",
                 selected = NULL,
                 position = 'static-top',
                 inverse = TRUE,
                 collapsible = TRUE,
                 fluid = TRUE,
                 tabPanel("Select Data",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('stationselector',
                                          label = "Station Name",
                                          choices = sort(stationselector_list$STAT_NAME),
                                          multiple = TRUE,
                                          selected = NULL),
                              dateRangeInput('dateselector', label = "Date Range"),
                              radioButtons('includeextrahydro_tf',
                                           label = "Include Extra Hydrometric Data?",
                                           choices = c("Yes" = "TRUE",
                                                       "No"  = "FALSE"),
                                           selected = "Yes"),
                              actionButton('stationselector_go', "Load Data")
                            ),
                            mainPanel(
                              verbatimTextOutput('test2')
                            )
                          )),
                 tabPanel("Edit Data"),
                 tabPanel("Export Data")
)

# Server-side Code --------------------------------------------------------
server <- function(input, output, session) {
  raw <- reactiveValues()
  
  raw$data <- eventReactive(input$stationselector_go, { # Creates ADRS query for user-selected station and dates.
    x <- list()
    y <- input$includeextrahydro_tf
    #station      <- stationselector_list[stationselector_list$STAT_NAME == input$stationselector, 1]
    station      <- stationselector_list[stationselector_list$STAT_NAME %in% input$stationselector, 1]
    date_from    <- input$dateselector[1]
    date_to      <- input$dateselector[2]
    progress     <- Progress$new(session, min = 0, max = 1)
    progress$set(message = "Gathering data", value = 0)
    for (i in 1:length(station)) {
      progress$set(value = 0.2)
      query <- paste0("select * from ADRS.L_", station[i], " where NST_DATI between to_date('", date_from, "','YYYY/MM/DD') and to_date('", date_to, "','YYYY/MM/DD')")
      progress$set(value = 0.4)
      progress$set(value = 0.6)
      progress$set(message = "Gathering data complete", value = 1)
      Sys.sleep(0.25)
      progress$close()
      x[[i]] <- sqlQuery(channel = odbcDriverConnect("Driver={Oracle in OraClient11g_home1};Dbq=sde8;Uid=adrs_viewer;Pwd=adrs_viewer2005;"), query = query)
    }
    return(lapply(x, groom, y = TRUE))
  }, label = "Get data")
  output$test2 <- renderPrint(raw$data())
}

shinyApp(ui = ui, server = server)