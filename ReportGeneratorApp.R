# Packages to Load
require(RODBC)
require(tidyr)
require(ggplot2)

ADRS_con <- odbcDriverConnect("Driver={Oracle in OraClient11g_home1};Dbq=sde8;Uid=adrs_viewer;Pwd=adrs_viewer2005;")
stationselector_list <- sqlQuery(channel = ADRS_con, query = "select * from ADRS.STATIONS")
# stationselector_list <- sqlQuery(channel = ADRS_con, query = "select STAT_NAME from ADRS.STATIONS order by STAT_NAME asc")
odbcClose(ADRS_con)

ui <- navbarPage(title = "Real-Time Water Quality Report Generator",
                 id = NULL,
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
                                          choices = sort(stationselector_list$STAT_NAME)),
                              dateRangeInput('dateselector', label = "Date Range"),
                              actionButton('stationselector_go', "Submit")
                            ),
                            mainPanel(
                              tableOutput('test')
                            )
                          )),
                 tabPanel("Edit Data"),
                 tabPanel("Export Data")
)

# Server-side Code --------------------------------------------------------
server <- function(input, output, session) {
  raw <- reactiveValues()
  
  raw$data <- eventReactive(input$stationselector_go, {
    station <- stationselector_list[stationselector_list$STAT_NAME == input$stationselector, 1]
    date_from    <- input$dateselector[1]
    date_to      <- input$dateselector[2]
    query        <- paste0("select * from ADRS.L_", station, " where NST_DATI between to_date('", date_from, "','YYYY/MM/DD') and to_date('", date_to, "','YYYY/MM/DD')")
    return(sqlQuery(channel = odbcDriverConnect("Driver={Oracle in OraClient11g_home1};Dbq=sde8;Uid=adrs_viewer;Pwd=adrs_viewer2005;"), query = query))
  })
  
  output$test <- renderTable(raw$data())
}

shinyApp(ui = ui, server = server)