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
                              tableOutput('raw')
                            )
                          )),
                 tabPanel("Edit Data"),
                 tabPanel("Export Data")
)

# Server-side Code --------------------------------------------------------
server <- function(input, output, session) {
  raw <- reactiveValues()
  # raw_station_number <- reactiveVal()
  # raw_station_number$value <- stationselector_list[1, stationselector_list$STAT_NAME == input$stationselector]
  raw_query <- reactiveVal()
  raw_query$value <- paste0("select * from ADRS.L_", stationselector_list[1, stationselector_list$STAT_NAME == input$stationselector], "where NST_DATI between to_date('", input$dateselector[1], "','YYYY/MM/DD') and to_date('", input$dateselctor[2], "','YYYY/MM/DD')")
  raw$data <- eventReactive(input$stationselector_go, {
    sqlQuery(channel = ADRS_con, query = raw_query)
  })
  output$raw <- renderTable(raw$data())
}
test
shinyApp(ui = ui, server = server)