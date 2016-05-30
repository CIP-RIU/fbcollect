
#' tab_dataSource
#'
#' @return shiny object
#' @import shiny
#' @import shinydashboard
#' @export
tab_dataSource <- function(){
  tabItem(tabName = "dashboard_source",

    shinyjs::useShinyjs(),
    tabBox(width = 12, selected = "Data source", height = 500,
           tabPanel("Data source",
                    fluidRow(
                      column(3,
                             uiOutput("sourceType"),
                             uiOutput("cropType"),
                             conditionalPanel("input.sources == 'BrAPI'",
                                              radioButtons("brapi_db", "BTI DB:",
                                                           c("sweetpotatobase-test.sgn.cornell.edu",
                                                             "sweetpotatobase.org")),
                                              numericInput("brapi_port", "Port", 80, 80, 3000),
                                              textInput("user", "User:"),
                                              passwordInput("password", "Password:"),
                                              textInput("proxy", "Proxy:", "sgn:eggplant")
                             ),
                             actionButton("connectSource", "Connect Source"),
                             conditionalPanel("input.sources == 'Local'",
                                              radioButtons("importFrom", "ImportFrom",
                                                           c("CSV",
                                                             "CloneSelector",
                                                             "DataCollector",
                                                             "AccuDataLog",
                                                             "FieldbookAPP",
                                                             "Collect"), inline = TRUE)
                             ),
                             hr()
                      ),


                      column(9,
                             tabBox(width = 12,
                                    tabPanel("Locations",
                                             DT::dataTableOutput("cache_test")
                                    )
                             )


                      )
                    )
           )

    )
  )
}

