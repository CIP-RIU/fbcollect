library("shiny")
library("shinyjs")
library("shinydashboard")
library("readxl")
library(shinyBS)
library(shinyFiles)
library(DT)
library(brapi)
library(stringr)


dburl = "sweetpotatobase-test.sgn.cornell.edu"
locos = Sys.getenv("OS")

if(str_detect(locos, "Windows")){
  hddir = file.path(Sys.getenv("LOCALAPPDATA"), "HIDAP")
} else {
  hddir = ""
}
  if(!dir.exists(hddir)) dir.create(hddir, recursive = TRUE)


login <- yaml::yaml.load_file("login.yaml")

session = NULL
fileLocs = file.path(hddir, "locs.rds")

cacheLocationData <- function(){
  pb <- progress::progress_bar$new(total = 1e7, clear = FALSE, width = 60,
                                   format = "  downloading :what [:bar] :percent eta: :eta")
  pb$tick(1, tokens = list(what = "connect to db"))

  if(can_internet(dburl)){
    pb$tick(1e7/2, tokens = list(what = "retrieve data"))
    brapi = brapi_con("sweetpotato",
                      paste0(login$sgn, dburl ),
                      80, login$user, login$login)
    locs = brapi::locations_list()
    if(nrow(locs) > 0){
      pb$tick(1e7, tokens = list(what = "cache data"))
      saveRDS(locs, file = fileLocs)
      #pb$tick(1e7, tokens = list(what = "finished data download"))
    }
  }
}

cacheLocationData()

locsData <- reactiveFileReader(600000, session, filePath = fileLocs, readRDS)

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "HIDAP collect"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      shinyjs::useShinyjs(),
                      tabBox(width = 12, selected = "Data source", height = 500,
                             tabPanel("Data source",
                              fluidRow(
                                column(4,
                                  radioButtons("crop", "Crop",
                                               c("potato", "sweetpotato", "cassava",
                                                         "yam", "musa"), "sweetpotato",
                                               inline = TRUE
                                               ),
                                  radioButtons("sourceType", "Data source type",
                                               choices = c("Local",
                                                           "BrAPI",
                                                           "CloneSelector", "DataCollector",
                                                           "AccuDataLog", "Fieldbook App"),
                                               selected = "Local", inline = TRUE)

                                       ),

                                column(8,
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
)



server <- function(input, output, session) {

  sharedValues <- reactiveValues()

  observe({
    if(file.exists(fileLocs)){
      sharedValues[['data']] <- locsData()
    } else {
      cacheLocationData()
    }

  })

  output$cache_test <- DT::renderDataTable({
    sharedValues[['data']]
  }, options = list(scrollX = TRUE))

}

shinyApp(ui = ui, server = server)


