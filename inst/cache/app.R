library("shiny")
library("shinyjs")
library("shinydashboard")
library("readxl")
library(shinyBS)
library(shinyFiles)
library(DT)
library(brapi)
library(stringr)



# cacheLocationData <- function(){
#   pb <- progress::progress_bar$new(total = 1e7, clear = FALSE, width = 60,
#                                    format = "  downloading :what [:bar] :percent eta: :eta")
#   pb$tick(1, tokens = list(what = "connect to db"))
#
#   if(!brapi::can_internet()) return(NULL)
#   pb$tick(1e7/3, tokens = list(what = "retrieve data"))
#   locs = NULL
#   try({
#     brapi = brapi_con("sweetpotato",
#                       paste0(login$sgn, dburl ),
#                       80, login$user, login$login)
#     locs = brapi::locations_list()
#   })
#   #print(locs)
#   if(!is.null(locs)){
#     locs = readRDS("www/HIDAP/locs.rds")
#   }
#   if(nrow(locs) > 0){
#     pb$tick(1e7/2, tokens = list(what = "cache data"))
#     saveRDS(locs, file = fileLocs)
#     pb$tick(1e7, tokens = list(what = "finished data download"))
#   }
#
# }

dburl = "sweetpotatobase-test.sgn.cornell.edu"
locos = Sys.getenv("OS")
login <- yaml::yaml.load_file("login.yaml")
brapi <<- brapi_con("sweetpotato",
                  paste0(login$sgn, dburl ),
                  80, login$user, login$login)




if(str_detect(locos, "Windows")){
  hddir = file.path(Sys.getenv("LOCALAPPDATA"), "HIDAP")
} else {
  hddir = file.path("www", "HIDAP")
}
if(!dir.exists(hddir)) dir.create(hddir, recursive = TRUE)
fileLocs = file.path(hddir, "locs.rds")


cacheLocationData <- function(){
pb <- progress::progress_bar$new(total = 1e7, clear = FALSE, width = 60,
                                 format = "  downloading :what [:bar] :percent eta: :eta")
pb$tick(1, tokens = list(what = "connect to db"))

#if(!brapi::can_internet()) return(NULL)
pb$tick(1e7/3, tokens = list(what = "retrieve data"))
locs = NULL

try({
  if(brapi::can_internet()){
    brapi_auth(login$user, login$login)
    locs = brapi::locations_list()
  }
})
#(locs)

if(!is.null(locs) & nrow(locs) > 0){
  pb$tick(1e7/2, tokens = list(what = "cache data"))
  saveRDS(locs, file = fileLocs)
  pb$tick(1e7, tokens = list(what = "finished data download"))
}
locs
}

locs <- cacheLocationData()

locsData <- readRDS("www/HIDAP/locs.rds")



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


  if(!is.null(locs)){
    sharedValues[['data']] = readRDS("www/HIDAP/locs.rds")
  }


  withProgress(message = "Downloading location data", {
    locsData <- reactiveFileReader(10000, session, filePath = fileLocs, readRDS)
  })



  observe({
    invalidateLater(1000, session)
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


