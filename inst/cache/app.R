library("shiny")
library("shinyjs")
library("shinydashboard")
library("readxl")
library(shinyBS)
library(shinyFiles)
library(DT)
library(brapi)
library(stringr)
library(magrittr)




dburl = "sweetpotatobase-test.sgn.cornell.edu"
locos = Sys.getenv("OS")
login <- yaml::yaml.load_file("login.yaml")
brapi <<- brapi_con("sweetpotato",
                  paste0(login$sgn, dburl ),
                  80, login$user, login$login)
is.server <- function(){
  login$mode == 'server'
}



if(str_detect(locos, "Windows")){
  hddir = file.path(Sys.getenv("LOCALAPPDATA"), "HIDAP")
} else {
  hddir = file.path("/Users", Sys.getenv("USER"),"Dcouments", "HIDAP")
}
if(!dir.exists(hddir)) {
  dir.create(hddir, recursive = TRUE)
  file.copy(system.file("/cache/www/HIDAP/Demo", package = 'fbcollect'), hddir, recursive = TRUE)
}
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

locs = NULL
#locs <- cacheLocationData()



ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "HIDAP collect"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      shinyjs::useShinyjs(),
                      tabBox(width = 12, selected = "Data source", height = 500,
                             tabPanel("Data source",
                              fluidRow(
                                column(3,
                                  uiOutput("sourceType"),
                                  conditionalPanel("input.sources == 'BrAPI'",
                                                   radioButtons("brapi_db", "BTI DB:",
                                                                c("sweetpotatobase-test.sgn.cornell.edu",
                                                                              "sweetpotatobase.org")),
                                                   numericInput("brapi_port", "Port", 80, 80, 3000)
                                  ),
                                  uiOutput("cropType"),
                                  conditionalPanel("input.sources == 'BrAPI'",
                                                   textInput("user", "User:"),
                                                   passwordInput("password", "Password:"),
                                                   textInput("proxy", "Proxy:", "sgn:eggplant")
                                                   ),
                                  actionButton("loadData", "Load data")
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

)



server <- function(input, output, session) {
  sharedValues <- reactiveValues()


  list_cache_crops <- function( src = input$sources){
    x = list.dirs(file.path( hddir, src, 'crops'))
    out = x[('crops' != basename(x))] %>% basename
    #print(out)
    if(length(out) == 0) return(NULL)
    out
  }


  get_data <-reactive({
    sharedValues[['data']] = NULL
    if(input$sources == 'Demo') {
      sharedValues[['data']] = readRDS(file.path(hddir,input$sources,"crops",input$crop,"locs.rds"))
    }
    if(input$sources == 'BrAPI' & input$crop == "sweetpotato") {
      withProgress(message="Connecting to DB ...",
      try({
        if(brapi::can_internet()){
          brapi <<- brapi_con(input$crop,
                              paste0(input$proxy, "@", input$brapi_db),
                              input$brapi_port, input$user, input$password)
          brapi_auth(input$user, input$password)
          sharedValues[['data']] = brapi::locations_list()
        }
      })
      )

    }

  })


  output$sourceType <- renderUI({
    src_all = c("Local", "Demo",
                "BrAPI",
                "CloneSelector", "DataCollector",
                "AccuDataLog", "FieldbookApp")
    if(is.server()){
      src_all = c("Demo",
                  "BrAPI")
    }
    radioButtons("sources", "Data source type",
                 choices = src_all,
                inline = TRUE)
  })

  output$cropType <- renderUI({
    req(input$sources)
    #print(input$sources)
    crops = c("potato", "sweetpotato", "cassava",
                  "yam", "musa")
    crops_all = crops
    if(input$sources == 'Demo') {
      crops_all = list_cache_crops()
    }
    if(input$sources == 'BrAPI' ) {

      crops_all = c("sweetpotato", "cassava",
                    "yam")
    }
    # print(crops_all)
    # print(getwd())
    # print(list_cache_crops())
    if(is.null(crops_all)) crops_all = crops
    radioButtons("crop", "Crop",
                 crops_all,
                 inline = TRUE
    )

  })



  observeEvent(input$loadData,{
    get_data()
  })

  output$cache_test <- DT::renderDataTable({
    req(sharedValues[['data']])
    sharedValues[['data']]
  }, options = list(scrollX = TRUE))

}

shinyApp(ui = ui, server = server)


