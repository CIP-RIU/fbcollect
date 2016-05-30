
#' srv_dataSource
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param values shiny reactive values
#' @export
srv_dataSource <- function(input, output, session, values){

  values <- reactiveValues()

  config = yaml::yaml.load_file("config.yaml")

  dburl = "sweetpotatobase-test.sgn.cornell.edu"

  login <- yaml::yaml.load_file("login.yaml")
  brapi <<- brapi_con("sweetpotato",
                      paste0(login$sgn, dburl ),
                      80, login$user, login$login)
  is.server <- function(){
    login$mode == 'server'
  }


  hddir = fbglobal::get_base_dir()

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



  list_cache_crops <- function( src = input$sources){
    x = list.dirs(file.path( hddir, src, 'crops'), recursive = FALSE)
    out = x[('crops' != basename(x))] %>% basename
    #print(out)
    if(length(out) == 0) return(NULL)
    out
  }


  get_data <-reactive({
    values[['data_loc']] = NULL
    if(input$sources == 'Demo') {
      values[['data_loc']] = readRDS(file.path(hddir,input$sources,"crops",input$crop,"locs.rds"))
    }
    if(input$sources == 'BrAPI' & input$crop == "sweetpotato") {
      withProgress(message="Connecting to DB ...",
                   try({
                     if(brapi::can_internet()){
                       brapi <<- brapi_con(input$crop,
                                           paste0(input$proxy, "@", input$brapi_db),
                                           input$brapi_port, input$user, input$password)
                       brapi_auth(input$user, input$password)
                       values[['data_loc']] = brapi::locations_list()
                     }
                   })
      )

    }

  })


  output$sourceType <- renderUI({
    src_all = c("Local", "Demo",
                "BrAPI")
    if(is.server()){
      src_all = c("Demo",
                  "BrAPI")
    }
    radioButtons("sources", "Data source type",
                 choices = src_all, config$source,
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
                 crops_all, config$crop,
                 inline = TRUE
    )

  })


  update_config <- function(src, crop){
    txt = c(paste("source:", src),
            paste("crop:", crop))
    writeLines(txt, con = "config.yaml")
  }

  observeEvent(input$connectSource,{
    # add the source and crop to yaml
    update_config(input$sources, input$crop)
    get_data()
  })

  output$cache_test <- DT::renderDataTable({
    req(values[['data_loc']])
    values[['data_loc']]
  }, options = list(scrollX = TRUE))

}
