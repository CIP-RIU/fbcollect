library("shiny")
library("shinyjs")
library("shinydashboard")
library("readxl")
library(shinyBS)
library(shinyFiles)
library(DT)

# db = readxl::read_excel("D:/simple_template.xlsx", "Fieldbook")
# tr = readxl::read_excel("D:/simple_template.xlsx", "Traits")


ui <- dashboardPage(skin = "yellow",
      dashboardHeader(title = "HIDAP collect"),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        shinyjs::useShinyjs(),
        tabBox(width = 12, selected = "Config", height = 500,
          tabPanel("Config",
              fluidRow(
                column(width = 4,
                       shinyFilesButton('collFile', 'File select',
                                        'Please select a file', FALSE
                       ),
                       uiOutput("uniqueId"),
                       uiOutput("primaryOrder"),
                       uiOutput("secondaryOrder")
                      ),
                column(width  = 8,

                      uiOutput("activeTraits")
                )
              )
            ),
           tabPanel("Field", height = 500,

              fluidRow(
                column(width = 6,
                       uiOutput("fieldColl")
                )
              )
                    ),
          tabPanel("Data", height = 500,
                   tableOutput("fieldData")),
          tabPanel("Help"
          ),
          tabPanel("About"
          )

        )
      )
)


server <- function(input, output, session) {


  volumes <- getVolumes(c("(E:)", "Page File (F:)"))

  shinyFileChoose(input, 'collFile', roots=volumes, session=session,
                  filetypes=c( 'xlsx'))

  collectFile <- reactive({
    req(input$collFile)
    mf = parseFilePaths(volumes, input$collFile)$datapath
    mf = as.character(mf)
    #print(mf)
    if(length(mf)==0) return("")
    mf
  })


  #fb = NULL

  fieldbook <- reactive({
    fb = readxl::read_excel(collectFile(), "Fieldbook")
    fb
  })

  # fieldbook1 <- reactive({
  #   cf = collectFile()
  #   fb = reactiveFileReader(1000, session, filePath = cf,
  #                                 readxl::read_excel, "Fieldbook")
  #   #print(str(fb))
  #   fb
  # })

  fieldbook1 <-
    reactiveFileReader(1000, session, filePath = "D:/simple_template.xlsx",
                            readxl::read_excel, "Fieldbook")


  output$fieldData <- renderTable({
    fieldbook1()
  })

  fieldtraits <- reactive({
    tr = readxl::read_excel(collectFile(), "Traits")
    #print(head(tr))
    tr
  })

  # fieldbook_edit <- reactive({
  #   req(input$trInput)
  #   id = as.integer(input$plId)
  #   tr = input$trId
  #   fb = fieldbook()
  #   #print(id)
  #   print(vl)
  #   fb[fb[[input$uqId]] == id , tr] = vl
  #   fb
  # })


  output$fieldColl <- renderUI({
    req(input$collFile)

    tagList(
    HTML("<center>"),
    div(style="display:inline-block;vertical-align:10%",
        uiOutput("uniqueIdColl")),
    div(style="display:inline-block;vertical-align:70%",
        textOutput("uqIdText") ),
    div(style="display:inline-block;vertical-align:10%",
        uiOutput("primaryOrderColl")),
    div(style="display:inline-block;vertical-align:70%",
        textOutput("prOrText")),

    HTML("<br/>"),

    div(style="display:inline-block;vertical-align:10%",
        actionButton("prevTrait", icon("backward"))),
    div(style="display:inline-block;vertical-align:-60%",
        uiOutput("traitId")),
    div(style="display:inline-block;vertical-align:10%",
        actionButton("nextTrait", icon("forward"))),
    HTML("<br/>"),

    div(style="display:inline-block;vertical-align:10%",
        actionButton("prevPlot", icon("backward"))),
    div(style="display:inline-block;vertical-align:-60%",
        uiOutput("plotId")),
    div(style="display:inline-block;vertical-align:10%",
        actionButton("nextPlot", icon("forward"))),

    wellPanel(
      HTML("</h4> Trait value:</h4>"),
      uiOutput("traitValue")
    ),

    HTML("</center>")
  )
  })

  output$uniqueId <- renderUI({
    #req(input$collFile)
    lbls = names(fieldbook())[1:6]

    #print(lbls)
    selectInput("uqId", "Unique ID", lbls)
  })

  output$uniqueIdColl <- renderUI({
    #req(input$collFile)
    lbls = names(fieldbook())[1:6]
    #print(lbls)
    selectInput("uqIdColl", "", lbls, input$uqId, width = 150)
  })

  output$uqIdText <- renderText({
    req(input$plId)
    fb = fieldbook()
    #print(input$uqIdColl)
    id = as.integer(input$plId)
    #print(id)
    out = fb[fb[[input$uqId]] == id, input$uqIdColl]
    #print(out)
    out
  })

  output$primaryOrder <- renderUI({
    #req(input$collFile)
    lbls = names(fieldbook())[1:6]
    #print(lbls)
    selectInput("prOrder", "Primary Order", lbls)
  })


  output$primaryOrderColl <- renderUI({
    #req(input$collFile)
    lbls = names(fieldbook())[1:6]
    #print(lbls)
    selectInput("prOrderColl", "", lbls, input$prOrder, width = 150)
  })

  output$prOrText <- renderText({
    req(input$plId)
    fb = fieldbook()
    id = as.integer(input$plId)
    out = fb[fb[[input$uqId]] == id, input$prOrderColl]
    out
  })


  output$secondaryOrder <- renderUI({
    #req(input$collFile)
    lbls = names(fieldbook())[1:6]
    #print(lbls)
    selectInput("scOrder", "Secondary Order", lbls)
  })

  get_trait_labels <- function(input) {
    lbls = names(fieldbook())
    lbls[!(lbls %in% c(input$prOrder, input$scOrder, "BLOCK", "REP", "ENTRY",
                              "GENOTYPE", "CIPNUMBER", "BREEDCODE", "INSTCODE") )]
  }

  output$activeTraits <- renderUI({
    lbls = get_trait_labels(input)
    checkboxGroupInput("acTraits", "activeTraits", lbls, inline = TRUE)
  })

  output$traitId <- renderUI({
    selectInput("trId", "", input$acTraits)
  })

  plLabel <- reactive({
    req(input$uqId)
    req(input$scOrder)

    # Add here the proper label based on unique Id and 2ndary order
    out = as.list(fieldbook()[[input$uqId]])
    llo = paste0(input$uqId, ": ", fieldbook()[,input$uqId])
    ll2 = paste0(" | ", input$scOrder, ": ", fieldbook()[, input$scOrder])

    names(out) = paste0(llo, ll2)
    out
  })

  output$plotId <- renderUI({
    lbls = plLabel()
    selectInput("plId", "", lbls)
  })

  observeEvent(input$prevTrait, {
    nid = length(input$acTraits)
    x = which(input$acTraits == input$trId)
    x = x - 1
    if(x <= 0) x = nid
    updateSelectInput(session, "trId", choices = input$acTraits,
                      selected = input$acTraits[x] )
  })

  observeEvent(input$nextTrait, {
    nid = length(input$acTraits)
    x = which(input$acTraits == input$trId)
    x = x + 1
    if(x >= nid) x = 1
    updateSelectInput(session, "trId", choices = input$acTraits,
                      selected = input$acTraits[x] )
  })

  observeEvent(input$prevPlot, {
    lbls = plLabel()
    nid = length(lbls)
    x = which(lbls == input$plId)
    x = x - 1
    if(x <= 0) x = nid
    updateSelectInput(session, "plId", choices = lbls,
                       selected = lbls[x] )
  })

  saveFieldbook <- reactive({
    id = as.integer(input$plId)
    tr = input$trId
    #fn = "D:/my_fieldbook.xlsx"
    fn = collectFile()
    if(file.exists(fn)){
      wb = openxlsx::loadWorkbook(fn)
      fb = openxlsx::readWorkbook(fn, sheet = "Fieldbook")
    }

    vl = input$trInput
    fb[fb[[input$uqId]] == id , tr] = vl

    #openxlsx::write.xlsx(fb, file = fn , sheetName = "Fieldbook")
    suppressWarnings({
      openxlsx::writeData(wb, "Fieldbook", fb)
      openxlsx::saveWorkbook(wb, fn, overwrite = TRUE)
    })

  })

  observeEvent(input$nextPlot, {
    lbls = plLabel()
    nid = length(lbls)
    x = which(lbls == input$plId)
    x = x + 1
    if(x >= nid) x = 1

    saveFieldbook()

    updateSelectInput(session, "plId", choices = lbls,
                      selected = lbls[x] )
  })


  output$traitValue = renderUI({
    req(input$plId)
    id = as.integer(input$plId)
    tr = input$trId
    fb = fieldbook()
    #print(id)
    #print(tr)
    vl = fb[fb[[input$uqId]] == id , tr]
    #print(vl)
    textInput("trInput", label= "", value = vl)
  })

  # to store data from trInput -> observeEvent! to update table!






}


# Shiny app with 3 fields that the user can submit data for
shinyApp(ui = ui, server = server)