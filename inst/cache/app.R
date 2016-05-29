library("shiny")
library("shinyjs")
library("shinydashboard")
library("readxl")
library(shinyBS)
library(shinyFiles)
library(DT)



ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "HIDAP collect"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      shinyjs::useShinyjs(),
                      tabBox(width = 12, selected = "Config", height = 500,
                             tabPanel("Config",

                                uiOutput("cache_test")

                             )
                      )
                    )
)



server <- function(input, output, session) {

  output$cache_test <- renderText({
    "Hello, world!"
  })

}

shinyApp(ui = ui, server = server)


