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




ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "HIDAP collect"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(fbcollect::tab_dataSource())
)

#############################

server <- function(input, output, session) {
  values = shiny::reactiveValues()
  fbcollect::srv_dataSource(input, output, session, values = values)
}

shinyApp(ui = ui, server = server)
