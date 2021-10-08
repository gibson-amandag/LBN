### Corticosterone Plate Analysis Module

# https://shiny.rstudio.com/articles/modules.html


uploadCortEIAUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        class = "col-xs-6",
        fileInput(
          ns("dataFile"),
          label = "Select CSV File",
          accept = ".csv"
        )
      ),
      div(
        class = "col-xs-6",
        a("Example CSV File", href = "cortEIAFileExample.xlsx")
      )
    ),
    cortEIAUI(ns("cortEIA"))
  )
}


uploadCortEIAServer <- function(
  id,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        req(input$dataFile)
        cortEIAServer(
          "cortEIA",
          input$dataFile$datapath,
          compType
        )
      })
      
    }
  )
}