### Zoom Axis Module

#Creates a row for selecting whether to zoom an axis. 
#The lower and upper limit boxes only show up when checked

# https://shiny.rstudio.com/articles/modules.html

zoomAxisUI <- function(id, 
                       whichAxis #"x" or "y"
                       ){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             checkboxInput(ns("zoom"),
                           paste0("Zoom ", whichAxis, " axis?")
             )
             ),
      column(4,
             uiOutput(ns("min"))
             ),
      column(4,
             uiOutput(ns("max"))
             
      )
    )
    
  )
}


zoomAxisServer <- function(id,
                           whichAxis,
                           minVal,
                           maxVal){
  moduleServer(
    id,
    function(input, output, session) {
    
      output$min <- renderUI({
        validate(
          need(input$zoom == TRUE, FALSE)
        )
        ns <- session$ns
        numericInput(ns("min"),
                     paste0("Lower Limit ", whichAxis, "-axis:"),
                     value = minVal)
        
      })
      
      output$max <- renderUI({
        validate(
          need(input$zoom == TRUE, FALSE)
        )
        ns <- session$ns
        numericInput(ns("max"),
                     paste0("Upper Limit ", whichAxis, "-axis:"),
                            value = maxVal)
        
      })
      
      
    }
  )
}

