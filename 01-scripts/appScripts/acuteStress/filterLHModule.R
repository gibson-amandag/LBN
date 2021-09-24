### Filter by LH

# https://shiny.rstudio.com/articles/modules.html

filterLHUI <- function(
  id
){
  ns <- NS(id)
  tagList(
    uiOutput(ns("filterUI")),
    uiOutput(ns("surgeMinUI"))
  )
}


filterLHServer <- function(
  id,
  minVal,
  display = FALSE
){
  moduleServer(
    id,
    function(input, output, session) {
      output$filterUI <- renderUI({
        validate(
          need(display, FALSE)
        )
        ns <- session$ns
        checkboxInput(
          ns("filterLH"),
          "Remove non-surging mice:",
          value = FALSE
        )
      })
      
      output$surgeMinUI <- renderUI({
        validate(
          need(display, FALSE)
        )
        ns <- session$ns
        numericInput(
          ns("surgeMin"),
          "Min LH value for surge:",
          value = minVal
        )
        
      })
      
      #Return these values as a list to be able to use them in other modules
      # ...$zoom()
      return(
        list(
          useFilter = reactive({input$filterLH}),
          surgeMin = reactive({input$surgeMin})
        )
      )
    }
  )
}

