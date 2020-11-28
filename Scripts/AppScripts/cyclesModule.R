### Cycles App Module

# https://shiny.rstudio.com/articles/modules.html

cyclesUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Offspring Cycles"),
    
    filteringDFUI(ns("cycles_filter")),
    
    h4("Control offspring"),
    plotOutput(ns("control_plot"),
               height = "600px"),
    
    h4("LBN offspring"),
    plotOutput(ns("LBN_plot"),
               height = "600px")
    
  )
}


cyclesServer <- function(id,
                         Cycles_off
                         ){
  moduleServer(
    id,
    function(input, output, session) {
      
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      
      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 15)
      
      cycles_react <- filteringDFServer("cycles_filter", Cycles_off)
      
      Cycles_off_long <- reactive({
        Cycles_off_long <- make_cycles_long(cycles_react()) %>%
          add_Day_col() %>%
          drop_na(Stage)
        return(Cycles_off_long)
      })
      
      output$control_plot <- renderPlot({
        Cycles_off_long() %>%
          filter(Treatment == "Control") %>%
          cyclesPlotFunc()
      })
      
      output$LBN_plot <- renderPlot({
        Cycles_off_long() %>%
          filter(Treatment == "LBN") %>%
          cyclesPlotFunc()
      })

      
    }
  )
}

