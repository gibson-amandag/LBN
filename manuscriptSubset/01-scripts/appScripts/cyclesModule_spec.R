### Cycles App Module

#creates a plot for the control and LBN offspring cycle plots
#Each animal is plotted individually
#Ability to filter by paraType, dam strain, and DOB

# https://shiny.rstudio.com/articles/modules.html

cycles_specUI <- function(id){
  ns <- NS(id)
  tagList(
    #header
    h3("Offspring Cycles"),
    
    #filtering controls
    filteringDFUI(ns("cycles_filter")),
    
    h4("Control offspring"),
    plotOutput(
      ns("control_plot"),
      height = "600px"
    ),
    
    h4("LBN offspring"),
    plotOutput(
      ns("LBN_plot"),
      height = "600px"
    )
    
  )
}


cycles_specServer <- function(
  id,
  Cycles_off
){
  moduleServer(
    id,
    function(input, output, session) {
      #Reactive cyles data frame that has been filtered. Call with cycles_react(), 
      #unless input to summaryTables, or other module. Then just cycles_react. 
      cycles_react <- filteringDFServer("cycles_filter", Cycles_off)
      
      # #Make long-form for plotting - reactive
      # Cycles_off_long <- reactive({
      #   Cycles_off_long <- make_cycles_long(cycles_react()) %>%
      #     add_Day_col() %>%
      #     drop_na(Stage)
      #   return(Cycles_off_long)
      # })
      # 
      # #plot controls
      # output$control_plot <- renderPlot({
      #   Cycles_off_long() %>%
      #     filter(Treatment == "Control") %>%
      #     cyclesPlotFunc()
      # })
      # 
      # #plot LBN
      # output$LBN_plot <- renderPlot({
      #   Cycles_off_long() %>%
      #     filter(Treatment == "LBN") %>%
      #     cyclesPlotFunc()
      # })
      
      
    }
  )
}

