### Filtering Data Frame Module

#Creates a row for selecting paradigm type, selecting strain, and selecting DOB range. 
#The server function returns a filtered reactive df

# https://shiny.rstudio.com/articles/modules.html

filteringEphysUI <- function(
  id
  ){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        class = "col-xs-3",
        checkboxInput(
          ns("filterByRseries"),
          "Filter by series resistance?",
          value = FALSE
        ),
        numericInput(
          ns("RseriesMin"),
          "min Rseries (MOhm):",
          value = 0
        ),
        numericInput(
          ns("RseriesMax"),
          "max Rseries (MOhm):",
          value = 20
        )
      ),
      div(
        class = "col-xs-3",
        checkboxInput(
          ns("filterByRinput"),
          "Filter by input resistance?",
          value = FALSE
        ),
        numericInput(
          ns("RinputMin"),
          "min Rinput (MOhm):",
          value = 500
        ),
        numericInput(
          ns("RinputMax"),
          "max Rinput (MOhm):",
          value = 1500
        )
      ),
      div(
        class = "col-xs-3",
        checkboxInput(
          ns("filterByHoldingCurr"),
          "Filter by holding current?",
          value = FALSE
        ),
        numericInput(
          ns("holdingCurrMin"),
          "min current (pA):",
          value = -50
        ),
        numericInput(
          ns("holdingCurrMax"),
          "max current (pA):",
          value = 10
        )
      ),
      div(
        class = "col-xs-3",
        checkboxInput(
          ns("filterByCapacitance"),
          "Filter by capacitance?",
          value = FALSE
        ),
        numericInput(
          ns("capacitanceMin"),
          "min capacitance (pF):",
          value = 5
        ),
        numericInput(
          ns("capacitanceMax"),
          "max capacitance (pF):",
          value = 20
        )
        
      )
    ),
    tableOutput(ns("test"))
  )
}


filteringEphysServer <- function(
  id,
  df # reactive, don't include ()
  ){
  moduleServer(
    id,
    function(input, output, session) {
      
      filterByPassives <- function(df){
        if(input$filterByRseries){
          df <- df %>%
            filter(
              Rseries <= input$RseriesMax & Rseries >= input$RseriesMin
            )
        }
        if(input$filterByRinput){
          df <- df %>%
            filter(
              Rinput <= input$RinputMax & Rinput >= input$RinputMin
            )
        }
        if(input$filterByHoldingCurr){
          df <- df %>%
            filter(
              holdingCurrent <= input$holdingCurrMax & holdingCurrent >= input$holdingCurrtMin
            )
        }
        if(input$filterByCapacitance){
          df <- df %>%
            filter(
              capacitance <= input$capacitanceMax & capacitance >= input$capacitanceMin
            )
        }
        return(df)
      }
      
      df_react <- reactive({
        df <- df() %>%
          filterByPassives()
        
        return(df)
      })
      
      return(df_react) #This is a reactive df
    }
  )
}

