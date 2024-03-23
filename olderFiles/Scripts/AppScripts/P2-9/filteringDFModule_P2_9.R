### Filtering Data Frame Module

#Creates a row for selecting paradigm type, selecting strain, and selecting DOB range. 
#The server function returns a filtered reactive df

# https://shiny.rstudio.com/articles/modules.html

filteringDF_P2_9_UI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        radioButtons(
          ns("WhichStrain"),
          "Which dam strains?",
          c("Both", "B6", "CBA")
        )
      ),
      column(
        4,
        dateRangeInput(
          ns("DOB_range"),
          "Select range of birth dates",
          start = "2019-12-15",
          end = Sys.Date()
        )
      )
    )
    
  )
}


filteringDF_P2_9_Server <- function(id,
                              df){
  moduleServer(
    id,
    function(input, output, session) {
      
      df_react <- reactive({
        df
        
        #Filter for DOB
        df <- df %>%
          filter(DOB >= input$DOB_range[1] & DOB <= input$DOB_range[2])
        
        #Filter for Strain - By Dam Strain
        if(input$WhichStrain == "B6"){
          df <- df %>%
            filter(Dam_Strain == "B6")
        }else if(input$WhichStrain == "CBA"){
          df <- df %>%
            filter(Dam_Strain == "CBA")
        }
        return(df)
      })
      
      # return(df_react) #This is a reactive df
    }
  )
}

