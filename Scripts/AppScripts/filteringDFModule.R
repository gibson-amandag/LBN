### Filtering Data Frame Module

#Creates a row for selecting paradigm type, selecting strain, and selecting DOB range. 
#The server function returns a filtered reactive df

# https://shiny.rstudio.com/articles/modules.html

filteringDFUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # column(
      #   4,
      #   radioButtons(
      #     ns("ParaTypes"),
      #     "Which paradigm type?",
      #     c("Both", "P2-P9" = 2, "P4-P11" = 4),
      #     selected = "Both"
      #   )
      # ),
      # column(
      #   4,
      #   radioButtons(
      #     ns("WhichStrain"),
      #     "Which dam strains?",
      #     c("Both", "B6", "CBA")
      #   )
      # ),
      column(
        4,
        dateRangeInput(
          ns("DOB_range"),
          "Select range of birth dates",
          start = "2019-12-15",
          end = Sys.Date()
        )
      ),
      column(
        4,
        radioButtons(
          ns("LitterNum"),
          "Which litter number?",
          c("Both", "First" = 1, "Second" = 2),
          selected = "Both"
        )
      )
    )
    
  )
}


filteringDFServer <- function(id,
                              df){
  moduleServer(
    id,
    function(input, output, session) {
      
      df_react <- reactive({
        df
        # #Filter for paradigm type
        # if(input$ParaTypes == 2){
        #   df <- df %>%
        #     filter(ParaType == 2)
        # }else if(input$ParaTypes == 4){
        #   df <- df %>%
        #     filter(ParaType == 4)
        # }
        
        #Filter for DOB
        df <- df %>%
          filter(DOB >= input$DOB_range[1] & DOB <= input$DOB_range[2])
        
        # #Filter for Strain - By Dam Strain
        # if(input$WhichStrain == "B6"){
        #   df <- df %>%
        #     filter(Dam_Strain == "B6")
        # }else if(input$WhichStrain == "CBA"){
        #   df <- df %>%
        #     filter(Dam_Strain == "CBA")
        # }
        
        #Filter for Litter Number
        if(input$LitterNum == 1){
          df <- df %>%
            filter(Litter_num == 1)
        } else if(input$LitterNum == 2){
          df <- df %>%
            filter(Litter_num == 2)
        }
        return(df)
      })
      
      # return(df_react) #This is a reactive df
    }
  )
}

