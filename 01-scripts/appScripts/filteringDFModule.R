### Filtering Data Frame Module

#Creates a row for selecting paradigm type, selecting strain, and selecting DOB range. 
#The server function returns a filtered reactive df

# https://shiny.rstudio.com/articles/modules.html

filteringDFUI <- function(
  id,
  off_data = tibble(Litter_num = c(1:2, 1:2, 1:2), Cohort = 1:6)
  ){
  ns <- NS(id)
  tagList(
    fluidRow(
      # column(
      #   4,
      #   dateRangeInput(
      #     ns("DOB_range"),
      #     "Select range of birth dates",
      #     start = "2019-12-15",
      #     end = Sys.Date()
      #   )
      # ),
      column(
        4,
        selectInput(
          ns("LitterNum"),
          "Which litter number?",
          choices = unique(off_data$Litter_num), # Changed from levels to unique
          multiple = TRUE,
          selected = unique(off_data$Litter_num)
        )
      ),
      column(
        4,
        selectInput(
          ns("cohort"),
          "Which cohorts?",
          choices = unique(off_data$Cohort),
          multiple = TRUE,
          selected = unique(off_data$Cohort),
        )
      )
    )
    
  )
}


filteringDFServer <- function(
  id,
  df
  ){
  moduleServer(
    id,
    function(input, output, session) {
      
      df_react <- reactive({
        #Filter for DOB
        # df <- df %>%
        #   filter(DOB >= input$DOB_range[1] & DOB <= input$DOB_range[2])
        
        # Filter for litter number
        df <- df %>%
          filter(
            Litter_num %in% as.character(input$LitterNum)
          )
        
        # Filter for cohort
        df <- df %>%
          filter(
            Cohort %in% as.character(input$cohort)
          )
        
        return(df)
      })
      
      return(df_react) #This is a reactive df
    }
  )
}

