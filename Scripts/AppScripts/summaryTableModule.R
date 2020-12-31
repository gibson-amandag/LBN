### SUMMARY TABLE MODULE #############################

#summaryTableUI -> select variables to summarize and variables to group by
#Provides table output

#summaryTableServer -> LBN_summary_byGroup function to make data frame giving mean, sd, n, and SEM 
#render table
#Provides list with vars_to_sum, grouping_vars, and summary data frame (df_sum)
#provide a reactive df to this. Don't use (). these are called here

# https://shiny.rstudio.com/articles/modules.html

summaryTableUI <- function(
  id,
  df_sum, #df with columns selected for variable to summarize
  selected_sum, # c("") selected variables to summarize
  df_group, #df with columns selected for grouping variables
  selected_group #c("") selected grouping variable
){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        varSelectInput(
          ns("vars_to_sum"),
          "Select variables to summarize",
          data = df_sum,
          selected = selected_sum,
          multiple = TRUE #this always has to be true to feed into map_dfr appropriately
        ) 
      ),
      column(
        4,
        varSelectInput(
          ns("grouping_vars"),
          "Select variables to group by",
          data = df_group,
          selected = selected_group,
          multiple = TRUE
        )
      )
    ),
    
    dataTableOutput(ns("summaryTable"))
    
  )
}


summaryTableServer <- function(
  id,
  df
){
  moduleServer(
    id,
    function(input, output, session) {
      
      df_sum <- reactive({
        map_dfr(input$vars_to_sum, LBN_summary_byGroup, df(), input$grouping_vars)
      })
      
      output$summaryTable <- renderDataTable(df_sum())
      
      #Return these values as a list to be able to use them in other modules
      # ...$vars_to_sum()
      return(
        list(
          vars_to_sum = reactive({ input$vars_to_sum }),
          grouping_vars = reactive({ input$grouping_vars }),
          df_sum = df_sum
        )
      )
    }
  )
}

