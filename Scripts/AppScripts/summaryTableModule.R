### Summary Table Module

#Creates a row for selecting variables to summarize and grouping variables.
#Need to provide the df with selected columns for both
#Need to provide the initially selected variables for both
#These values (vars_to_sum, grouping_vars, and summary_df) are output in a list as reactive values by the server

# https://shiny.rstudio.com/articles/modules.html

summaryTableUI <- function(id, 
                       df_sum, #data frame with possible columns for summary variables
                       selected_sum, # c(" ", " ") vector with selected variables
                       df_group, #grouping variables
                       selected_group
                       ){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             varSelectInput(ns("vars_to_sum"),
                            "Select variables to summarize",
                            data = df_sum,
                            selected = selected_sum,
                            multiple = TRUE) #this always has to be true to feed into map_dfr appropriately
      ),
      column(4,
             varSelectInput(ns("grouping_vars"),
                            "Select variables to group by",
                            data = df_group,
                            selected = selected_group,
                            multiple = TRUE))
    ),
    dataTableOutput(ns("summary_df"))
    
  )
}


summaryTableServer <- function(id,
                               df){
  moduleServer(
    id,
    function(input, output, session) {
    
      output$summary_df <- renderDataTable({
        map_dfr(input$vars_to_sum, LBN_summary_byGroup, df, input$grouping_vars)
      })

        

      
      #Return these values as a list to be able to use them in other modules
      # ...$vars_to_sum()
      return(
        list(
          vars_to_sum = reactive({ input$vars_to_sum }),
          grouping_vars = reactive({ input$grouping_vars }),
          df_sum = reactive({ output$summary_df })
        )
      )
    }
  )
}

