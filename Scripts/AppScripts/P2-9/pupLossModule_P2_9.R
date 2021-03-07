### Pup Loss App Module

#Creates a data frame that summarizes the number of pups lost during the paradigm for the different groups

# https://shiny.rstudio.com/articles/modules.html

pupLossUI <- function(
  id,
  Demo_dam
){
  ns <- NS(id)
  tagList(
    
    h3("Pup Loss During Paradigm"),
    fluidRow(
      varSelectInput(
        ns("Pup_loss_grouping_vars"),
        "Select variables to group by:",
        data = Demo_dam %>%
          select(
            Treatment:Dam_Strain,
            ParaType,
            Sac_or_stop
          ),
        selected = c(
          "Treatment",
          "Dam_Strain",
          "ParaType"
        ),
        multiple = TRUE)
    ),
    
    dataTableOutput(ns("Pup_loss_summary"))
    
  )
}


pupLossServer <- function(
  id,
  Demo_dam
){
  moduleServer(
    id,
    function(input, output, session) {
      
      output$Pup_loss_summary <- renderDataTable(
        LBN_summary_byGroup(expr(pupLoss), Demo_dam, input$Pup_loss_grouping_vars) %>%
          select(-Variable, - VarName)
      )
      
    }
  )
}

