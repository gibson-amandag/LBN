### Dam Cort App Module

#Creates a dot plot comparing corticosterone levels for dams at the end of the paradigm

# https://shiny.rstudio.com/articles/modules.html

damCortUI <- function(id){
  ns <- NS(id)
  tagList(
    
    h3("Dam Corticosterone on P21"),
    
    fluidRow(
      column(4,
             radioButtons(ns("Cort_dams_ParaTypes"),
                          "Which paradigm type?",
                          c("Both", "P2-P9" = 2, "P4-P11" = 4),
                          selected = "Both")
      ),
      column(4,
             radioButtons(ns("Cort_dams_whichStrain"),
                          "Which dam strains?",
                          c("Both", "B6", "CBA"))
      ),
      column(4,
             dateRangeInput(ns("Cort_dams_DOBs"),
                            "Select range of birth dates",
                            start = "2019-12-15",
                            end = Sys.Date())
      )
    ),
    
    zoomAxisUI(ns("zoom_y"), "y"),
    
    plotOutput(ns("Dam_cort21"))
    
  )
}


damCortServer <- function(id,
                          Demo_dam
                          ){
  moduleServer(
    id,
    function(input, output, session) {
    
      zoomAxisServer("zoom_y",
                     whichAxis = "y",
                     minVal = 0,
                     maxVal = 15)
      
      #Dam Cort Plot
      output$Dam_cort21 <- renderPlot({
        Cort_dams <- Demo_dam %>%
          drop_na(Treatment, Cort_dam_P21)
        
        #Filter for paradigm type
        if(input$Cort_dams_ParaTypes == 2){
          Cort_dams <- Cort_dams %>%
            filter(ParaType == 2)
        }else if(input$Cort_dams_ParaTypes == 4){
          Cort_dams <- Cort_dams %>%
            filter(ParaType == 4)
        }
        
        #Filter for DOB
        Cort_dams <- Cort_dams %>%
          filter(DOB >= input$Cort_dams_DOBs[1] & DOB <= input$Cort_dams_DOBs[2])
        
        #Filter for Strain - By Dam Strain
        if(input$Cort_dams_whichStrain == "B6"){
          Cort_dams <- Cort_dams %>%
            filter(Dam_Strain == "B6")
        }else if(input$Cort_dams_whichStrain == "CBA"){
          Cort_dams <- Cort_dams %>%
            filter(Dam_Strain == "CBA")
        }
        
        my_puberty_dot_plot(
          df = Cort_dams,
          var_to_plot = expr(Cort_dam_P21), #expr()
          phenotype_name = NULL,
          shape = expr(Dam_Strain),
          colour = expr(Dam_Strain),
          width = 0.3,
          change_ymax = FALSE,
          ymax = NA,
          alt_ytitle = TRUE,
          ytitle = "Corticosterone (ng/mL)" #alternative y title
        )
      })
      
    }
  )
}

