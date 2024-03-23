### Dam Cort App Module

#Creates a dot plot comparing corticosterone levels for dams at the end of the paradigm

# https://shiny.rstudio.com/articles/modules.html

damCortUI <- function(id, Demo_dam){
  ns <- NS(id)
  tagList(
    
    h3("Dam Corticosterone on P21"),
    
    filteringDFUI(ns("Cort_dams"), Demo_dam),
    
    zoomAxisUI(ns("zoom_y"), "y"),
    
    plotOutput(ns("Dam_cort21")),
    plotOutput(ns("Dam_cort11")),
    
    h3("Summary Data"),
    
    summaryTableUI(
      id = ns("damCortSum"), 
      df_sum = Demo_dam %>%
        select(Cort_dam_P21), #data frame with possible columns for summary variables
      selected_sum = c("Cort_dam_P21"), # c(" ", " ") vector with selected variables
      df_group = Demo_dam %>%
        select(
          Treatment:Dam_Strain,
          ParaType,
          Sac_or_stop
        ),
      selected_group = c(
        "Treatment",
        "Dam_Strain"
      )
    )
    
  )
}


damCortServer <- function(id,
                          Demo_dam
){
  moduleServer(
    id,
    function(input, output, session) {
      
      #Gives the "zoom", "min", and "max" values as reactive
      #Call these with zoom_y$zoom(), for example
      zoom_y <- zoomAxisServer(
        "zoom_y",
        whichAxis = "y",
        minVal = 0,
        maxVal = 15
      )
      
      #Dam Cort Plot -------------
      output$Dam_cort21 <- renderPlot({
        Cort_dams <- Demo_dam %>%
          drop_na(Treatment, Cort_dam_P21)
        
        Cort_dams_react <- filteringDFServer(
          "Cort_dams",
          Cort_dams
        )
        
        my_puberty_dot_plot(
          df = Cort_dams_react(),
          var_to_plot = expr(Cort_dam_P21), #expr()
          phenotype_name = NULL,
          shape = expr(Dam_Strain),
          colour = expr(Dam_Strain),
          width = 0.3,
          change_ymax = zoom_y$zoom(),
          ymin = zoom_y$min(),
          ymax = zoom_y$max(),
          alt_ytitle = TRUE,
          ytitle = "Corticosterone (ng/mL)" #alternative y title
        )
      })
      output$Dam_cort11 <- renderPlot({
        Cort_dams <- Demo_dam %>%
          drop_na(Treatment, Cort_dam_P11)
        
        Cort_dams_react <- filteringDFServer(
          "Cort_dams",
          Cort_dams
        )
        
        my_puberty_dot_plot(
          df = Cort_dams_react(),
          var_to_plot = expr(Cort_dam_P11), #expr()
          phenotype_name = NULL,
          shape = expr(Dam_Strain),
          colour = expr(Treatment),
          width = 0.3,
          change_ymax = zoom_y$zoom(),
          ymin = zoom_y$min(),
          ymax = zoom_y$max(),
          alt_ytitle = TRUE,
          ytitle = "Corticosterone (ng/mL)" #alternative y title
        )+ 
          stat_compare_means(method = "t.test", label.y = max(Cort_dams_react()$Cort_dam_P11, na.rm = TRUE) + 10)
      })
      
      damCortSum <- summaryTableServer("damCortSum", reactive(Demo_dam))
      
    }
  )
}

