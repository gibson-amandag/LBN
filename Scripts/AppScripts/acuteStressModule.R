### Acute Stress App Module

# https://shiny.rstudio.com/articles/modules.html

acuteStressUI <- function(id,
                          AcuteStress_off){
  ns <- NS(id)
  tagList(
    
    h3("Acute, Layered, Psychosocial Stress Paradigm"),
    
    filteringDFUI(ns("ALPS_filter")),
    
    fluidRow(
      column(
        4,
        radioButtons(
          ns("WhichSex"),
          "Which sex?",
          c("Both", "Male" = "M", "Female" = "F"),
          selected = "Both"
        )
      ),
      column(
        4,
        #plot individual lines?
        checkboxInput(
          ns("Individual_lines"),
          "Plot individual lines?",
          value = TRUE
        ),
        #plot means?
        checkboxInput(
          ns("Mean_lines"),
          "Plot mean lines?",
          value = TRUE
        ),
        #plot by dam strain?
        checkboxInput(
          ns("By_strain"),
          "Plot by strain?",
          value = TRUE
        )
        
      ),
      column(
        4,
        #Add a title
        textInput(
          ns("Title"),
          "Graph Title:"
        ),
        #plot by dam?
        checkboxInput(
          ns("By_dam"),
          "Plot by litter?",
          value = FALSE
        )
      )
    ),
    
    zoomAxisUI(ns("zoom_x"), "x"),
    
    zoomAxisUI(ns("zoom_y"), "y"),
    
    #plot dam mass
    plotOutput(ns("Plot"), height = "600px"),
    
    h3("Summary Table"),
    
    summaryTableUI(
      id = ns("ALPSSum"), 
      df_sum = AcuteStress_off %>%
        select(Cort_pre:LH_5.5), #data frame with possible columns
      selected_sum = c("Cort_pre", "Cort_post"), # c(" ", " ") vector with selected variables
      df_group = AcuteStress_off %>%
        select(
          Sex:Treatment,
          Dam_ID,
          Dam_Strain:ParaType
        ),
      selected_group = c(
        "Treatment",
        "Dam_Strain"
      )
    )
    
  )
}


acuteStressServer <- function(
  id,
  AcuteStress_off,
  Demo_dam
){
  moduleServer(
    id,
    function(input, output, session) {
      
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      
      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 15)
      
      output$Plot <- renderPlot({
        #needs to be before the averaging by litter
        if(input$WhichSex == "M"){
          AcuteStress_off <- AcuteStress_off %>%
            filter(Sex == "M")
        }else if(input$WhichSex == "F"){
          AcuteStress_off <- AcuteStress_off %>%
            filter(Sex == "F")
        }

        if(input$By_dam == FALSE){
         AcuteStress_off_long <- makeCortLong(AcuteStress_off)
        }

        if(input$By_dam == TRUE){
          AcuteStress_off_long <- AcuteStress_off %>%
            getAvgByDam(Demo_dam) %>%
            makeCortLong()
        }

        AcuteStress_off_long_react <- filteringDFServer("ALPS_filter", AcuteStress_off_long)

        stress_interaction_plot(AcuteStress_off_long_react(), Cort, "Cort (ng/mL)", plotMean = input$Mean_lines)
      })
      
      ALPSSum <- summaryTableServer("ALPSSum", reactive(AcuteStress_off))
      
      
    }
  )
}

