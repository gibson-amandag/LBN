### Offspring Maturation App Module

# https://shiny.rstudio.com/articles/modules.html

maturationOff_P2_9_UI <- function(
  id,
  Maturation_off
){
  ns <- NS(id)
  tagList(
    
    h3("Offspring Maturation"),
    
    filteringDF_P2_9_UI(ns("MaturationOff_filter")),
    
    tabsetPanel(
      tabPanel(
        "Plots",
        
        #Culumative Frequency Plots
        h4("Cumulative Frequency Plots"),
        p("Zooming x-axis will force the same for all three plots"),
        
        zoomAxisUI(
          ns("cumFreq_zoomX"),
          whichAxis = "x",
          startOn = TRUE
        ),
        
        #The Plots Row
        fluidRow(
          column(
            4,
            plotOutput(ns("VO_cumFreq"))
          ),
          column(
            4,
            plotOutput(ns("FirstE_cumFreq"))
          ),
          column(
            4,
            plotOutput(ns("PPS_cumFreq"))
          )
        ),
        
        #Dot Plots
        h4("Puberty Dot Plots - Day"),
        p("Zooming y-axis will force the same for all three plots"),
        
        zoomAxisUI(
          ns("dotDay_zoomY"),
          whichAxis = "y",
          startOn = TRUE
        ),
        
        #The Plots Row
        fluidRow(
          column(
            4,
            plotOutput(ns("VO_dot"))
          ),
          column(
            4,
            plotOutput(ns("FirstE_dot"))
          ),
          column(
            4,
            plotOutput(ns("PPS_dot"))
          )
        ),
        
        #Dot Plots - Mass
        h4("Puberty Dot Plots - Mass"),
        p("Zooming y-axis will force the same for all three plots"),
        
        zoomAxisUI(
          ns("dotMass_zoomY"),
          whichAxis = "y",
          startOn = TRUE
        ),
        
        #The Plots Row
        fluidRow(
          column(
            4,
            plotOutput(ns("VO_dot_mass"))
          ),
          column(
            4,
            plotOutput(ns("FirstE_dot_mass"))
          ),
          column(
            4,
            plotOutput(ns("PPS_dot_mass"))
          )
        )
      ), #end plots panel
      
      tabPanel(
        "AGD Summary",
        h4("Ano-genital distance"),
        
        summaryTableUI(
          id = ns("AGDSum"), 
          df_sum = Maturation_off %>%
            select(AGD_wean:AGD_adult_by_mass, AGD_P22:AGD_P72), #data frame with possible columns
          selected_sum = c(
            "AGD_wean",
            "AGD_adult",
            "Mass_P9",
            "Mass_P11"
          ), # c(" ", " ") vector with selected variables
          df_group = Maturation_off %>%
            select(
              Sex:Treatment,
              Dam_ID,
              Dam_Strain:ParaType
            ),
          selected_group = c(
            "Sex",
            "Treatment"
          )
        )
      ), #end AGD summary
      
      tabPanel(
        "VO Summary",
        h4("Vaginal Opening"),
        
        summaryTableUI(
          id = ns("VOSum"), 
          df_sum = Maturation_off %>%
            select(VO_age, VO_mass), #data frame with possible columns
          selected_sum = c("VO_age", "VO_mass"), # c(" ", " ") vector with selected variables
          df_group = Maturation_off %>%
            select(
              Treatment,
              Dam_ID,
              Dam_Strain:ParaType
            ),
          selected_group = c("Treatment")
        ),
        
        verbatimTextOutput(ns("VO_tTest"))
        
      ), #End VO summary
      
      tabPanel(
        "Estrus Summary",
        h4("First Estrus"),
        
        summaryTableUI(
          id = ns("EstrusSum"), 
          df_sum = Maturation_off %>%
            select(Estrus_age, Estrus_mass), #data frame with possible columns
          selected_sum = c("Estrus_age", "Estrus_mass"), # c(" ", " ") vector with selected variables
          df_group = Maturation_off %>%
            select(
              Treatment,
              Dam_ID,
              Dam_Strain:ParaType
            ),
          selected_group = c("Treatment")
        ),
        
        verbatimTextOutput(ns("Estrus_tTest"))
        
      ), #End estrus summary
      
      tabPanel(
        "PPS Summary",
        h4("Preputial Separation"),
        
        summaryTableUI(
          id = ns("PPSSum"), 
          df_sum = Maturation_off %>%
            select(PreputialSep_age, PreputialSep_mass), #data frame with possible columns
          selected_sum = c("PreputialSep_age", "PreputialSep_mass"), # c(" ", " ") vector with selected variables
          df_group = Maturation_off %>%
            select(
              Treatment,
              Dam_ID,
              Dam_Strain:ParaType
            ),
          selected_group = c("Treatment")
        ),
        
        verbatimTextOutput(ns("PPS_tTest"))
        
      ) #End PPS summary
      
    ) #end tabsetPanel
    
  )
}


maturationOff_P2_9_Server <- function(
  id,
  Maturation_off
){
  moduleServer(
    id,
    function(input, output, session) {
      
      MaturationOff_react <- filteringDF_P2_9_Server("MaturationOff_filter", Maturation_off)
      
      ### Cumulative Frequency Plots --------
      cumFreq_zoomX <- zoomAxisServer("cumFreq_zoomX", "x", minVal = 21, maxVal = 50)
      
      output$VO_cumFreq <- renderPlot({
        my_cumulative_freq_plot(
          df = MaturationOff_react(),
          color_var = expr(Treatment),
          linetype_var = expr(Dam_Strain),
          var_to_plot = expr(VO_age), #as expr()
          phenotype_name = "VO", #string
          title = TRUE,
          change_xmax = cumFreq_zoomX$zoom(),
          xmax = cumFreq_zoomX$max(),
          xmin = cumFreq_zoomX$min()
        )
      })
      
      output$FirstE_cumFreq <- renderPlot({
        my_cumulative_freq_plot(
          df = MaturationOff_react(),
          color_var = expr(Treatment),
          linetype_var = expr(Dam_Strain),
          var_to_plot = expr(Estrus_age), #as expr()
          phenotype_name = "First Estrus", #string
          title = TRUE,
          change_xmax = cumFreq_zoomX$zoom(),
          xmax = cumFreq_zoomX$max(),
          xmin = cumFreq_zoomX$min()
        )
      })
      
      output$PPS_cumFreq <- renderPlot({
        my_cumulative_freq_plot(
          df = MaturationOff_react(),
          color_var = expr(Treatment),
          linetype_var = expr(Dam_Strain),
          var_to_plot = expr(PreputialSep_age), #as expr()
          phenotype_name = "PPS", #string
          title = TRUE,
          change_xmax = cumFreq_zoomX$zoom(),
          xmax = cumFreq_zoomX$max(),
          xmin = cumFreq_zoomX$min()
        )
      })
      
      ### Dot Plots Day -------- 
      dotDay_zoomY <- zoomAxisServer("dotDay_zoomY", "Y", minVal = 0, maxVal = 50)
      
      output$VO_dot <- renderPlot({
        my_puberty_dot_plot(
          df = MaturationOff_react(),
          expr(VO_age), #expr()
          phenotype_name = "VO",
          shape = expr(Dam_Strain),
          colour = expr(Dam_Strain),
          width = 0.3,
          change_ymax = dotDay_zoomY$zoom(),
          ymin = dotDay_zoomY$min(),
          ymax = dotDay_zoomY$max(),
          DaysOrMass = "Days"
        )
      })
      
      output$FirstE_dot <- renderPlot({
        my_puberty_dot_plot(
          df = MaturationOff_react(),
          expr(Estrus_age), #expr()
          phenotype_name = "First Estrus",
          shape = expr(Dam_Strain),
          colour = expr(Dam_Strain),
          width = 0.3,
          change_ymax = dotDay_zoomY$zoom(),
          ymin = dotDay_zoomY$min(),
          ymax = dotDay_zoomY$max(),
          DaysOrMass = "Days"
        )
      })
      
      output$PPS_dot <- renderPlot({
        my_puberty_dot_plot(
          df = MaturationOff_react(),
          expr(PreputialSep_age), #expr()
          phenotype_name = "PPS",
          shape = expr(Dam_Strain),
          colour = expr(Dam_Strain),
          width = 0.3,
          change_ymax = dotDay_zoomY$zoom(),
          ymin = dotDay_zoomY$min(),
          ymax = dotDay_zoomY$max(),
          DaysOrMass = "Days"
        )
      })
      
      ### Dot Plots Mass --------
      dotMass_zoomY <- zoomAxisServer("dotMass_zoomY", "Y", minVal = 0, maxVal = 25)
      
      output$VO_dot_mass <- renderPlot({
        my_puberty_dot_plot(
          df = MaturationOff_react(),
          expr(VO_mass), #expr()
          phenotype_name = "VO",
          shape = expr(Dam_Strain),
          colour = expr(Dam_Strain),
          width = 0.3,
          change_ymax = dotMass_zoomY$zoom(),
          ymin = dotMass_zoomY$min(),
          ymax = dotMass_zoomY$max(),
          DaysOrMass = "Mass"
        )
      })
      
      output$FirstE_dot_mass <- renderPlot({
        my_puberty_dot_plot(
          df = MaturationOff_react(),
          expr(Estrus_mass), #expr()
          phenotype_name = "First Estrus",
          shape = expr(Dam_Strain),
          colour = expr(Dam_Strain),
          width = 0.3,
          change_ymax = dotMass_zoomY$zoom(),
          ymin = dotMass_zoomY$min(),
          ymax = dotMass_zoomY$max(),
          DaysOrMass = "Mass"
        )
      })
      
      output$PPS_dot_mass <- renderPlot({
        my_puberty_dot_plot(
          df = MaturationOff_react(),
          expr(PreputialSep_mass), #expr()
          phenotype_name = "PPS",
          shape = expr(Dam_Strain),
          colour = expr(Dam_Strain),
          width = 0.3,
          change_ymax = dotMass_zoomY$zoom(),
          ymin = dotMass_zoomY$min(),
          ymax = dotMass_zoomY$max(),
          DaysOrMass = "Mass"
        )
      })
      
      ### AGD Summary -----
      
      AGDSum <- summaryTableServer("AGDSum", MaturationOff_react) #doesn't seem to be changing
      
      
      ### VO Summary -----
      VOSum <- summaryTableServer("VOSum", MaturationOff_react)
      
      ### Estrus Summary -----
      EstrusSum <- summaryTableServer("EstrusSum", MaturationOff_react)
      
      ### PPS Summary -----
      PPSSum <- summaryTableServer("PPSSum", MaturationOff_react)
      
      ### t-tests ------
      output$VO_tTest <- renderPrint({
        t.test(VO_age ~ Treatment, MaturationOff_react())
      })
      
      output$Estrus_tTest <- renderPrint({
        t.test(Estrus_age ~ Treatment, MaturationOff_react()) 
      })
      
      output$PPS_tTest <- renderPrint({
        t.test(PreputialSep_age ~ Treatment, MaturationOff_react())
      })
      
    }
  )
}

