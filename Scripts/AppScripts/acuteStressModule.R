### Acute Stress App Module

# https://shiny.rstudio.com/articles/modules.html

acuteStressUI <- function(id,
                          AcuteStress_off){
  ns <- NS(id)
  tagList(
    
    h3("Acute, Layered, Psychosocial Stress Paradigm"),
    
    filteringDFUI(ns("ALPS_filter"), AcuteStress_off),
    
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
        #plot by dam? - can't diff treatments
        # checkboxInput(
        #   ns("By_dam"),
        #   "Plot by litter?",
        #   value = FALSE
        # )
      )
    ),
    
    zoomAxisUI(ns("zoom_x"), "x"),
    
    zoomAxisUI(ns("zoom_y"), "y"),
    
    #plot dam mass
    plotOutput(ns("Plot"), height = "600px"),
    
    h3("ANOVA"),
    
    tableOutput(ns("anova")),
    
    h4("Interaction of ALPS Treatment and Time"),
    
    p("This explores the two-way ANOVA at each level of early-life treatment"),
    
    tableOutput(ns("twoway")),
    
    h4("Effect of time"),
    
    p("This further explores the effect of time for each treatment group. p-value for significance needs to be adjusted for multiple comparison by Bonferroni"),
    
    tableOutput(ns("timeEffect")),
    
    h4("Pairwise Comparisons"),
    
    tableOutput(ns("pairwise")),
    
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
          Dam_Strain:ParaType,
          Stress_treatment
        ),
      selected_group = c(
        "Treatment",
        "Stress_treatment"
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
      
      AcuteStress_off_react <- reactive({
        #needs to be before the averaging by litter
        if(input$WhichSex == "M"){
          AcuteStress_off <- AcuteStress_off %>%
            filter(Sex == "M")
        }else if(input$WhichSex == "F"){
          AcuteStress_off <- AcuteStress_off %>%
            filter(Sex == "F")
        }
        
        AcuteStress_off  <- AcuteStress_off %>%
          filter(!is.na(Cort_pre) & !is.na(Cort_post))
        
        AcuteStress_off_react <- filteringDFServer("ALPS_filter", AcuteStress_off)
        return(AcuteStress_off_react())
      })
      
      AcuteStress_off_long_react <- reactive({
        AcuteStress_off_long <- makeCortLong(AcuteStress_off_react())
        
        AcuteStress_off_long %>%
          convert_as_factor(
            Mouse_ID, Time, Treatment, Stress_treatment
          )
        
        AcuteStress_off_long$Time <- factor(
          AcuteStress_off_long$Time,
          levels = c("Cort_pre","Cort_post"),
          labels = c("pre", "post")
        )
        
        return(AcuteStress_off_long)
      })
      
      output$Plot <- renderPlot({
        stress_interaction_plot(AcuteStress_off_long_react(), Cort, "Cort (ng/mL)", plotMean = input$Mean_lines)
      })
      
      ALPSSum <- summaryTableServer("ALPSSum", AcuteStress_off_react)
      
      output$anova <- renderTable({
        res.aov <- AcuteStress_off_long_react() %>%
          anova_test(
            dv = Cort,
            wid = Mouse_ID,
            within = Time, 
            between = c(Treatment, Stress_treatment),
            type = 3
          )
        
        return(get_anova_table(res.aov))
      })
      
      output$twoway <- renderTable({
        two.way <- AcuteStress_off_long_react() %>%
          group_by(
            Treatment
          ) %>%
          anova_test(
            dv = Cort,
            wid = Mouse_ID,
            between = Stress_treatment,
            within = Time
          ) %>%
          get_anova_table() %>%
          adjust_pvalue(
            method = "bonferroni"
          )%>%
          add_significance("p.adj")
        
        colnames(two.way)[1] <- "Early.Life"
        
        return(two.way)
      })
      
      output$timeEffect <- renderTable({
        time.effect <- AcuteStress_off_long_react() %>%
          group_by(
            Treatment,
            Stress_treatment
          ) %>%
          anova_test(
            dv = Cort,
            wid = Mouse_ID,
            within = Time 
          ) %>%
          get_anova_table() %>%
          adjust_pvalue(
            method = "bonferroni"
          )%>%
          add_significance("p.adj")
          
        
        colnames(time.effect)[1] <- "Early.Life"
        colnames(time.effect)[2] <- "Adult.Treatment"
        
        return(time.effect)
      })
      
      output$pairwise <- renderTable({
        pwc <- AcuteStress_off_long_react() %>%
          group_by(
            Treatment,
            Stress_treatment
          ) %>%
          pairwise_t_test(
            Cort ~ Time,
            paired = TRUE,
            p.adjust.method = "bonferroni"
          ) %>%
          select(
            -statistic,
            -df,
            -p
          )
        
        colnames(pwc)[1] <- "Early.Life"
        colnames(pwc)[2] <- "Adult.Treatment"
        
        pwc
      })
      
    }
  )
}

