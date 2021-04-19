### Offspring Maturation App Module

# https://shiny.rstudio.com/articles/modules.html

maturationOffUI <- function(
  id,
  Maturation_off,
  Maturation_litter1
){
  ns <- NS(id)
  tagList(
    
    h3("Offspring Maturation"),
    
    filteringDFUI(ns("MaturationOff_filter")),
    
    # checkboxInput(
    #   ns("undisturbedFirstLitter"),
    #   label = "Plot undisturbed first litters",
    #   value = FALSE
    # ),
    
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
              Dam_Strain:ParaType,
              Litter_num
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
              Dam_Strain:ParaType,
              Litter_num
            ),
          selected_group = c("Treatment")
        ),
        
        verbatimTextOutput(ns("VO_tTest")),
        verbatimTextOutput(ns("VO_mass_tTest")),
        
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
              Dam_Strain:ParaType,
              Litter_num
            ),
          selected_group = c("Treatment")
        ),
        
        verbatimTextOutput(ns("Estrus_tTest")),
        verbatimTextOutput(ns("Estrus_mass_tTest")),
        
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
              Dam_Strain:ParaType,
              Litter_num
            ),
          selected_group = c("Treatment")
        ),
        
        verbatimTextOutput(ns("PPS_tTest")),
        verbatimTextOutput(ns("PPS_mass_tTest")),
        
      ), #End PPS summary
      
      tabPanel(
        "Litter Size",
        
        plotOutput(ns("LitterSizeVO")),
        plotOutput(ns("LitterSizeFirstE")),
        plotOutput(ns("LitterSizePPS"))
      ),
      
      tabPanel(
        "PND by Mass",
        
        plotOutput(ns("PND_MassVO")),
        plotOutput(ns("PND_MassFirstE")),
        plotOutput(ns("PND_MassPPS"))
      )
    ) #end tabsetPanel
    
  )
}


maturationOffServer <- function(
  id,
  Maturation_off,
  Maturation_litter1
){
  moduleServer(
    id,
    function(input, output, session) {
      
      Maturation_litter1_forBinding <- Maturation_litter1 %>%
        select(
          Mouse_ID,
          VO_mass,
          VO_age,
          Estrus_mass,
          Estrus_age,
          PreputialSep_mass,
          PreputialSep_age,
          Sex,
          Litter_num,
          Treatment,
          DOB,
          Litter_size_wean
        )
      
      Maturation_litter1_forBinding <- Maturation_litter1_forBinding %>%
        mutate(
          Litter_size = Litter_size_wean
        )
      
      Maturation_off <- Maturation_off %>%
        mutate(
          Litter_size = Litter_size_endPara
        )
      
      Maturation_off <- Maturation_off %>% full_join(Maturation_litter1_forBinding)
      
      MaturationOff_react <- filteringDFServer("MaturationOff_filter", Maturation_off)
      
      # MaturationOff_react <- reactive({
      #   if(input$undisturbedFirstLitter){
      #     df1 <- Maturation_off %>% full_join(Maturation_litter1_forBinding)
      #     df <- filteringDFServer("MaturationOff_filter", df1)
      #   } else {
      #     df <- filteringDFServer("MaturationOff_filter", Maturation_off)
      #   }
      #   return (df())
      # })
      
      ### Cumulative Frequency Plots --------
      cumFreq_zoomX <- zoomAxisServer("cumFreq_zoomX", "x", minVal = 21, maxVal = 50)
      
      lineSchemes <- scale_linetype_manual(
        breaks = c("1", "2", "undisturbed"),
        values = c("1" = "solid", "2" = "dashed", "undisturbed" = "twodash"),
        labels = c("First Litter", "Second Litter", "Undisturbed")
      )
      
      output$VO_cumFreq <- renderPlot({
        my_cumulative_freq_plot(
          df = MaturationOff_react(),
          color_var = expr(Treatment),
          linetype_var = expr(Litter_num),
          var_to_plot = expr(VO_age), #as expr()
          phenotype_name = "VO", #string
          title = TRUE,
          change_xmax = cumFreq_zoomX$zoom(),
          xmax = cumFreq_zoomX$max(),
          xmin = cumFreq_zoomX$min()
        ) + lineSchemes
      })
      
      output$FirstE_cumFreq <- renderPlot({
        my_cumulative_freq_plot(
          df = MaturationOff_react(),
          color_var = expr(Treatment),
          linetype_var = expr(Litter_num),
          var_to_plot = expr(Estrus_age), #as expr()
          phenotype_name = "First Estrus", #string
          title = TRUE,
          change_xmax = cumFreq_zoomX$zoom(),
          xmax = cumFreq_zoomX$max(),
          xmin = cumFreq_zoomX$min()
        ) + lineSchemes
      })
      
      output$PPS_cumFreq <- renderPlot({
        my_cumulative_freq_plot(
          df = MaturationOff_react(),
          color_var = expr(Treatment),
          linetype_var = expr(Litter_num),
          var_to_plot = expr(PreputialSep_age), #as expr()
          phenotype_name = "PPS", #string
          title = TRUE,
          change_xmax = cumFreq_zoomX$zoom(),
          xmax = cumFreq_zoomX$max(),
          xmin = cumFreq_zoomX$min()
        ) + lineSchemes
      })
      
      ### Dot Plots Day -------- 
      dotDay_zoomY <- zoomAxisServer("dotDay_zoomY", "Y", minVal = 0, maxVal = 50)
      
      output$VO_dot <- renderPlot({
        my_puberty_dot_plot(
          df = MaturationOff_react(),
          expr(VO_age), #expr()
          phenotype_name = "VO",
          shape = expr(Litter_num),
          colour = expr(Litter_num),
          width = 0.3,
          change_ymax = dotDay_zoomY$zoom(),
          ymin = dotDay_zoomY$min(),
          ymax = dotDay_zoomY$max(),
          DaysOrMass = "Days"
        ) 
        #+
          # scale_color_discrete(
          #   breaks = c("1", "2"),
          #   labels = c("First Litter", "Second Litter")
          # )
      })
      
      output$FirstE_dot <- renderPlot({
        my_puberty_dot_plot(
          df = MaturationOff_react(),
          expr(Estrus_age), #expr()
          phenotype_name = "First Estrus",
          shape = expr(Litter_num),
          colour = expr(Litter_num),
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
          shape = expr(Litter_num),
          colour = expr(Litter_num),
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
          shape = expr(Litter_num),
          colour = expr(Litter_num),
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
          shape = expr(Litter_num),
          colour = expr(Litter_num),
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
          shape = expr(Litter_num),
          colour = expr(Litter_num),
          width = 0.3,
          change_ymax = dotMass_zoomY$zoom(),
          ymin = dotMass_zoomY$min(),
          ymax = dotMass_zoomY$max(),
          DaysOrMass = "Mass"
        )
      })
      
      output$LitterSizeVO <- renderPlot({
        MaturationOff_react() %>%
        ggplot(aes(
          x = Litter_size, y = VO_age, 
          color = Treatment,
          shape = Litter_num
        )
        ) +
          geom_jitter(
            alpha = 0.6,
            width = 0.2,
            size = 2
            # position = position_jitterdodge(jitter.width = .2)
          )+
          coord_cartesian(ylim = c(0, NA)) +
          scale_colour_manual(
            values = c("gray 20", "gray 70"),
            breaks = c("Control", "LBN")
          ) +
          scale_shape_discrete(
            breaks = c("1", "2", "undisturbed"),
            labels = c("First Litter", "Second Litter", "Undisturbed")
          ) +
          stat_summary(fun = mean, geom = "line", aes(linetype = interaction(Treatment, Litter_num)), size = 1, alpha = .6)+
          my_theme +
          guides(color = FALSE) + 
          scale_linetype_discrete(
            breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
            labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
          )+
          theme(legend.position="bottom",
                legend.box="vertical", 
                legend.margin=margin()
          )
      })
      
      output$LitterSizeFirstE <- renderPlot({
        MaturationOff_react() %>%
          ggplot(aes(
            x = Litter_size, y = Estrus_age, 
            color = Treatment,
            shape = Litter_num
          )
          ) +
          geom_jitter(
            alpha = 0.6,
            width = 0.2,
            size = 2
            # position = position_jitterdodge(jitter.width = .2)
          )+
          coord_cartesian(ylim = c(0, NA)) +
          scale_colour_manual(
            values = c("gray 20", "gray 70"),
            breaks = c("Control", "LBN")
          ) +
          scale_shape_discrete(
            breaks = c("1", "2", "undisturbed"),
            labels = c("First Litter", "Second Litter", "Undisturbed")
          ) +
          stat_summary(fun = mean, geom = "line", aes(linetype = interaction(Treatment, Litter_num)), size = 1, alpha = .6)+
          my_theme +
          guides(color = FALSE) + 
          scale_linetype_discrete(
            breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
            labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
          )+
          theme(legend.position="bottom",
                legend.box="vertical", 
                legend.margin=margin()
          )
      })
      
      output$LitterSizePPS <- renderPlot({
        MaturationOff_react() %>%
          ggplot(aes(
            x = Litter_size, y = PreputialSep_age, 
            color = Treatment,
            shape = Litter_num
          )
          ) +
          geom_jitter(
            alpha = 0.6,
            width = 0.2,
            size = 2
            # position = position_jitterdodge(jitter.width = .2)
          )+
          coord_cartesian(ylim = c(0, NA)) +
          scale_colour_manual(
            values = c("gray 20", "gray 70"),
            breaks = c("Control", "LBN")
          ) +
          scale_shape_discrete(
            breaks = c("1", "2", "undisturbed"),
            labels = c("First Litter", "Second Litter", "Undisturbed")
          ) +
          stat_summary(fun = mean, geom = "line", aes(linetype = interaction(Treatment, Litter_num)), size = 1, alpha = .6)+
          my_theme +
          guides(color = FALSE) + 
          scale_linetype_discrete(
            breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
            labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
          )+
          theme(legend.position="bottom",
                legend.box="vertical", 
                legend.margin=margin()
          )
      })
      
      output$PND_MassVO <- renderPlot({
        MaturationOff_react() %>%
          filter(!is.na(VO_age)) %>%
          ggplot(aes(
            x = VO_age, y = VO_mass, 
            color = Treatment,
            shape = Litter_num
          )
          ) +
          geom_jitter(
            alpha = 0.6,
            width = 0.2,
            size = 2
            # position = position_jitterdodge(jitter.width = .2)
          )+
          coord_cartesian(ylim = c(0, NA)) +
          scale_colour_manual(
            values = c("gray 20", "gray 70"),
            breaks = c("Control", "LBN")
          ) +
          scale_shape_discrete(
            breaks = c("1", "2", "undisturbed"),
            labels = c("First Litter", "Second Litter", "Undisturbed")
          ) +
          geom_smooth(method='lm', se = FALSE) +
          # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(Treatment, Litter_num)), size = 1, alpha = .6)+
          my_theme +
          # guides(color = FALSE) + 
          # scale_linetype_discrete(
          #   breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
          #   labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
          # )+
          theme(legend.position="bottom",
                legend.box="vertical", 
                legend.margin=margin()
          )
      })
      
      output$PND_MassFirstE <- renderPlot({
        MaturationOff_react() %>%
          ggplot(aes(
            x = Estrus_age, y = Estrus_mass, 
            color = Treatment,
            shape = Litter_num
          )
          ) +
          geom_jitter(
            alpha = 0.6,
            width = 0.2,
            size = 2
            # position = position_jitterdodge(jitter.width = .2)
          )+
          coord_cartesian(ylim = c(0, NA)) +
          scale_colour_manual(
            values = c("gray 20", "gray 70"),
            breaks = c("Control", "LBN")
          ) +
          scale_shape_discrete(
            breaks = c("1", "2", "undisturbed"),
            labels = c("First Litter", "Second Litter", "Undisturbed")
          ) +
          geom_smooth(method='lm', se = FALSE) +
          # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(Treatment, Litter_num)), size = 1, alpha = .6)+
          my_theme +
          guides(color = FALSE) + 
          scale_linetype_discrete(
            breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
            labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
          )+
          theme(legend.position="bottom",
                legend.box="vertical", 
                legend.margin=margin()
          )
      })
      
      output$PND_MassPPS <- renderPlot({
        MaturationOff_react() %>%
          ggplot(aes(
            x = PreputialSep_age, y = PreputialSep_mass, 
            color = Treatment,
            shape = Litter_num
          )
          ) +
          geom_jitter(
            alpha = 0.6,
            width = 0.2,
            size = 2
            # position = position_jitterdodge(jitter.width = .2)
          )+
          coord_cartesian(ylim = c(0, NA)) +
          scale_colour_manual(
            values = c("gray 20", "gray 70"),
            breaks = c("Control", "LBN")
          ) +
          scale_shape_discrete(
            breaks = c("1", "2", "undisturbed"),
            labels = c("First Litter", "Second Litter", "Undisturbed")
          ) +
          geom_smooth(method='lm', se = FALSE) +
          # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(Treatment, Litter_num)), size = 1, alpha = .6)+
          my_theme +
          guides(color = FALSE) + 
          scale_linetype_discrete(
            breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
            labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
          )+
          theme(legend.position="bottom",
                legend.box="vertical", 
                legend.margin=margin()
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
      
      output$VO_mass_tTest <- renderPrint({
        t.test(VO_mass ~ Treatment, MaturationOff_react())
      })
      
      output$Estrus_tTest <- renderPrint({
        t.test(Estrus_age ~ Treatment, MaturationOff_react()) 
      })
      
      output$Estrus_mass_tTest <- renderPrint({
        t.test(Estrus_mass ~ Treatment, MaturationOff_react()) 
      })
      
      output$PPS_tTest <- renderPrint({
        t.test(PreputialSep_age ~ Treatment, MaturationOff_react())
      })
      
      output$PPS_mass_tTest <- renderPrint({
        t.test(PreputialSep_mass ~ Treatment, MaturationOff_react())
      })
    }
  )
}

