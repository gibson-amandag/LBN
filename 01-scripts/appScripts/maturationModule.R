### Offspring Maturation App Module

# https://shiny.rstudio.com/articles/modules.html

maturationUI <- function(id,
                      Maturation_off,
                      LBN_data){
  ns <- NS(id)
  tagList(
    
    h3("Offspring Maturation"),
    
    # Filtering ---------------------------------------------------------------
    filteringDFUI(ns("maturation_filter"), Maturation_off),
    
    tableOutput(ns("matTable")),
    
    fluidRow(
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("groupByDam"),
          "Group by dam",
          TRUE
        )
      )
    ),
    
    fluidRow(
      div(
        class = "col-xs-4",
        numericInput(
          ns("fontSize"),
          "font size",
          20
        ),
        numericInput(
          ns("dotSize"),
          "dot size",
          2.5
        )
      ),
      div(
        class= "col-xs-4",
        colourpicker::colourInput(
          ns("STDColor"),
          "STD Color",
          "white"
        )
      ),
      div(
        class = "col-xs-4",
        colourpicker::colourInput(
          ns("LBNColor"),
          "LBN Color",
          "cyan4"
        )
      )
    ),
    
    fluidRow(
      div(
        class = "col-xs-12 col-sm-3",
        div(
          class = "col-xs-12",
          radioButtons(
            ns("imgType"),
            "Select File Type",
            choices = c("png", "pdf")
          ),
        ),
      ),
      div(
        class = "col-xs-4 col-sm-3",
        actionButton(
          ns("fullPPT"),
          "Size for full PPT slide"
        ),
        actionButton(
          ns("halfPPT"),
          "Size for half PPT slide"
        )
      ),
      div(
        class = "col-xs-4 col-sm-3",
        radioButtons(
          ns("units"),
          "Units:",
          choices = c(
            "inches" = "in",
            "cm" = "cm",
            "mm" = "mm",
            "pixels" = "px"
          ),
          selected = "in"
        )
      ),
      div(
        class = "col-xs-4 col-sm-3",
        numericInput(
          ns("height"),
          "Height:",
          5.5
        ),
        numericInput(
          ns("width"),
          "Width:",
          4.33
        )
      )
    ),
    
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
          div(
            class = "col-xs-4",
            plotUI2(ns("VO_cumFreq"))
          ),
          div(
            class = "col-xs-4",
            plotUI2(ns("FirstE_cumFreq"))
          ),
          div(
            class = "col-xs-4",
            plotUI2(ns("PPS_cumFreq"))
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
          div(
            class = "col-xs-4",
            plotUI2(ns("VO_dot"))
          ),
          div(
            class = "col-xs-4",
            plotUI2(ns("FirstE_dot"))
          ),
          div(
            class = "col-xs-4",
            plotUI2(ns("PPS_dot"))
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
          div(
            class = "col-xs-4",
            plotUI2(ns("VO_dot_mass"))
          ),
          div(
            class = "col-xs-4",
            plotUI2(ns("FirstE_dot_mass"))
          ),
          div(
            class = "col-xs-4",
            plotUI2(ns("PPS_dot_mass"))
          )
        )
      )
      # ), #end plots panel # change when adding
      
      ## Summary Tables --------
      # tabPanel(
      #   "AGD Summary",
      #   h4("Ano-genital distance"),
      #   
      #   summaryTableUI(
      #     id = ns("AGDSum"), 
      #     df_sum = Maturation_off %>%
      #       select(AGD_wean:AGD_adult_by_mass, AGD_P22:AGD_P72), #data frame with possible columns
      #     selected_sum = c(
      #       "AGD_wean",
      #       "AGD_adult",
      #       "Mass_P9",
      #       "Mass_P11"
      #     ), # c(" ", " ") vector with selected variables
      #     df_group = Maturation_off %>%
      #       select(
      #         Sex:earlyLifeTrt,
      #         Dam_ID,
      #         Dam_Strain:ParaType,
      #         Litter_num
      #       ),
      #     selected_group = c(
      #       "Sex",
      #       "earlyLifeTrt"
      #     )
      #   )
      # ), #end AGD summary
      # 
      # tabPanel(
      #   "VO Summary",
      #   h4("Vaginal Opening"),
      #   
      #   summaryTableUI(
      #     id = ns("VOSum"), 
      #     df_sum = Maturation_off %>%
      #       select(VO_age, VO_mass), #data frame with possible columns
      #     selected_sum = c("VO_age", "VO_mass"), # c(" ", " ") vector with selected variables
      #     df_group = Maturation_off %>%
      #       select(
      #         earlyLifeTrt,
      #         Dam_ID,
      #         Dam_Strain:ParaType,
      #         Litter_num
      #       ),
      #     selected_group = c("earlyLifeTrt")
      #   ),
      #   p("Welch test corrects for different sample sizes, and may result in a different p-value than the regression analysis "),
      #   verbatimTextOutput(ns("VO_tTest")),
      #   verbatimTextOutput(ns("VO_mass_tTest")),
      #   
      # ), #End VO summary
      # 
      # tabPanel(
      #   "Estrus Summary",
      #   h4("First Estrus"),
      #   
      #   summaryTableUI(
      #     id = ns("EstrusSum"), 
      #     df_sum = Maturation_off %>%
      #       select(Estrus_age, Estrus_mass), #data frame with possible columns
      #     selected_sum = c("Estrus_age", "Estrus_mass"), # c(" ", " ") vector with selected variables
      #     df_group = Maturation_off %>%
      #       select(
      #         earlyLifeTrt,
      #         Dam_ID,
      #         Dam_Strain:ParaType,
      #         Litter_num
      #       ),
      #     selected_group = c("earlyLifeTrt")
      #   ),
      #   p("Welch test corrects for different sample sizes, and may result in a different p-value than the regression analysis "),
      #   verbatimTextOutput(ns("Estrus_tTest")),
      #   verbatimTextOutput(ns("Estrus_mass_tTest")),
      #   
      # ), #End estrus summary
      # 
      # tabPanel(
      #   "PPS Summary",
      #   h4("Preputial Separation"),
      #   
      #   summaryTableUI(
      #     id = ns("PPSSum"), 
      #     df_sum = Maturation_off %>%
      #       select(PreputialSep_age, PreputialSep_mass), #data frame with possible columns
      #     selected_sum = c("PreputialSep_age", "PreputialSep_mass"), # c(" ", " ") vector with selected variables
      #     df_group = Maturation_off %>%
      #       select(
      #         earlyLifeTrt,
      #         Dam_ID,
      #         Dam_Strain:ParaType,
      #         Litter_num
      #       ),
      #     selected_group = c("earlyLifeTrt")
      #   ),
      #   
      #   p("Welch test corrects for different sample sizes, and may result in a different p-value than the regression analysis "),
      #   verbatimTextOutput(ns("PPS_tTest")),
      #   verbatimTextOutput(ns("PPS_mass_tTest")),
      #   
      # ), #End PPS summary
      
      ## Litter Size ------
      
      # tabPanel(
      #   "Litter Size",
      #   
      #   plotUI2(ns("LitterSizeVO")),
      #   plotUI2(ns("LitterSizeFirstE")),
      #   plotUI2(ns("LitterSizePPS"))
      # ),
      # 
      # tabPanel(
      #   "PND by Mass",
      #   
      #   plotUI2(ns("PND_MassVO")),
      #   plotUI2(ns("PND_MassFirstE")),
      #   plotUI2(ns("PND_MassPPS"))
      # ),
      # 
      # tabPanel(
      #   "Regression",
      #   maturationRegUI(ns("matReg"), Maturation_off, LBN_data)
      # )
    ), #end tabsetPanel
    
    ## From massOff -----
    # tabsetPanel(
    #   tabPanel(
    #     "Graphs",
    #     
    #     fluidRow(
    #       div(
    #         class = "col-xs-4",
    #         checkboxInput(
    #           ns("useLineType"),
    #           "Use different linetypes",
    #           FALSE
    #         )
    #       ),
    #       div(
    #         class = "col-xs-4",
    #         checkboxInput(
    #           ns("individualLines"),
    #           "Individual lines",
    #           TRUE
    #         )
    #       ),
    #       div(
    #         class = "col-xs-4",
    #         checkboxInput(
    #           ns("meanLines"),
    #           "Mean lines",
    #           TRUE
    #         )
    #       )
    #     ),
    #     
    #     zoomAxisUI(ns("zoom_x"), "x"),
    #     
    #     zoomAxisUI(ns("zoom_y"), "y"),
    #     
    #     
    #     fluidRow(
    #       div(
    #         class = "col-xs-4",
    #         sliderInput(
    #           ns("indivAlpha"),
    #           "individual line alpha",
    #           min = 0,
    #           max = 1,
    #           value = 0.5
    #         )
    #       ),
    #       div(
    #         class = "col-xs-4",
    #         sliderInput(
    #           ns("meanAlpha"),
    #           "mean line alpha",
    #           min = 0,
    #           max = 1,
    #           value = 1
    #         )
    #       ),
    #       div(
    #         class = "col-xs-4",
    #         sliderInput(
    #           ns("errorBarAlpha"),
    #           "error bar alpha",
    #           min = 0,
    #           max = 1,
    #           value = 1
    #         )
    #       )
    #     ),
    #     
    #     # plotOutput(ns("plot"))
    #     tableOutput(ns("plotInfo")),
    #     plotUI2(ns("plot"))
    #   ),
    #   tabPanel(
    #     "ANOVA",
    #     varSelectInput(
    #       ns("betweenVars"),
    #       "Select 'between' variables",
    #       data = Maturation_off %>% select(earlyLifeTrt, sex),
    #       multiple = TRUE
    #     ),
    #     uiOutput(
    #       ns("massANOVA")
    #     ),
    #     uiOutput(
    #       ns("byDayANOVA")
    #     )
    #   )
    # ),
    
    
  )
}


maturationServer <- function(
  id,
  Maturation_off,
  Demo_dam,
  Demo_dam_for_offspring,
  niceNames,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      # Plotting information -------
      convertUnits <- function(currentVal, currentUnits, desiredUnits){
        conversions <- dplyr::tibble(
          unit = c("in", "cm", "mm", "px"),
          std = c(100/2.54, 100, 1000, 100/2.54*96)
        )
        stdVal <- currentVal / conversions$std[which(conversions$unit == currentUnits)]
        newVal <- stdVal * conversions$std[which(conversions$unit == desiredUnits)]
        return(newVal)
      }
      
      prevUnits <- reactiveVal("in")
      
      observeEvent(
        input$fullPPT,
        {
          height <- convertUnits(5, "in", input$units)
          width <- convertUnits(11.5, "in", input$units)
          updateNumericInput(
            session,
            "height",
            value = height
          )
          updateNumericInput(
            session,
            "width",
            value = width
          )
        }
      )
      
      observeEvent(
        input$halfPPT,
        {
          height <- convertUnits(5.33, "in", input$units)
          width <- convertUnits(6.07, "in", input$units)
          updateNumericInput(
            session,
            "height",
            value = height
          )
          updateNumericInput(
            session,
            "width",
            value = width
          )
        }
      )
      
      observeEvent(
        input$units,
        {
          height <- convertUnits(input$height, prevUnits(), input$units)
          width <- convertUnits(input$width, prevUnits(), input$units)
          prevUnits(input$units)
          updateNumericInput(
            session,
            "height",
            value = height
          )
          updateNumericInput(
            session,
            "width",
            value = width
          )
        }
      )
      
      # Filtering information --------
      
      # maturationRegServer("matReg", Maturation_off, LBN_data)
      MaturationOff_react <- filteringDFServer("maturation_filter", Maturation_off)
      
      ### Cumulative Frequency Plots --------
      cumFreq_zoomX <- zoomAxisServer("cumFreq_zoomX", "x", minVal = 21, maxVal = 50)
      
      # 2021-10-19 - Haven't remade the cumulative frequency plot yet. Not top priority
      # Repeat for other plots
      # VO_cumFreqPlot <- reactive({
      #   my_cumulative_freq_plot(
      #     df = MaturationOff_react(),
      #     color_var = expr(earlyLifeTrt),
      #     linetype_var = expr(earlyLifeTrt),
      #     var_to_plot = expr(VO_age), #as expr()
      #     phenotype_name = "VO", #string
      #     title = TRUE,
      #     change_xmax = cumFreq_zoomX$zoom(),
      #     xmax = cumFreq_zoomX$max(),
      #     xmin = cumFreq_zoomX$min()
      #   ) 
      # })
      # 
      # VO_cumFreq_info <- plotServer("VO_cumFreq", VO_cumFreqPlot, "VO_cumFreq", compType) 
      
      ### Dot Plots Day -------- 
      dotDay_zoomY <- zoomAxisServer("dotDay_zoomY", "Y", minVal = 0, maxVal = 50)
      
      plotAgeScatter <- function(
        df,
        yVar,
        title
      ){
        scatterPlotLBN(
          df = df ,
          yVar = {{ yVar }},
          yLab = "age (days)",
          STDColor = input$STDColor,
          LBNColor = input$LBNColor,
          textSize = input$fontSize,
          zoom_y = dotDay_zoomY$zoom(), # Zoom to part of y axis
          ymin = dotDay_zoomY$min(),
          ymax = dotDay_zoomY$max(),
          dotSize = input$dotSize,
          fillAlpha = 1,
          jitterWidth = 0.35,
          jitterHeight = 0,
          title = title
        )
      }
      
      VO_dot_plot <- reactive({
        MaturationOff_react() %>%
          filter(!is.na(VO_age)) %>%
          plotAgeScatter(yVar = VO_age, title = "Vaginal Opening")+
          stat_compare_means(
            method = "t.test",
            label.y = max(MaturationOff_react()$VO_age, na.rm = TRUE) + 2,
            size = 9, 
            family = "Arial", 
            color = "black"
          )
      })
      
      VO_dot_info <- plotServer2(
        "VO_dot", 
        VO_dot_plot, 
        "VO_age", 
        compType,
        imgType = reactive(input$imgType),
        units = reactive(input$units),
        height = reactive(input$height),
        width = reactive(input$width)
        )
      
      FirstE_dot_plot <- reactive({
        MaturationOff_react() %>%
          filter(!is.na(Estrus_age)) %>%
          plotAgeScatter(yVar = Estrus_age, title = "First Estrus")+
          stat_compare_means(
            method = "t.test",
            label.y = max(MaturationOff_react()$Estrus_age, na.rm = TRUE) + 2,
            size = 9, 
            family = "Arial", 
            color = "black"
          )
      })
      
      FirstE_dot_info <- plotServer2(
        "FirstE_dot", 
        FirstE_dot_plot, 
        "FirstE_age", 
        compType,
        imgType = reactive(input$imgType),
        units = reactive(input$units),
        height = reactive(input$height),
        width = reactive(input$width)
        )
      
      PPS_dot_plot <- reactive({
        MaturationOff_react() %>%
          filter(!is.na(PreputialSep_age)) %>%
          plotAgeScatter(yVar = PreputialSep_age, title = "Preputial Separation")+
          stat_compare_means(
            method = "t.test",
            label.y = max(MaturationOff_react()$PreputialSep_age, na.rm = TRUE) + 2,
            size = 9, 
            family = "Arial", 
            color = "black"
          )
      })
      
      PPS_dot_info <- plotServer2(
        "PPS_dot", 
        PPS_dot_plot, 
        "PPS_age", 
        compType,
        imgType = reactive(input$imgType),
        units = reactive(input$units),
        height = reactive(input$height),
        width = reactive(input$width)
        )
      
      # output$FirstE_dot <- renderPlot({
      #   my_puberty_dot_plot(
      #     df = MaturationOff_react(),
      #     expr(Estrus_age), #expr()
      #     phenotype_name = "First Estrus",
      #     shape = expr(earlyLifeTrt),
      #     colour = expr(earlyLifeTrt),
      #     width = 0.3,
      #     change_ymax = dotDay_zoomY$zoom(),
      #     ymin = dotDay_zoomY$min(),
      #     ymax = dotDay_zoomY$max(),
      #     DaysOrMass = "Days"
      #   ) + 
      #     stat_compare_means(method = "t.test", label.y = max(MaturationOff_react()$Estrus_age, na.rm = TRUE) + 2)
      # })
      # 
      # output$PPS_dot <- renderPlot({
      #   my_puberty_dot_plot(
      #     df = MaturationOff_react(),
      #     expr(PreputialSep_age), #expr()
      #     phenotype_name = "PPS",
      #     shape = expr(earlyLifeTrt),
      #     colour = expr(earlyLifeTrt),
      #     width = 0.3,
      #     change_ymax = dotDay_zoomY$zoom(),
      #     ymin = dotDay_zoomY$min(),
      #     ymax = dotDay_zoomY$max(),
      #     DaysOrMass = "Days"
      #   ) + 
      #     stat_compare_means(method = "t.test", label.y = max(MaturationOff_react()$PreputialSep_age, na.rm = TRUE) + 2)
      # })
      
      ### Dot Plots Mass --------
      dotMass_zoomY <- zoomAxisServer("dotMass_zoomY", "Y", minVal = 0, maxVal = 25)
      
      # output$VO_dot_mass <- renderPlot({
      #   my_puberty_dot_plot(
      #     df = MaturationOff_react(),
      #     expr(VO_mass), #expr()
      #     phenotype_name = "VO",
      #     shape = expr(earlyLifeTrt),
      #     colour = expr(earlyLifeTrt),
      #     width = 0.3,
      #     change_ymax = dotMass_zoomY$zoom(),
      #     ymin = dotMass_zoomY$min(),
      #     ymax = dotMass_zoomY$max(),
      #     DaysOrMass = "Mass"
      #   ) + 
      #     stat_compare_means(method = "t.test", label.y = max(MaturationOff_react()$VO_mass, na.rm = TRUE) + 2)
      # })
      # 
      # output$FirstE_dot_mass <- renderPlot({
      #   my_puberty_dot_plot(
      #     df = MaturationOff_react(),
      #     expr(Estrus_mass), #expr()
      #     phenotype_name = "First Estrus",
      #     shape = expr(earlyLifeTrt),
      #     colour = expr(earlyLifeTrt),
      #     width = 0.3,
      #     change_ymax = dotMass_zoomY$zoom(),
      #     ymin = dotMass_zoomY$min(),
      #     ymax = dotMass_zoomY$max(),
      #     DaysOrMass = "Mass"
      #   ) + 
      #     stat_compare_means(method = "t.test", label.y = max(MaturationOff_react()$Estrus_mass, na.rm = TRUE) + 2)
      # })
      # 
      # output$PPS_dot_mass <- renderPlot({
      #   my_puberty_dot_plot(
      #     df = MaturationOff_react(),
      #     expr(PreputialSep_mass), #expr()
      #     phenotype_name = "PPS",
      #     shape = expr(earlyLifeTrt),
      #     colour = expr(earlyLifeTrt),
      #     width = 0.3,
      #     change_ymax = dotMass_zoomY$zoom(),
      #     ymin = dotMass_zoomY$min(),
      #     ymax = dotMass_zoomY$max(),
      #     DaysOrMass = "Mass"
      #   ) + 
      #     stat_compare_means(method = "t.test", label.y = max(MaturationOff_react()$PreputialSep_mass, na.rm = TRUE) + 2)
      # })
      # 
      # output$LitterSizeVO <- renderPlot({
      #   MaturationOff_react() %>%
      #     ggplot(aes(
      #       x = Litter_size_endPara, y = VO_age, 
      #       color = earlyLifeTrt,
      #       shape = Cohort
      #     )
      #     ) +
      #     geom_jitter(
      #       alpha = 0.6,
      #       width = 0.2,
      #       size = 2
      #       # position = position_jitterdodge(jitter.width = .2)
      #     )+
      #     coord_cartesian(ylim = c(0, NA)) +
      #     scale_colour_manual(
      #       values = c("gray 20", "gray 70"),
      #       breaks = c("Control", "LBN")
      #     ) +
      #     geom_smooth(method='lm', se = FALSE) +
      #     my_theme +
      #     theme(legend.position="bottom",
      #           legend.box="vertical", 
      #           legend.margin=margin()
      #     )
      # })
      # 
      # output$LitterSizeFirstE <- renderPlot({
      #   MaturationOff_react() %>%
      #     ggplot(aes(
      #       x = Litter_size_endPara, y = Estrus_age, 
      #       color = earlyLifeTrt,
      #       shape = Litter_num
      #     )
      #     ) +
      #     geom_jitter(
      #       alpha = 0.6,
      #       width = 0.2,
      #       size = 2
      #       # position = position_jitterdodge(jitter.width = .2)
      #     )+
      #     coord_cartesian(ylim = c(0, NA)) +
      #     scale_colour_manual(
      #       values = c("gray 20", "gray 70"),
      #       breaks = c("Control", "LBN")
      #     ) +
      #     scale_shape_discrete(
      #       breaks = c("1", "2", "undisturbed"),
      #       labels = c("First Litter", "Second Litter", "Undisturbed")
      #     ) +
      #     geom_smooth(method='lm', se = FALSE) +
      #     # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(earlyLifeTrt, Litter_num)), size = 1, alpha = .6)+
      #     my_theme +
      #     guides(color = FALSE) + 
      #     scale_linetype_discrete(
      #       breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
      #       labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
      #     )+
      #     theme(legend.position="bottom",
      #           legend.box="vertical", 
      #           legend.margin=margin()
      #     )
      # })
      # 
      # output$LitterSizePPS <- renderPlot({
      #   MaturationOff_react() %>%
      #     ggplot(aes(
      #       x = Litter_size_endPara, y = PreputialSep_age, 
      #       color = earlyLifeTrt,
      #       shape = Litter_num
      #     )
      #     ) +
      #     geom_jitter(
      #       alpha = 0.6,
      #       width = 0.2,
      #       size = 2
      #       # position = position_jitterdodge(jitter.width = .2)
      #     )+
      #     coord_cartesian(ylim = c(0, NA)) +
      #     scale_colour_manual(
      #       values = c("gray 20", "gray 70"),
      #       breaks = c("Control", "LBN")
      #     ) +
      #     scale_shape_discrete(
      #       breaks = c("1", "2", "undisturbed"),
      #       labels = c("First Litter", "Second Litter", "Undisturbed")
      #     ) +
      #     geom_smooth(method='lm', se = FALSE) +
      #     # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(earlyLifeTrt, Litter_num)), size = 1, alpha = .6)+
      #     my_theme +
      #     guides(color = FALSE) + 
      #     scale_linetype_discrete(
      #       breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
      #       labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
      #     )+
      #     theme(legend.position="bottom",
      #           legend.box="vertical", 
      #           legend.margin=margin()
      #     )
      # })
      # 
      # output$PND_MassVO <- renderPlot({
      #   MaturationOff_react() %>%
      #     filter(!is.na(VO_age)) %>%
      #     ggplot(aes(
      #       x = VO_age, y = VO_mass, 
      #       color = earlyLifeTrt,
      #       shape = Litter_num
      #     )
      #     ) +
      #     geom_jitter(
      #       alpha = 0.6,
      #       width = 0.2,
      #       size = 2
      #       # position = position_jitterdodge(jitter.width = .2)
      #     )+
      #     coord_cartesian(ylim = c(0, NA)) +
      #     scale_colour_manual(
      #       values = c("gray 20", "gray 70"),
      #       breaks = c("Control", "LBN")
      #     ) +
      #     scale_shape_discrete(
      #       breaks = c("1", "2", "undisturbed"),
      #       labels = c("First Litter", "Second Litter", "Undisturbed")
      #     ) +
      #     geom_smooth(method='lm', se = FALSE) +
      #     # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(earlyLifeTrt, Litter_num)), size = 1, alpha = .6)+
      #     my_theme +
      #     # guides(color = FALSE) + 
      #     # scale_linetype_discrete(
      #     #   breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
      #     #   labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
      #     # )+
      #     theme(legend.position="bottom",
      #           legend.box="vertical", 
      #           legend.margin=margin()
      #     )
      # })
      # 
      # output$PND_MassFirstE <- renderPlot({
      #   MaturationOff_react() %>%
      #     ggplot(aes(
      #       x = Estrus_age, y = Estrus_mass, 
      #       color = earlyLifeTrt,
      #       shape = Litter_num
      #     )
      #     ) +
      #     geom_jitter(
      #       alpha = 0.6,
      #       width = 0.2,
      #       size = 2
      #       # position = position_jitterdodge(jitter.width = .2)
      #     )+
      #     coord_cartesian(ylim = c(0, NA)) +
      #     scale_colour_manual(
      #       values = c("gray 20", "gray 70"),
      #       breaks = c("Control", "LBN")
      #     ) +
      #     scale_shape_discrete(
      #       breaks = c("1", "2", "undisturbed"),
      #       labels = c("First Litter", "Second Litter", "Undisturbed")
      #     ) +
      #     geom_smooth(method='lm', se = FALSE) +
      #     # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(earlyLifeTrt, Litter_num)), size = 1, alpha = .6)+
      #     my_theme +
      #     guides(color = FALSE) + 
      #     scale_linetype_discrete(
      #       breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
      #       labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
      #     )+
      #     theme(legend.position="bottom",
      #           legend.box="vertical", 
      #           legend.margin=margin()
      #     )
      # })
      # 
      # output$PND_MassPPS <- renderPlot({
      #   MaturationOff_react() %>%
      #     ggplot(aes(
      #       x = PreputialSep_age, y = PreputialSep_mass, 
      #       color = earlyLifeTrt,
      #       shape = Litter_num
      #     )
      #     ) +
      #     geom_jitter(
      #       alpha = 0.6,
      #       width = 0.2,
      #       size = 2
      #       # position = position_jitterdodge(jitter.width = .2)
      #     )+
      #     coord_cartesian(ylim = c(0, NA)) +
      #     scale_colour_manual(
      #       values = c("gray 20", "gray 70"),
      #       breaks = c("Control", "LBN")
      #     ) +
      #     scale_shape_discrete(
      #       breaks = c("1", "2", "undisturbed"),
      #       labels = c("First Litter", "Second Litter", "Undisturbed")
      #     ) +
      #     geom_smooth(method='lm', se = FALSE) +
      #     # stat_summary(fun = mean, geom = "line", aes(linetype = interaction(earlyLifeTrt, Litter_num)), size = 1, alpha = .6)+
      #     my_theme +
      #     guides(color = FALSE) + 
      #     scale_linetype_discrete(
      #       breaks = c("Control.1", "LBN.1", "Control.undisturbed"),
      #       labels = c("Control - Litter 1", "LBN - Litter 1", "Control - undisturbed")
      #     )+
      #     theme(legend.position="bottom",
      #           legend.box="vertical", 
      #           legend.margin=margin()
      #     )
      # })
      # 
      # ### AGD Summary -----
      # 
      # AGDSum <- summaryTableServer("AGDSum", MaturationOff_react) #doesn't seem to be changing
      # 
      # 
      # ### VO Summary -----
      # VOSum <- summaryTableServer("VOSum", MaturationOff_react)
      # 
      # ### Estrus Summary -----
      # EstrusSum <- summaryTableServer("EstrusSum", MaturationOff_react)
      # 
      # ### PPS Summary -----
      # PPSSum <- summaryTableServer("PPSSum", MaturationOff_react)
      # 
      # ### t-tests ------
      # output$VO_tTest <- renderPrint({
      #   t.test(VO_age ~ earlyLifeTrt, MaturationOff_react())
      # })
      # 
      # output$VO_mass_tTest <- renderPrint({
      #   t.test(VO_mass ~ earlyLifeTrt, MaturationOff_react())
      # })
      # 
      # output$Estrus_tTest <- renderPrint({
      #   t.test(Estrus_age ~ earlyLifeTrt, MaturationOff_react()) 
      # })
      # 
      # output$Estrus_mass_tTest <- renderPrint({
      #   t.test(Estrus_mass ~ earlyLifeTrt, MaturationOff_react()) 
      # })
      # 
      # output$PPS_tTest <- renderPrint({
      #   t.test(PreputialSep_age ~ earlyLifeTrt, MaturationOff_react())
      # })
      # 
      # output$PPS_mass_tTest <- renderPrint({
      #   t.test(PreputialSep_mass ~ earlyLifeTrt, MaturationOff_react())
      # })
      # 
      # 
      # ## Filtering -------------------------------------------------------------
      # 
      # Maturation_off_filtered <- filteringDFServer("maturation_filter", Maturation_off)
      # Maturation_off_react <- reactive({
      #   Maturation_off_filtered() %>% 
      #     filter(
      #       # remove D020 offspring, as small litter (2 pups) 
      #       # and female didn't survive
      #       damID != "D020-02" 
      #     )
      # })
      # zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      # zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 18)
      # 
      # massPlot <- reactive({
      #   groupVar <- ifelse(input$groupByDam, expr(damID), expr(mouseID))
      #   
      #   df <- Maturation_off_react()
      #   if(input$whichSex == "M"){
      #     df <- df %>%
      #       filter(sex == "M")
      #   }else if(input$whichSex == "F"){
      #     df <- df %>%
      #       filter(sex == "F")
      #   }
      #   
      #   df %>%
      #     plot_maturation_lines(
      #       groupByDam = input$groupByDam,
      #       useLineType = input$useLineType, # TRUE/FALSE
      #       lineTypeVar = earlyLifeTrt,
      #       lineGroupVar = {{ groupVar }},
      #       xtitle = "PND", #x axis label
      #       ytitle = "mass (g)", #y axis label
      #       title = NULL, # plot title
      #       individualLines = input$individualLines, # plot individual lines
      #       meanLines = input$meanLines, # plot mean lines with SE
      #       zoom_x = zoom_x$zoom(), # Zoom to part of x axis
      #       xmin = zoom_x$min(),
      #       xmax = zoom_x$max(),
      #       zoom_y = zoom_y$zoom(), # Zoom to part of y axis
      #       ymin = zoom_y$min(),
      #       ymax = zoom_y$max(),
      #       indivLineAlpha = input$indivAlpha,
      #       indivLineSize = 0.8,
      #       errorBarWidth = 0,
      #       meanLineSize = 1.4,
      #       meanAlpha = input$meanAlpha,
      #       errorBarSize = 1,
      #       # errorBarColor = "grey10",
      #       errorBarAlpha = input$errorBarAlpha,
      #       textSize = input$fontSize,
      #       axisSize = 0.5,
      #       legendPosition = "top",
      #       STDColor = input$STDColor,
      #       LBNColor = input$LBNColor
      #     )
      # })
      # 
      # plotInfo <- plotServer("plot", massPlot, "massPlot", compType)
      # # To access plot click information plotInfo$click()
      # # x = plotInfo$click()$x
      # 
      # output$plotInfo <- renderTable({
      #   
      #   df <- Maturation_off_react()
      #   if(input$whichSex == "M"){
      #     df <- df %>%
      #       filter(sex == "M")
      #   }else if(input$whichSex == "F"){
      #     df <- df %>%
      #       filter(sex == "F")
      #   }
      #   
      #   if(input$groupByDam){
      #     df <- df %>%
      #       getAvgByDam()
      #   }
      #   
      #   df_long <- df %>%
      #     makeOffMassLong()
      #   
      #   nearPoints(
      #     df_long %>% select(
      #       !! ifelse(input$groupByDam, expr(damID), expr(mouseID)),
      #       earlyLifeTrt,
      #       day,
      #       mass
      #     ), 
      #     plotInfo$click())
      # })
      # 
      # 
      # output$massANOVA <- renderUI({
      #   
      #   df <- Maturation_off_react()
      #   if(input$whichSex == "M"){
      #     df <- df %>%
      #       filter(sex == "M")
      #   }else if(input$whichSex == "F"){
      #     df <- df %>%
      #       filter(sex == "F")
      #   }
      #   
      #   # if(input$groupByDam){
      #   #   df <- df %>%
      #   #     getAvgByDam()
      #   # }
      #   
      #   df_long <- df %>%
      #     makeOffMassLong()
      #   
      #   anova <- df_long %>%
      #     filter(
      #       day != 4 # don't include day 4, start of paradigm
      #     ) %>%
      #     anova_test(
      #       dv = mass,
      #       wid = mouseID,
      #       within = day,
      #       # between = c(earlyLifeTrt, sex),
      #       between = c(!!! input$betweenVars),
      #       type = 3
      #     ) 
      #   
      #   anova$ANOVA%>%
      #     formatAnova() %>%
      #     htmltools_value()
      # })
      # 
      # output$byDayANOVA <- renderUI({
      #   df <- Maturation_off_react()
      #   if(input$whichSex == "M"){
      #     df <- df %>%
      #       filter(sex == "M")
      #   }else if(input$whichSex == "F"){
      #     df <- df %>%
      #       filter(sex == "F")
      #   }
      #   
      #   # if(input$groupByDam){
      #   #   df <- df %>%
      #   #     getAvgByDam()
      #   # }
      #   
      #   df_long <- df %>%
      #     makeOffMassLong()
      #   
      #   req(length(input$betweenVars) > 0)
      #   
      #   df_long %>%
      #     filter(
      #       day != 4 # don't include day 4, start of paradigm
      #     ) %>%
      #     group_by(day) %>%
      #     anova_test(
      #       dv = mass,
      #       wid = mouseID,
      #       # between = c(earlyLifeTrt, sex),
      #       between = c(!!! input$betweenVars),
      #       type = 3
      #     ) %>%
      #     adjust_pvalue(method = "bonferroni")%>%
      #     formatAdjAnova() %>%
      #     # colformat_num(
      #     #   j = "p.adj",
      #     #   digits = 5
      #     # ) %>%
      #     htmltools_value()
      # })
    }
  )
}

