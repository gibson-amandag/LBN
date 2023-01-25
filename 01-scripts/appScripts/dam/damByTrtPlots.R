### Dam Mass App Module

# https://shiny.rstudio.com/articles/modules.html

damByTrtUI <- function(
    id,
    damFramesAndBehaviorByDam
){
  ns <- NS(id)
  tagList(
    
    h3("Dam Information"),
    
    # Filtering ---------------------------------------------------------------
    filteringDFUI(ns("damTrtFilter"), damFramesAndBehaviorByDam),
    
    fluidRow(
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("facetByLitter"),
          "Facet by litter",
          TRUE
        )
      ),
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("facetByTrt"),
          "Facet by trt",
          FALSE
        )
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("dotSize"),
          "Dot size",
          value = 2
        )
      )
    ),
    
    tabsetPanel(
      ## Single Variable -----------------------------------------------------------
      tabPanel(
        "Single Variable",
        catPlotEarlyLifeTrtUI(
          ns("singleVar"),
          damFramesAndBehaviorByDam %>%
            select(
              Num_exits:clump8_propLitter
              , Litter_size
              , Avg_litter_mass_startPara
              , Cort_dam_P11
              , Cort_dam_P21
            )
        )
      ),
      
      ## Two-variable scatter --------------------------------------------------
      tabPanel(
        "Two Variable",
        scatterPlotEarlyLifeTrtUI(
          ns("twoVar"),
          damFramesAndBehaviorByDam %>%
            select(
              Num_exits:clump8_propLitter
              , Litter_size
              , Avg_litter_mass_startPara
              , Cort_dam_P11
              , Cort_dam_P21
            ),
          damFramesAndBehaviorByDam %>%
            select(
              Num_exits:clump8_propLitter
              , Litter_size
              , Avg_litter_mass_startPara
              , Cort_dam_P11
              , Cort_dam_P21
            )
        )
      ),
    )
  )
}


damByTrtServer <- function(
    id,
    damFramesAndBehaviorByDam,
    niceNames,
    compType
){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ## Filtering -------------------------------------------------------------
      
      Demo_dam_filtered <- filteringDFServer("damTrtFilter", damFramesAndBehaviorByDam)
      Demo_dam_react <- reactive({
        Demo_dam_filtered() %>% 
          filter(
            # remove D020 offspring, as small litter (2 pups) 
            # and female didn't survive
            damID != "D020-02" 
          )
      })
      # zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      # zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 35)
      
      ## Single Var -----------------------------------------------------------------------
      
      observeEvent(
        c(Demo_dam_react(),
          input$dotSize),
        {
          catPlotEarlyLifeTrtServer(
            "singleVar",
            Demo_dam_react(),
            getNiceName,
            c(
              !!! exprs(
                damID,
                earlyLifeTrt,
                cohort,
                litterNum,
                Litter_size
              )
            ),
            dotSize = input$dotSize,
            compType = compType
          )
        }
      )
      
      ## Two variable ----------------------------------------------------------
      observeEvent(
        c(Demo_dam_react(),
          input$dotSize),
        {
          scatterPlotEarlyLifeTrtServer(
            "twoVar",
            Demo_dam_react(),
            getNiceName,
            c(
              !!! exprs(
                damID,
                earlyLifeTrt,
                cohort,
                litterNum,
                Litter_size
              )
            ),
            dotSize = input$dotSize,
            compType = compType
          )
        }
      )
      
      # massPlot <- reactive({
      #   df <- Demo_dam_react()
      #   
      #   plot <- df %>%
      #     plot_dam_mass_lines(
      #       useLineType = input$useLineType, # TRUE/FALSE
      #       lineTypeVar = earlyLifeTrt,
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
      #       errorBarWidth = 1,
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
      #   
      #   if(input$facetByLitter){
      #     if(input$facetByTrt){
      #       plot <- plot + 
      #         facet_wrap(
      #           vars(earlyLifeTrt, litterNum),
      #           nrow = 1,
      #           labeller = labeller(litterNum = c("1" = "first", "2" = "second"))
      #         )
      #     } else {
      #       plot <- plot + 
      #         facet_wrap(
      #           vars(litterNum),
      #           labeller = labeller(litterNum = c("1" = "first", "2" = "second"))
      #         )
      #     }
      #     
      #   } else if(input$facetByTrt){
      #     plot <- plot + 
      #       facet_wrap(
      #         vars(earlyLifeTrt)
      #       )
      #   }
      #   return(plot)
      # })
      # 
      # plotInfo <- plotServer("plot", massPlot, "massPlot", compType)
      # # To access plot click information plotInfo$click()
      # # x = plotInfo$click()$x
      # 
      # output$plotInfo <- renderTable({
      #   
      #   df <- Demo_dam_react()
      #   
      #   df_long <- df %>%
      #     makeDamMassLong()
      #   
      #   nearPoints(
      #     df_long %>% select(
      #       damID,
      #       earlyLifeTrt,
      #       litterNum,
      #       day,
      #       mass
      #     ), 
      #     plotInfo$click())
      # })
      # 
      # output$massSummary <- renderTable({
      #   df <- Demo_dam_react()
      #   
      #   df_long <- df %>%
      #     makeDamMassLong()
      #   
      #   df_long %>%
      #     group_by(
      #       day,
      #       !!! input$betweenVars
      #     ) %>%
      #     meanSummary(mass)
      # })
      # 
      # output$massANOVA <- renderUI({
      #   
      #   df <- Demo_dam_react()
      #   
      #   df_long <- df %>%
      #     makeDamMassLong()
      #   
      #   anova <- df_long %>%
      #     anova_test(
      #       dv = mass,
      #       wid = damID,
      #       within = day,
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
      #   df <- Demo_dam_react()
      #   
      #   df_long <- df %>%
      #     makeDamMassLong()
      #   
      #   req(length(input$betweenVars) > 0)
      #   
      #   df_long %>%
      #     group_by(day) %>%
      #     anova_test(
      #       dv = mass,
      #       wid = damID,
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
      # 
      # output$demoTable <- renderDT({
      #   Demo_dam_react() %>%
      #     select(
      #       damID,
      #       earlyLifeTrt,
      #       litterNum,
      #       Dam_Mass_P4,
      #       Dam_Mass_P11,
      #       Dam_Mass_P21
      #     )
      # })
    }
  )
}

