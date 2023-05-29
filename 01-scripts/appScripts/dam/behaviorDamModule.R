### Dam Behavior App Module

# https://shiny.rstudio.com/articles/modules.html


behaviorDamUI <- function(
    id,
    Demo_dam,
    dfForVars,
    damFramesAndBehavior_ByPND_ZT
){
  ns <- NS(id)
  tagList(
    
    h3("Dam Behavior"),
    
    # Filtering ---------------------------------------------------------------
    filteringDFUI(ns("filter"), Demo_dam),
    
    fluidRow(
      div(
        class = "col-xs-4"
        , varSelectInput(
          ns("groupBy"),
          "Group by time?",
          damFramesAndBehavior_ByPND_ZT %>%
            select(
              PND, ZT
            ),
          multiple = TRUE
          , selected = c("ZT")
        ),
        
      ),
      div(
        class = "col-xs-4",
        varSelectInput(
          ns("singleVar"),
          "Select variable to summarize",
          dfForVars
        )
      )
    ),
    
    fluidRow(
      div(
        class = "col-xs-4",
        selectInput(
          ns("days"),
          "Which days?",
          choices = unique(damFramesAndBehavior_ByPND_ZT$PND), # Changed from levels to unique
          multiple = TRUE,
          selected = c(4:11)
        )
      ),
      div(
        class = "col-xs-4",
        selectInput(
          ns("ZTs"),
          "Which ZTs?",
          choices = sort(unique(damFramesAndBehavior_ByPND_ZT$ZT)), # Changed from levels to unique
          multiple = TRUE,
          selected = c(0:23)
        )
      )
      
    ),
    
    tabsetPanel(
      tabPanel(
        "Graphs",
        
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
              TRUE
            )
          ),
          div(
            class = "col-xs-4"
            , checkboxInput(
              ns("removeLegend")
              , "Remove legend"
              , TRUE
            )
          )
        ),
        
        fluidRow(
          div(
            class = "col-xs-4",
            numericInput(
              ns("fontSize"),
              "font size",
              16
            ),
          ),
          div(
            class= "col-xs-4",
            colourpicker::colourInput(
              ns("STDFill"),
              "STD Fill",
              "white"
            ),
            colourpicker::colourInput(
              ns("STDColor"),
              "STD Color",
              "grey30"
            )
          ),
          div(
            class = "col-xs-4",
            colourpicker::colourInput(
              ns("LBNFill"),
              "LBN Fill",
              "cyan4"
            ),
            colourpicker::colourInput(
              ns("LBNColor"),
              "LBN Color",
              "cyan4"
            )
          )
        ),
        fluidRow(
          div(
            class = "col-xs-4"
            , sliderInput(
              ns("lineSize"),
              "line size",
              min = 0,
              max = 3,
              value = .8
            )
          ),
          div(
            class = "col-xs-4",
            sliderInput(
              ns("indivAlpha"),
              "individual point alpha",
              min = 0,
              max = 1,
              value = 0.4
            )
          ),
          div(
            class = "col-xs-4"
            , checkboxInput(
              ns("addVertError")
              , "Add vertical error bars"
              , TRUE
            )
          )
        ),
        
        fluidRow(
          div(
            class = "col-xs-4",
            checkboxInput(
              ns("showDots"),
              "Show dots",
              TRUE
            )
          ),
          div(
            class = "col-xs-4",
            numericInput(
              ns("dotSize"),
              "Dot size",
              value = 3
            )
          ),
          div(
            class = "col-xs-4",
            sliderInput(
              ns("dodgeVal"),
              "Position dodge",
              min = 0,
              max = 1,
              value = 0
            )
          ),
        ),
        
        strong("Options when grouped by time"),
        
        fluidRow(
          div(
            class = "col-xs-4",
            checkboxInput(
              ns("triangleMean"),
              "Mean as triangle",
              TRUE
            )
          ),
          div(
            class = "col-xs-4",
            checkboxInput(
              ns("redMean"),
              "Make mean red",
              TRUE
            )
          ),
          div(
            class = "col-xs-4"
            , checkboxInput(
              ns("colorByDam")
              , "Color by dam"
              , TRUE
            )
          )
        ),
        
        zoomAxisUI(ns("zoom_x"), "x"),
        
        zoomAxisUI(ns("zoom_y"), "y"),
        
        tableOutput(ns("plotInfo")),
        plotUI(ns("plot"), height = "600px"),
        
        DTOutput(ns("demoTable"))
      ),
      # tabPanel(
      #   "ANOVA",
      #   varSelectInput(
      #     ns("betweenVars"),
      #     "Select 'between' variables",
      #     data = Demo_dam %>% select(earlyLifeTrt, litterNum),
      #     multiple = TRUE
      #   ),
      #   h4("Mean summary information"),
      #   
      #   tableOutput(ns("massSummary")),
      #   
      #   h4("ANOVA"),
      #   
      #   uiOutput(
      #     ns("massANOVA")
      #   ),
      #   
      #   h4("By day ANOVA"),
      #   
      #   uiOutput(
      #     ns("byDayANOVA")
      #   ),
      # )
    ),
    
      
  )
}


behaviorDamServer <- function(
  id,
  damBehaviorDF,
  damFramesDF,
  niceNames,
  compType,
  demoDamToAdd
){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ## Filtering -------------------------------------------------------------
      damBehavior_filtered <- filteringDFServer("filter", damBehaviorDF)
      damBehavior_react <- reactive({
        damBehavior_filtered()
      })
      
      damFrames_filtered <- filteringDFServer("filter", damFramesDF)
      damFrames_react <- reactive({
        damFrames_filtered()
      })
      
      yVar <- reactive(as.character(input$singleVar))
      yText <- reactive(getNiceName(yVar()))
      
      behaviorVars <- names(damBehaviorDF %>% select(
        Num_exits:Avg_dur_on_nest
      ))
      
      framesVars <- names(damFramesDF %>% select(
        damOnNest:clump8_percLitter
      ))
      
      df_picked <- reactive({
        
        if(yVar() %in% behaviorVars){
          df <- damBehavior_react()
        } else {
          df <- damFrames_react()
        }
        
        return(df)
      })
      
      useDF <- reactive({
        if(yVar() %in% behaviorVars){
          useDF <- "behavior"
        } else {
          useDF <- "frames"
        }
        return(useDF)
      })
      
      output$test <- renderText(useDF())
      
      df_react <- reactive({
        df <- df_picked() %>%
          filter(
            PND %in% input$days
            , ZT %in% input$ZTs
            # , !is.na(!! input$singleVar ) # remove 2023-05-12. Don't want to connect missing vals
          )
        
        if(useDF() == "behavior"){
          df <- df %>%
            group_by(
              damID
              , !!! input$groupBy
            ) %>%
            summarizeDamBehavior(demoDamToAdd)
        } else {
          df <- df %>%
            group_by(
              damID
              , !!! input$groupBy
            ) %>%
            summarizeDamFrames(demoDamToAdd)
        }
        return(df)
      })
      
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 35)
      
      behaviorPlot <- reactive({
        df <- df_react()
        
        # This works w/ expand_limits
        # plot <- df %>%
        #   plotDamBehavior_test(
        #     !! input$singleVar
        #   )
        
        # This works
        # plot <-  df %>%
        #   ggplot(
        #     aes(
        #       x = ZT
        #       , y = !! input$singleVar
        #     )
        #   ) +
        #   geom_line(
        #     alpha = 0.5,
        #     aes(group = damID, color = damID),
        #     position = position_dodge(0.4),
        #     size = 1
        #   ) +
        #   boxTheme()
        
        # this works
        # plot <-  df %>%
        #   ggplot(
        #     aes(
        #       x = ZT
        #       , y = !! input$singleVar
        #     )
        #   ) +
        #   geom_line(
        #     alpha = 0.5,
        #     aes(group = damID, color = damID),
        #     position = position_dodge(0.4),
        #     size = 1
        #   ) +
        #   boxTheme() +
        #   expand_limits(
        #     x = c(0,23)
        #   )
        
        plot <- df %>%
          plotDamBehavior(
            yVar = !! input$singleVar
            , yLab = yText()
            , fontSize = input$fontSize
            , dotSize = input$dotSize
            , lineSize = input$lineSize
            , lineAlpha = input$indivAlpha
            , dodgeVal = input$dodgeVal
            , addTriangleForMean = input$triangleMean
            , redMean = input$redMean
            , colorByDam = input$colorByDam
            , showDots = input$showDots
            , addVertError = input$addVertError
            , facetByTrt = input$facetByTrt
            , facetByLitter = input$facetByLitter
            , removeLegend = input$removeLegend
            , STDColor = input$STDColor
            , LBNColor = input$LBNColor
            , STDFill = input$STDFill
            , LBNFill = input$LBNFill
            , zoom_x = zoom_x$zoom() # Zoom to part of x axis
            , xmin = zoom_x$min()
            , xmax = zoom_x$max()
            , zoom_y = zoom_y$zoom() # Zoom to part of y axis
            , ymin = zoom_y$min()
            , ymax = zoom_y$max()
          )
        return(plot)
      })

      plotInfo <- plotServer("plot", behaviorPlot, "behaviorPlot", compType)
      # To access plot click information plotInfo$click()
        # x = plotInfo$click()$x

      output$plotInfo <- renderTable({
        
        if(!is.null(plotInfo$click())){
          if(length(input$groupBy) == 0){
            x <- plotInfo$click()$x
            xRound <- round(x)
            y <- plotInfo$click()$y
            catLevel <- plotInfo$click()$domain$discrete_limits$x[[xRound]]
            
            df_filtered2 <- reactive({df_react() %>%
                filter(
                  ! is.na(!! input$singleVar)
                ) %>%
                filter(
                  ! (row_number() %in% input$table_rows_selected)
                )
            })
            
            if(zoom_y$zoom()){
              yRange = zoom_y$max() - zoom_y$min()
            } else {
              yMin <- min(df_filtered2() %>% select(!! input$singleVar), na.rm = TRUE)
              yMax <- max(df_filtered2() %>% select(!! input$singleVar), na.rm = TRUE)
              yRange <- yMax - yMin
            }
            
            yError <- yRange * 0.03
            
            df <- df_filtered2() %>%
              filter(
                earlyLifeTrt == catLevel,
                !! input$singleVar <= y + yError & !! input$singleVar >= y - yError
              ) %>%
              select(
                damID,
                earlyLifeTrt,
                litterNum,
                !! input$singleVar
              )
            
          } else {
            df <- df_react()
            
            
            
            print(plotInfo$click())
            
            if(length(input$groupBy) == 2){
              df <- df %>%
                mutate(
                  dayTime = as_datetime(ymd_h(paste0(paste0("2000-01-", PND), paste0(" ", ZT)))),
                  .after = ZT
                )
            }

            df <- nearPoints(
              df %>% select(
                damID,
                earlyLifeTrt,
                litterNum,
                !!! input$groupBy,
                any_of(c("dayTime")),
                !! input$singleVar
              ),
              plotInfo$click())
          }
        return(df)
        }
        
      })
      
      
      # 
      # output$massSummary <- renderTable({
      #   df <- df_react()
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
      #   df <- df_react()
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
      #   df <- df_react()
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
      #   df_react() %>%
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

