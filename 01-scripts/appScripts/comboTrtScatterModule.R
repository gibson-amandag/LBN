### Single variable (category plot) and double variable Scatter plots by combo treatment

# https://shiny.rstudio.com/articles/modules.html
catPlotComboTrtUI <- function(
  id,
  dfForVars
){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        class = "col-xs-4",
        varSelectInput(
          ns("singleVar"),
          "Select variable to summarize",
          dfForVars
        )
      )
    ),
    uiOutput(ns("ANOVA")),
    zoomAxisUI(ns("zoomY"), "y"),
    plotUI(
      ns("plot")
    ),
    p("Click on a point to get info"),
    tableOutput(
      ns("info")
    ),
    shiny::dataTableOutput(ns("sumTable")),
    p("Click on a row in the table to exclude from plot"),
    DTOutput(ns("table"))
  )
}


catPlotComboTrtServer <- function(
  id,
  df_filtered_orig,
  yVarCaseFunc, # a case_when function for processing the label for the yvar
    # Example:
    # caseFunc <- function(colName){
    #   case_when(
    #     colName == "frequency" ~ "frequency (Hz)",
    #     colName == "relPeak" ~ "relative amplitude (pA)",
    #     TRUE ~ as.character(colName)
    #   )
    # }
    # parameter: caseFunc
    ## use getNiceNames instead
  varsForTable,
    ## Example:
    # c(
    #   !!! exprs(
    #     cellID,
    #     mouseID,
    #     earlyLifeTrt,
    #     adultTrt,
    #     comboTrt,
    #     ReproTract_mass,
    #     Rseries,
    #     Rinput,
    #     capacitance,
    #     holdingCurrent
    #   )
    # )
  dotSize,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      
      df_filtered <- reactive(df_filtered_orig)
      
      ## Single Var -----------------------------------------------------------------------
      zoomY <- zoomAxisServer("zoomY", "y", minVal = 0, maxVal = 50)
      
      yVar <- reactive(as.character(input$singleVar))
      yText <- reactive(yVarCaseFunc(yVar()))
      
      plot <- reactive({
        df_filtered() %>%
          filter(
            ! is.na(!! input$singleVar)
          ) %>%
          filter(
            ! (row_number() %in% input$table_rows_selected),
            ! (is.na(adultTrt)) # added 2022-03-14
          ) %>%
          scatterPlotComboTrt(
            yVar = !! input$singleVar,
            yLab = yText(),
            dotSize = dotSize,
            fontSize = 16,
            zoom_y = zoomY$zoom(),
            ymin = zoomY$min(),
            ymax = zoomY$max()
          # ) + 
          # theme(
          #   legend.position = "top"
          )
      })
      
      info <- plotServer("plot", plot, paste0(yText(), Sys.Date()), compType)
      
      output$info <- renderTable({
        if(!is.null(info$click())){
          x <- info$click()$x
          xRound <- round(x)
          y <- info$click()$y
          catLevel <- info$click()$domain$discrete_limits$x[[xRound]]
          
          df_filtered2 <- reactive({df_filtered() %>%
            filter(
              ! is.na(!! input$singleVar)
            ) %>%
            filter(
              ! (row_number() %in% input$table_rows_selected)
            )
          })
          
          if(zoomY$zoom()){
            yRange = zoomY$max() - zoomY$min()
          } else {
            yMin <- min(df_filtered2() %>% select(!! input$singleVar), na.rm = TRUE)
            yMax <- max(df_filtered2() %>% select(!! input$singleVar), na.rm = TRUE)
            yRange <- yMax - yMin
          }
          
          yError <- yRange * 0.03
          
          df_filtered2() %>%
            filter(
              comboTrt == catLevel,
              !! input$singleVar <= y + yError & !! input$singleVar >= y - yError
            ) %>%
            select(
              {{ varsForTable }},
              !! input$singleVar
            )
        }
      })
      
      output$table <- renderDT({
        df_filtered() %>%
          filter(
            ! is.na(!! input$singleVar)
          ) %>%
          select(
            {{ varsForTable }}
            , !! input$singleVar
          )
      })
      
      output$ANOVA <- renderUI({
        df_filtered() %>%
          filter(
            ! is.na(!! input$singleVar)
          ) %>%
          filter(
            ! (row_number() %in% input$table_rows_selected)
          ) %>%
          anova_test(
            dv = !! input$singleVar,
            between = c(earlyLifeTrt, adultTrt),
            type = 3
          ) %>%
          formatAnova() %>%
          htmltools_value()
      })
      
      output$countTable <- shiny::renderDataTable({
        countTbl <-  df_filtered() %>%
          filter(
            ! (row_number() %in% input$GABApscsScatterTable_rows_selected)
          ) %>%
          countMiceAndLitters(
            !! input$singleVar,
            c(expr(earlyLifeTrt), expr(adultTrt))
          )
        
        meanTbl <- df_filtered() %>%
          filter(
            !is.na(!! input$singleVar)
          ) %>%
          filter(
            ! (row_number() %in% input$table_rows_selected)
          ) %>%
          group_by(earlyLifeTrt, adultTrt) %>%
          meanSummary(!! input$singleVar)
        
        countTbl %>%
          left_join(
            meanTbl %>% select(-n),
            by = c("earlyLifeTrt", "adultTrt")
          )
      })
    }
  )
}


scatterPlotComboTrtUI <- function(
  id,
  dfForXVars,
  dfForYVars
){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        class = "col-xs-4",
        varSelectInput(
          ns("xVar"),
          "Select x-variable",
          dfForXVars
        )
      ),
      div(
        class = "col-xs-4",
        varSelectInput(
          ns("yVar"),
          "Select y-variable",
          dfForYVars
        )
      )
    ),
    uiOutput(ns("ANOVA")),
    zoomAxisUI(ns("zoomX"), "x"),
    zoomAxisUI(ns("zoomY"), "y"),
    plotUI(
      ns("plot")
    ),
    p("Click on a point to get info"),
    tableOutput(
      ns("info")
    ),
    p("Click on a row in the table to exclude from plot"),
    DTOutput(ns("table"))
  )
}


scatterPlotComboTrtServer <- function(
  id,
  df_filtered_orig,
  varCaseFunc, # a case_when function for processing the label for the yvar
    # Example:
    # caseFunc <- function(colName){
    #   case_when(
    #     colName == "frequency" ~ "frequency (Hz)",
    #     colName == "relPeak" ~ "relative amplitude (pA)",
    #     TRUE ~ as.character(colName)
    #   )
    # }
    # parameter: caseFunc
  varsForTable,
    ## Example:
    # c(
    #   !!! exprs(
    #     cellID,
    #     mouseID,
    #     earlyLifeTrt,
    #     adultTrt,
    #     comboTrt,
    #     ReproTract_mass,
    #     Rseries,
    #     Rinput,
    #     capacitance,
    #     holdingCurrent
    #   )
    # )
  dotSize,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      
      df_filtered <- reactive(df_filtered_orig)
      
      zoomX <- zoomAxisServer("zoomX", "x", minVal = 0, maxVal = 50)
      zoomY <- zoomAxisServer("zoomY", "y", minVal = 0, maxVal = 50)
      
      xVar <- reactive(as.character(input$xVar))
      xText <- reactive(varCaseFunc(xVar()))
      yVar <- reactive(as.character(input$yVar))
      yText <- reactive(varCaseFunc(yVar()))
      
      plot <- reactive({
        df_filtered() %>%
          filter(
            ! is.na(!! input$xVar),
            ! is.na(!! input$yVar)
          ) %>%
          filter(
            ! (row_number() %in% input$table_rows_selected),
            ! (is.na(adultTrt)) # added 2022-03-14
          ) %>%
          scatterPlotTwoVars_byComboTrt(
            yVar = !! input$yVar,
            yLab = yText(),
            xVar = !! input$xVar,
            xLab = xText(),
            dotSize = dotSize,
            fontSize = 16,
            zoom_x = zoomX$zoom(),
            xmin = zoomX$min(),
            xmax = zoomX$max(),
            zoom_y = zoomY$zoom(),
            ymin = zoomY$min(),
            ymax = zoomY$max()
          )
      })
      
      info <- plotServer("plot", plot, paste0(yText(), "-by-", xText(), Sys.Date()), compType)
      
      output$info <- renderTable({
        if(!is.null(info$click())){
          nearPoints(
            df_filtered() %>%
              filter(
                ! is.na(!! input$xVar),
                ! is.na(!! input$yVar)
              ) %>%
              filter(
                ! (row_number() %in% input$table_rows_selected)
              ) %>%
              select(
                {{ varsForTable }},
                !! input$xVar,
                !! input$yVar
              ),
            info$click()
          )
        }
      })
      
      output$table <- renderDT({
        df_filtered() %>%
          filter(
            ! is.na(!! input$xVar),
            ! is.na(!! input$yVar)
          ) %>%
          select(
            {{ varsForTable }}
            , !! input$xVar
            , !! input$yVar
          )
      })
    }
  )
}

