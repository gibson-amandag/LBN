### Single variable (category plot) and double variable Scatter plots by early-life treatment

# https://shiny.rstudio.com/articles/modules.html
catPlotEarlyLifeTrtUI <- function(
  id,
  dfForVars
){
  ns <- NS(id)
  tagList(
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
        class = "col-xs-4",
        varSelectInput(
          ns("singleVar"),
          "Select variable to summarize",
          dfForVars
        )
      )
    ),
    tableOutput(ns("countTable")),
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


catPlotEarlyLifeTrtServer <- function(
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
            ! (is.na(earlyLifeTrt))
          ) %>%
          scatterPlotLBN(
            yVar = !! input$singleVar,
            yLab = yText(),
            dotSize = dotSize,
            textSize = input$fontSize,
            zoom_y = zoomY$zoom(),
            ymin = zoomY$min(),
            ymax = zoomY$max(),
            STDColor = input$STDColor,
            LBNColor = input$LBNColor
          # ) + 
          # theme(
          #   legend.position = "top"
          )
      })
      
      info <- plotServer("plot", plot, paste0(yVar()), compType)
      
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
              earlyLifeTrt == catLevel,
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
            between = c(earlyLifeTrt),
            type = 3
          ) %>%
          formatAnova() %>%
          htmltools_value()
      })
      
      output$countTable <- renderTable({
        df_filtered() %>%
          filter(
            !is.na(!! input$singleVar)
          ) %>%
          filter(
            ! (row_number() %in% input$table_rows_selected)
          ) %>%
          group_by(earlyLifeTrt) %>%
          meanSummary(!! input$singleVar)
      })
    }
  )
}


scatterPlotEarlyLifeTrtUI <- function(
  id,
  dfForXVars,
  dfForYVars
){
  ns <- NS(id)
  tagList(
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


scatterPlotEarlyLifeTrtServer <- function(
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
            ! (is.na(earlyLifeTrt))
          ) %>%
          scatterPlotTwoVars_byLBN(
            yVar = !! input$yVar,
            yLab = yText(),
            xVar = !! input$xVar,
            xLab = xText(),
            dotSize = dotSize,
            textSize = input$fontSize,
            zoom_x = zoomX$zoom(),
            xmin = zoomX$min(),
            xmax = zoomX$max(),
            zoom_y = zoomY$zoom(),
            ymin = zoomY$min(),
            ymax = zoomY$max(),
            STDColor = input$STDColor,
            LBNColor = input$LBNColor,
            jitterWidth = 0
          )
      })
      
      info <- plotServer("plot", plot, paste0(yText(), "-by-", xText()), compType)
      
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

