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
      ),
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("facetByLitter"),
          "Facet by litter",
          TRUE
        )
      )
    ),
    h4("Mean summary"),
    tableOutput(ns("countTable")),
    h4("ANOVA"),
    uiOutput(ns("ANOVA")),
    zoomAxisUI(ns("zoomY"), "y"),
    h4("Click on a point to get info"),
    tableOutput(
      ns("info")
    ),
    plotUI(
      ns("plot")
    ),
    shiny::dataTableOutput(ns("sumTable")),
    h4("Click on a row in the table to exclude from plot"),
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
        plot <- df_filtered() %>%
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
        
        if(input$facetByLitter){
          plot <- plot + 
            facet_wrap(
              vars(litterNum)
              , ncol = 2
              , labeller = labeller(
                litterNum = c("1" = "first litter", "2" = "second litter")
              )
            )
        }
        return(plot)
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
        if(input$facetByLitter){
          tbl <- df_filtered() %>%
            filter(
              ! is.na(!! input$singleVar)
            ) %>%
            filter(
              ! (row_number() %in% input$table_rows_selected)
            ) %>%
            anova_test(
              dv = !! input$singleVar,
              between = c(earlyLifeTrt, litterNum),
              type = 3
            ) %>%
            formatAnova() %>%
            htmltools_value(ft.align = "left")
        } else {
          tbl <- df_filtered() %>%
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
            htmltools_value(ft.align = "left")
        }
        
        return(tbl)
      })
      
      output$countTable <- renderTable({
        # it works to do if(input$facetByLitter) litterNum in the group_by,
        # but then the if statement shows up in the table itself
        if(input$facetByLitter){
          meanDF <- df_filtered() %>%
            filter(
              !is.na(!! input$singleVar)
            ) %>%
            filter(
              ! (row_number() %in% input$table_rows_selected)
            ) %>%
            group_by(
              earlyLifeTrt
              , litterNum
            ) %>%
            meanSummary(!! input$singleVar)
        } else {
          meanDF <- df_filtered() %>%
            filter(
              !is.na(!! input$singleVar)
            ) %>%
            filter(
              ! (row_number() %in% input$table_rows_selected)
            ) %>%
            group_by(
              earlyLifeTrt
            ) %>%
            meanSummary(!! input$singleVar)
          
        }
        return(meanDF)
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
      ),
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("facetByLitter"),
          "Facet by litter",
          TRUE
        )
      )
    ),
    h4("ANOVA"),
    uiOutput(ns("ANOVA")),
    zoomAxisUI(ns("zoomX"), "x"),
    zoomAxisUI(ns("zoomY"), "y"),
    h4("Click on a point to get info"),
    tableOutput(
      ns("info")
    ),
    plotUI(
      ns("plot")
    ),
    h4("Click on a row in the table to exclude from plot"),
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
        plot <- df_filtered() %>%
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
        if(input$facetByLitter){
          plot <- plot + 
            facet_wrap(
              vars(litterNum)
              , ncol = 2
              , labeller = labeller(
                litterNum = c("1" = "first litter", "2" = "second litter")
              )
            )
        }
        return(plot)
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

