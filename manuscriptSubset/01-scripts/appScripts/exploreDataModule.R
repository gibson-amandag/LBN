### Explor Data App Module

# https://shiny.rstudio.com/articles/modules.html


# https://gist.github.com/wch/5436415/#gistcomment-1646351 

# https://stackoverflow.com/questions/38822863/shiny-dynamic-colour-fill-input-for-ggplot 
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

exploreDataUI <- function(
  id,
  df
){
  ns <- NS(id)
  tagList(
     fluidRow(
      div(
        class = "col-xs-6",
        varSelectInput(
          ns("xVar"),
          "Select x-axis variable",
          data = df
        ),
        textInput(
          ns("xLab"),
          "x-axis label"
        )
      ),
      div(
        class = "col-xs-6",
        varSelectInput(
          ns("yVar"),
          "Select y-axis variable",
          data = df
        ),
        textInput(
          ns("yLab"),
          "y-axis label"
        )
      ),
    ),
    
    fluidRow(
      div(
        class = "col-xs-6",
        checkboxInput(
          ns("useFillVar"),
          "Use fill variable",
          value = TRUE
        ),
        varSelectInput(
          ns("fillVar"),
          "Select fill variable",
          data = df
        ),
        actionButton(
          ns("resetFill"),
          "reset fill colors"
        )
      ),
      div(
        class = "col-xs-6",
        checkboxInput(
          ns("useLineColorVar"),
          "Use line color variable",
          value = TRUE
        ),
        varSelectInput(
          ns("lineColorVar"),
          "Select line color variable",
          data = df
        ),
        actionButton(
          ns("resetLineColor"),
          "reset line colors"
        )
      )
      #Could potentially add linetype/shape variable options
    ),
    
    fluidRow(
      uiOutput(ns("fillUI"))
    ),
    fluidRow(
      uiOutput(ns("lineColorUI"))
    ),
    
    tabsetPanel(
      tabPanel(
        "Table",
        dataTableOutput(
          ns("dataTable")
        )
      ),
      tabPanel(
        "Graphs",
        
        zoomAxisUI(ns("zoom_x"), "x"),
        
        zoomAxisUI(ns("zoom_y"), "y"),
        
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
            class = "col-xs-4",
            sliderInput(
              ns("dotSize"),
              "dot size",
              min = 0.5,
              max = 4,
              step = 0.5,
              value = 1
            )
          )
        ),
        
        # fluidRow(
        #   div(
        #     class = "col-xs-4",
        #     sliderInput(
        #       ns("indivAlpha"),
        #       "individual dot alpha",
        #       min = 0,
        #       max = 1,
        #       value = 0.5
        #     )
        #   ),
        #   div(
        #     class = "col-xs-4",
        #     sliderInput(
        #       ns("meanAlpha"),
        #       "mean line alpha",
        #       min = 0,
        #       max = 1,
        #       value = 1
        #     )
        #   ),
        #   div(
        #     class = "col-xs-4",
        #     sliderInput(
        #       ns("errorBarAlpha"),
        #       "error bar alpha",
        #       min = 0,
        #       max = 1,
        #       value = 1
        #     )
        #   )
        # ),
        
        tabsetPanel(
          tabPanel(
            "Dot Plot",
            tableOutput(ns("dotPlotInfo")),
            plotUI(ns("dotPlot"))
          )
        )
      )
    )
  )
}


exploreDataServer <- function(
  id,
  df,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ## Filtering -------------------------------------------------------------
      df_react <- reactive(
        df %>%
          filter(
            ! (row_number() %in% input$dataTable_rows_selected)
          )
      )
      
      ## Data Table ------------------------------------------------------------
      output$dataTable <- renderDataTable(
        df
      )
      
      ## Zoom ------------------------------------------------------------------
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 18)
      
      
      dotPlot <- reactive({
        if(input$useFillVar){
          fillVar <- input$fillVar
          fillLevs <- fillVarLevels()
          fillCols <- paste0("c(", paste0("input$fillCol", 1:length(fillLevs), collapse = ", "), ")")
          # print(fillCols)
          fillCols <- eval(parse(text = fillCols))
          # print(fillCols)
        } else {
          fillVar <- expr(NULL)
          fillLevs <- NULL
          fillCols <- NULL
        }
        
        if(input$useLineColorVar){
          lineColorVar <- input$lineColorVar
          lineColorLevs <- lineColorVarLevels()
          lineColorCols <- paste0("c(", paste0("input$lineCol", 1:length(lineColorLevs), collapse = ", "), ")")
          # print(lineColorCols)
          lineColorCols <- eval(parse(text = lineColorCols))
          # print(lineColorCols)
        } else {
          lineColorVar <- expr(NULL)
        }
        
        df %>%
          filter(
            !is.na(!! input$xVar),
            !is.na(!! input$yVar)
          ) %>%
          scatterPlot_general(
            xVar = !! input$xVar,
            xLab = input$xLab,
            yVar = !! input$yVar,
            yLab = input$yLab,
            fillVar = !! fillVar,
            fillLimits = fillLevs,
            fillValues = fillCols,
            lineColorVar = !! lineColorVar,
            lineColorLimits = lineColorLevs,
            lineColorValues = lineColorCols,
            textSize = input$fontSize,
            zoom_x = zoom_x$zoom(), # Zoom to part of x axis
            xmin = zoom_x$min(),
            xmax = zoom_x$max(),
            zoom_y = zoom_y$zoom(), # Zoom to part of y axis
            ymin = zoom_y$min(),
            ymax = zoom_y$max(),
            dotSize = input$dotSize,
            fillAlpha = 1,
            jitterWidth = 0.35,
            jitterHeight = 0,
            title = NULL,
            addMean = TRUE,
            addSE = TRUE
          )
      })
      
      dotPlotInfo <- plotServer("dotPlot", dotPlot, "plot", compType)
      # To access plot click information plotInfo$click()
      # x = plotInfo$click()$x
      
      output$dotPlotInfo <- renderTable({
        
        df <- df_react()
        
        if(input$useFillVar){
          fillVar <- input$fillVar
        } else {
          fillVar <- expr(NULL)
        }
        
        if(input$useLineColorVar){
          lineColorVar <- input$lineColorVar
        } else {
          lineColorVar <- expr(NULL)
        }
        
        nearPoints(
          df %>% 
            select(
              # !! ifelse(input$groupByDam, expr(damID), expr(mouseID)),
              !! input$xVar,
              !! input$yVar,
              !! fillVar,
              !! lineColorVar
            ), 
          dotPlotInfo$click())
      })
      
      ## Colors ----------------------------------------------------------------
      fillVarLevels <- reactive({
        req(df, input$fillVar)
        unique(df[[input$fillVar]])
      })
      lineColorVarLevels <- reactive({
        req(df, input$lineColorVar)
        unique(df[[input$lineColorVar]])
      })
      
      # Create a fillUI if useFillVar is selected
      output$fillUI <- renderUI({
        req(df)
        
        if(input$useFillVar){
          # print(fillVarLevels())
          lev <- fillVarLevels()
          cols <- gg_fill_hue(length(lev))
          
          # New IDs "col+level"
          lapply(seq_along(lev), function(i) {
            div(
              class = "col-xs-4",
              colourpicker::colourInput(inputId = session$ns(paste0("fillCol", i)),
                                        label = paste0("Choose color for ", lev[i]),
                                        value = cols[i]
              )
            )
          })
        }
      })
      
      # Reset to default colors when resetColors is pressed
      observeEvent(input$resetFill, {
        lev <- fillVarLevels()
        cols <- gg_fill_hue(length(lev))
        
        lapply(seq_along(lev), function(i) {
          do.call(what = "updateColourInput",
                  args = list(
                    session = session,
                    inputId = paste0("fillCol", i),
                    value = cols[i]
                  )
          )
        })
      })
      # Create a fillUI if useLineColorVar is selected
      output$lineColorUI <- renderUI({
        req(df)
        
        if(input$useLineColorVar){
          lev <- lineColorVarLevels()
          cols <- gg_fill_hue(length(lev))
          
          # New IDs "col+level"
          lapply(seq_along(lev), function(i) {
            div(
              class = "col-xs-4",
              colourpicker::colourInput(inputId = session$ns(paste0("lineCol", i)),
                                        label = paste0("Choose color for ", lev[i]),
                                        value = cols[i]
              )
            )
          })
        }
      })
      
      # Reset to default colors when resetColors is pressed
      observeEvent(input$resetLineColor, {
        lev <- lineColorVarLevels()
        cols <- gg_fill_hue(length(lev))
        
        lapply(seq_along(lev), function(i) {
          do.call(what = "updateColourInput",
                  args = list(
                    session = session,
                    inputId = paste0("lineCol", i),
                    value = cols[i]
                  )
          )
        })
      })
    }
  )
}

