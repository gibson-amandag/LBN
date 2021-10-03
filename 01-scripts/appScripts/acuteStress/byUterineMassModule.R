### Acute Stress App Module

# https://shiny.rstudio.com/articles/modules.html

byUterineMassUI <- function(id,
                          AcuteStress_off){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      div(
        class = "col-xs-4",
        varSelectInput(
          ns("uterineMass_yVar"),
          "Select y-variable",
          AcuteStress_off %>%
            select(
              maxLH,
              starts_with("cort_hr"),
              starts_with("LH_hr")
            )
        ),
        filterLHUI(ns("filterLH")),
      ),
      div(
        class = "col-xs-4",
        selectInput(
          ns("earlyLifeTrt"),
          "Which early-life treatment groups?",
          choices = unique(AcuteStress_off$earlyLifeTrt), # Changed from levels to unique
          multiple = TRUE,
          selected = unique(AcuteStress_off$earlyLifeTrt)
        )
      ),
      div(
        class = "col-xs-4",
        selectInput(
          ns("adultTrt"),
          "Which adult treatment groups?",
          choices = unique(AcuteStress_off$adultTrt),
          multiple = TRUE,
          selected = unique(AcuteStress_off$adultTrt),
        )
      )
    ),
    zoomAxisUI(ns("zoom_x"), "x"),
    zoomAxisUI(ns("zoom_y"), "y"),
    plotOutput(
      ns("plotByUterineMass"),
      click = ns("plotByUterineMass_click")
    ),
    verbatimTextOutput(
      ns("plotByUterineMass_info")
    ),
    p("Click on a row in the table to exclude from plot"),
    DTOutput(ns("plotByUterineMass_table"))
  )
}


byUterineMassServer <- function(
  id,
  df,
  proUterineCutoff,
  diUterineCutoff,
  dotSize
){
  moduleServer(
    id,
    function(input, output, session) {
      
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 200)
      zoom_y <- zoomAxisServer(
        "zoom_y", "y",
        minVal = 0,
        maxVal = max(df_react()[[input$uterineMass_yVar]], na.rm = TRUE)
      )
      
      df_react <- reactive({
        
        df <- df %>%
          filter(
            !is.na(!! input$uterineMass_yVar),
            earlyLifeTrt %in% as.character(input$earlyLifeTrt),
            adultTrt %in% as.character(input$adultTrt),
            !exclude_cort_hr5
          )
        
        filterLH <- LHfilter()$useFilter()
        minLH <- LHfilter()$surgeMin()
        
        if(!is.null(filterLH) & !is.null(minLH)){
          if(filterLH & !is.na(minLH)){
          df <- df %>%
            filter(
              maxLH >= minLH
            )
          }
        }
        return(df)
      })
      
      
      LHfilter <- reactiveVal()
      observeEvent(input$uterineMass_yVar, {
        LHfilter(filterLHServer("filterLH", minVal = 3, display = (input$uterineMass_yVar == "maxLH")))
      })
      
      # observeEvent(LHfilter()$surgeMin(), print(LHfilter()$useFilter()))
      
      plot <- reactive({
        req(df_react())
        if(input$uterineMass_yVar == "maxLH"){
          yLabText <- "max evening LH (ng/mL)"
        }else if(grepl("^cort_", input$uterineMass_yVar)){
          yLabText <- "corticosterone (ng/mL)"
        }else if(grepl("^LH_", input$uterineMass_yVar)){
          yLabText <- "LH (ng/mL)"
        }
        
        plot <- df_react() %>%
          filter(
            ! (row_number() %in% input$plotByUterineMass_table_rows_selected),
          ) %>%
          plotByUterineMass(
            yVar = !! input$uterineMass_yVar,
            yLab = yLabText,
            fontSize = 16,
            dotSize = dotSize,
            zoom_x = zoom_x$zoom(),
            xmin = zoom_x$min(),
            xmax = zoom_x$max(),
            zoom_y = zoom_y$zoom(),
            ymin = zoom_y$min(),
            ymax = zoom_y$max()
          ) +
          geom_vline(
            xintercept = proUterineCutoff,
            color = "red"
          ) +
          geom_vline(
            xintercept = diUterineCutoff,
            color = "blue"
          )
          
        return(plot)
      })
      
      output$plotByUterineMass <- renderPlot({
        plot()
      })
      
      output$plotByUterineMass_info <- renderPrint({
        if(is.null(input$plotByUterineMass_click)){
          "Click on a point to display values"
        }else(
          nearPoints(
            df_react() %>%
              filter(
                ! (row_number() %in% input$plotByUterineMass_table_rows_selected)
              ) %>%
              select(
                mouseID,
                num_ID,
                earlyLifeTrt,
                adultTrt,
                ReproTract_mass,
                !! input$uterineMass_yVar
              ),
            input$plotByUterineMass_click
          )
        )
      })
      
      output$plotByUterineMass_table <- renderDT({
        df_react() %>%
          select(
            mouseID,
            num_ID,
            earlyLifeTrt,
            adultTrt,
            ReproTract_mass,
            !! input$uterineMass_yVar
          )
      })
    }
  )
}

