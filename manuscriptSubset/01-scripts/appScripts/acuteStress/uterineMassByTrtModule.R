### Acute Stress App Module

# https://shiny.rstudio.com/articles/modules.html

uterineMassByTrtUI <- function(id,
                          AcuteStress_off){
  ns <- NS(id)
  tagList(
    plotOutput(
      ns("uterineMassByTrt"),
      click = ns("uterineMassByTrt_click")
    ),
    verbatimTextOutput(
      ns("uterineMassByTrt_infoText")
    ),
    shiny::dataTableOutput(ns("uterineMassByTrt_info")),
    p("Click on a row in the table to exclude from plot"),
    DTOutput(ns("uterineMassByTrt_table"))
  )
}


uterineMassByTrtServer <- function(
  id,
  df,
  proUterineCutoff,
  diUterineCutoff,
  dotSize
){
  moduleServer(
    id,
    function(input, output, session) {
      
      plot <- reactive({
        plot <- df %>%
          filter(
            ! (row_number() %in% input$uterineMassByTrt_table_rows_selected)
          ) %>%
          plotUterineMassByGroup(
            hLineVal = proUterineCutoff,
            fontSize = 16,
            dotSize = dotSize
          ) +
          geom_hline(
            yintercept = diUterineCutoff,
            color = "blue"
          )
        return(plot)
      })
      
      output$uterineMassByTrt <- renderPlot({
        plot()
      })
      
      output$uterineMassByTrt_infoText <- renderPrint({
        if(is.null(input$uterineMassByTrt_click$y)){
          "Click on a point to display values"
        }else(
          paste0(
            "Looking for values between ", #one binwidth +/- click y value
            round(input$uterineMassByTrt_click$y - 2, 3),
            " and ",
            round(input$uterineMassByTrt_click$y + 2, 3)
          )
        )
      })
      
      output$uterineMassByTrt_info <- shiny::renderDataTable({
        if(!is.null(input$uterineMassByTrt_click$y)){
          df %>%
            filter(
              ! (row_number() %in% input$uterineMassByTrt_table_rows_selected),
              near(ReproTract_mass, input$uterineMassByTrt_click$y, tol = 2)
            ) %>%
            select(
              mouseID,
              num_ID,
              earlyLifeTrt,
              adultTrt,
              Sac_cycle,
              ReproTract_mass,
              maxLH
            )
        }
      })
      
      output$uterineMassByTrt_table <- renderDT({
        df %>%
          select(
            mouseID,
            num_ID,
            earlyLifeTrt,
            adultTrt,
            Sac_cycle,
            ReproTract_mass
          )
      })
      
    }
  )
}

