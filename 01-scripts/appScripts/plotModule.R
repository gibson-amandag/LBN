### Plot display with save options

# Call the service with the reactive version of the plot (no () after plot name)
# The server returns the click information so that this can still bu used in the calling module and customized
# See server return for info about formating to obtain in calling module. Used in massOffModule

# https://shiny.rstudio.com/articles/modules.html

plotUI <- function(
  id,
  height = "400px"
){
  ns <- NS(id)
  tagList(
    plotOutput(
      ns("plot"),
      click = ns("plot_click")
      , height = height
    ),
    fluidRow(
      div(
        class = "col-xs-12 col-sm-3",
        div(
          class = "col-xs-4 col-sm-12",
          downloadButton(
            ns("downloadPlot"),
            "Download Plot"
          )
        ),
        div(
          class = "col-xs-8 col-sm-12",
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
          6
        ),
        numericInput(
          ns("width"),
          "Width:",
          4
        )
      )
    )
    
  )
}


plotServer <- function(
  id,
  plot, # as a reactive value
  plotFileNameBase,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      
      # plot <- eventReactive(plot, plot)
      output$plot <- renderPlot({
        # plot
        plot()
      })
      
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
      
      # Download the group cycles plot
      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste0(plotFileNameBase, "-", Sys.Date(), ".", input$imgType)
        },
        content = function(file) {
          flexSave(
            baseName = file,
            thisFilePrefix = NULL,
            # plot = plot,
            plot = plot(),
            fileType = input$imgType,
            filePath = NULL,
            width = input$width,
            height = input$height,
            units = input$units,
            compType = compType,
            shinySettings = TRUE
          )
        }
      )
      
      # Access by plotInfo <- plotServer(...)
      # plotInfo$click() (must be called in reactive context)
      # Example x value -> plotInfo$click()$x
      return(
        list(
          click = reactive({input$plot_click})
        )
      )
    }
  )
}

