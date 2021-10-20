### Plot display with save options

# Call the service with the reactive version of the plot (no () after plot name)
# The server returns the click information so that this can still bu used in the calling module and customized
# See server return for info about formating to obtain in calling module. Used in massOffModule

# https://shiny.rstudio.com/articles/modules.html

plotUI2 <- function(
  id
){
  ns <- NS(id)
  tagList(
    plotOutput(
      ns("plot"),
      click = ns("plot_click")
    ),
    fluidRow(
      div(
        class = "col-xs-12",
        downloadButton(
          ns("downloadPlot"),
          "Download Plot"
        )
      )
    )
    
  )
}


plotServer2 <- function(
  id,
  plot, # as a reactive value
  plotFileNameBase,
  compType,
  imgType,
  units,
  height,
  width
){
  moduleServer(
    id,
    function(input, output, session) {
      
      # plot <- eventReactive(plot, plot)
      output$plot <- renderPlot({
        # plot
        plot()
      })
      
      # Download the group cycles plot
      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste0(plotFileNameBase, "-", Sys.Date(), ".", imgType())
        },
        content = function(file) {
          flexSave(
            baseName = file,
            thisFilePrefix = NULL,
            # plot = plot,
            plot = plot(),
            fileType = imgType(),
            filePath = NULL,
            width = width(),
            height = height(),
            units = units(),
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

