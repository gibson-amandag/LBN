#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# # source("./01-scripts/01-set-up.R")
# # source(file.path(scriptsFolder, "02-get-datasets.R"))
# # # library(shinyFiles)
# # 
# # moduleFiles <- list.files(
# #   appScriptsFolder, 
# #   full.names = TRUE,
# #   recursive = TRUE, 
# #   pattern = "*.R"
# # )
# 
# # sapply(moduleFiles, source)


# Define UI for application
ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  actionButton("btn", "Click me")
)

############# SERVER #########################################################
server <-  function(input, output) {
  observeEvent(input$btn, {
    # Run JS code that simply shows a message
    runjs("var today = new Date(); alert(today);")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
