### Tasking Tracking App Module

# https://shiny.rstudio.com/articles/modules.html

rawDataUI <- function(id,
                      Demo_dam,
                      LBN_data){
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("LBN Data Frames"),
      
      #Dam Data
      h2("Dam Data"),
      varSelectInput(ns("dam_vars_include"),
                     label = "Select Column Variables",
                     data = Demo_dam,
                     selected = c("Dam_ID", 
                                  "Treatment",
                                  "Breed_date",
                                  "Plug_date",
                                  "DOB"),
                     multiple = TRUE),
      dataTableOutput(ns("Demo_dam")),
      
      #Offspring Data
      h2("Offspring Data"),
      tabsetPanel(
        tabPanel("Offspring Demographics",
                 h3("Offspring Demographics"),
                 dataTableOutput(ns("Demo_off"))),
        tabPanel("Offspring Mass",
                 h3("Offspring Mass"),
                 dataTableOutput(ns("Mass_off"))),
        tabPanel("Offspring Maturation",
                 h3("Offspring Maturation"),
                 dataTableOutput(ns("Maturation_off"))),
        tabPanel("After Paradigm",
                 h3("After Paradigm"),
                 dataTableOutput(ns("EndPara_off"))),
        tabPanel("Cycles",
                 h3("Offspring Cycles"),
                 dataTableOutput(ns("Cycles_off"))),
        tabPanel("Acute Stress",
                 h3("Acute Stress"),
                 dataTableOutput(ns("AcuteStress_off"))),
        tabPanel("Chronic Stress",
                 h3("Chronic Stress"),
                 dataTableOutput(ns("ChronicStress_off")))
      ),
      
      
      h3("Combined Offspring Data"),
      varSelectInput(ns("offspring_vars_include"),
                     label = "Select Column Variables",
                     data = LBN_data,
                     selected = c("Mouse_ID",
                                  "Dam_ID",
                                  "Treatment",
                                  "DOB"),
                     multiple = TRUE),
      dataTableOutput(ns("LBN_data"))
    )
  )
}


rawDataServer <- function(id,
                          Demo_dam,
                          Demo_off,
                          Mass_off,
                          Maturation_off,
                          EndPara_off,
                          Cycles_off,
                          AcuteStress_off,
                          ChronicStress_off,
                          LBN_data){
  moduleServer(
    id,
    function(input, output, session) {
      
      output$Demo_dam <- renderDataTable(
        Demo_dam %>%
          select(!!! input$dam_vars_include),
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
      output$Demo_off <- renderDataTable(
        Demo_off,
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
      output$Mass_off <- renderDataTable(
        Mass_off,
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
      output$Maturation_off <- renderDataTable(
        Maturation_off,
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
      output$EndPara_off <- renderDataTable(
        EndPara_off,
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
      output$Cycles_off <- renderDataTable(
        Cycles_off,
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
      output$AcuteStress_off <- renderDataTable(
        AcuteStress_off,
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
      output$ChronicStress_off <- renderDataTable(
        ChronicStress_off,
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
      
      output$LBN_data <- renderDataTable(
        LBN_data %>%
          select(!!! input$offspring_vars_include),
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 10)
      )
      
    }
  )
}

