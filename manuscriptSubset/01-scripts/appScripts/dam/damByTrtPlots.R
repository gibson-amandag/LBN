### Dam Mass App Module

# https://shiny.rstudio.com/articles/modules.html

damByTrtUI <- function(
    id,
    damFramesAndBehaviorByDam
){
  ns <- NS(id)
  tagList(
    
    h3("Dam Information"),
    
    # Filtering ---------------------------------------------------------------
    filteringDFUI(ns("damTrtFilter"), damFramesAndBehaviorByDam),
    
    fluidRow(
      div(
        class = "col-xs-4",
        numericInput(
          ns("dotSize"),
          "Dot size",
          value = 3
        )
      )
    ),
    
    tabsetPanel(
      ## Single Variable -----------------------------------------------------------
      tabPanel(
        "Single Variable",
        catPlotEarlyLifeTrtUI(
          ns("singleVar"),
          damFramesAndBehaviorByDam %>%
            select(
              Num_exits:clump8_percLitter
              , Litter_size
              , Avg_litter_mass_startPara
              , Cort_dam_P11
              , Cort_dam_P21
            )
        )
      ),
      
      ## Two-variable scatter --------------------------------------------------
      tabPanel(
        "Two Variable",
        scatterPlotEarlyLifeTrtUI(
          ns("twoVar"),
          damFramesAndBehaviorByDam %>%
            select(
              Num_exits:clump8_percLitter
              , Litter_size
              , Avg_litter_mass_startPara
              , Cort_dam_P11
              , Cort_dam_P21
            ),
          damFramesAndBehaviorByDam %>%
            select(
              Num_exits:clump8_percLitter
              , Litter_size
              , Avg_litter_mass_startPara
              , Cort_dam_P11
              , Cort_dam_P21
            )
        )
      ),
    )
  )
}


damByTrtServer <- function(
    id,
    damFramesAndBehaviorByDam,
    niceNames,
    compType
){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ## Filtering -------------------------------------------------------------
      
      Demo_dam_filtered <- filteringDFServer("damTrtFilter", damFramesAndBehaviorByDam)
      Demo_dam_react <- reactive({
        Demo_dam_filtered() %>% 
          filter(
            # remove D020 offspring, as small litter (2 pups) 
            # and female didn't survive
            damID != "D020-02" 
          )
      })
      # zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      # zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 35)
      
      ## Single Var -----------------------------------------------------------------------
      
      observeEvent(
        c(Demo_dam_react(),
          input$dotSize),
        {
          catPlotEarlyLifeTrtServer(
            "singleVar",
            Demo_dam_react(),
            getNiceName,
            c(
              !!! exprs(
                damID,
                earlyLifeTrt,
                cohort,
                litterNum,
                Litter_size
              )
            ),
            dotSize = input$dotSize,
            compType = compType
          )
        }
      )
      
      ## Two variable ----------------------------------------------------------
      observeEvent(
        c(Demo_dam_react(),
          input$dotSize),
        {
          scatterPlotEarlyLifeTrtServer(
            "twoVar",
            Demo_dam_react(),
            getNiceName,
            c(
              !!! exprs(
                damID,
                earlyLifeTrt,
                cohort,
                litterNum,
                Litter_size
              )
            ),
            dotSize = input$dotSize,
            compType = compType
          )
        }
      )
    }
  )
}

