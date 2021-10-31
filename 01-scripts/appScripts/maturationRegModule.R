### Offspring Maturation App Module

# https://shiny.rstudio.com/articles/modules.html

maturationRegUI <- function(
  id,
  Maturation_off,
  LBN_data
){
  ns <- NS(id)
  tagList(
    
    h3("Offspring Maturation"),
    
    filteringDFUI(ns("MaturationOff_filter"), LBN_data),
    
    fluidRow(
      column(
        4, 
        checkboxInput(
          ns("incCon"),
          "Include Controls",
          value = TRUE
        ),
        checkboxInput(
          ns("incLBN"),
          "Include LBN",
          value = TRUE
        ),
        checkboxInput(
          ns("byDam"),
          "Group by Dam",
          value = FALSE
        )
      ),
      column(
        4,
        varSelectInput(
          ns("modelVars"),
          "Select variables for regression",
          data = LBN_data %>% 
            select(
              Litter_size_endPara,
              earlyLifeTrt,
              Mass_P11,
              Mass_P21
            ),
          selected = c(
            "earlyLifeTrt"
          ),
          multiple = TRUE
        )
      ),
      column(
        4,
        varSelectInput(
          ns("varToPlot"),
          "Select maturation variable",
          data = LBN_data %>% 
            select(
              VO_age,
              Estrus_age,
              PreputialSep_age
            ),
          selected = c(
            "VO_age"
          )
        )
      ),
      column(
        12,
        p("Note that for Cohort 3, there is maturation data, but there is not mass data, 
          so any regressions that use mass will not include cohort 3 animals")
      )
    ),
    
    fluidRow(
      column(
        12,
        verbatimTextOutput(ns("regTest"))
      )
    ),
    fluidRow(
      column(
        12,
        varSelectInput(
          ns("xVar"),
          "Select x-axis variable",
          data = LBN_data %>%
            select(
              Litter_size_endPara,
              Mass_P11,
              Mass_P21
            ),
          selected = c("Litter_size_endPara")
        )
      )
    ),
    fluidRow(
      column(
        12,
        # plotOutput(ns("regPlot"))
        plotUI2(ns("regPlot"))
      )
    )
  )
}


maturationRegServer <- function(
  id,
  Maturation_off,
  LBN_data,
  STDFillColor, # as reactive
  LBNFillColor, # as reactive
  STDLineColor, # as reactive
  LBNLineColor, # as reactive
  fontSize, # as reactive
  dotSize, # as reactive
  compType,
  imgType,
  units,
  height,
  width
){
  moduleServer(
    id,
    function(input, output, session) {
      MaturationOff_react <- filteringDFServer("MaturationOff_filter", LBN_data)
      
      MaturationOff_react2 <- reactive({
        if(input$incCon & input$incLBN){
          df <- MaturationOff_react()
        }else if(input$incCon & !input$incLBN){
          df <- MaturationOff_react() %>% filter(earlyLifeTrt == "STD")
        } else if(!input$incCon & input$incLBN){
          df <- MaturationOff_react() %>% filter(earlyLifeTrt == "LBN")
        } else if(!input$incCon && !input$incLBN){
          df <- MaturationOff_react() %>% filter(earlyLifeTrt != "STD") %>% filter(earlyLifeTrt != "LBN")
        }
        
        df <- df %>% filter(
          !is.na(!! input$varToPlot)
        )
        
        if(input$byDam){
          df <- getAvgByDam(df)
        }
        
        return(df)
      })
      
      output$regTest <- renderPrint({
        df <- MaturationOff_react2() %>% select(
          !! input$varToPlot,
          !!! input$modelVars
        )
        
        matModel <- lm(
          df
        )
        
        summary(matModel)
      })
      
      regressionPlot <- reactive({
        MaturationOff_react2() %>%
          ggplot(
            aes(
              x = !! input$xVar, y = !! input$varToPlot,
              color = earlyLifeTrt, fill = earlyLifeTrt
            )
          ) +
          jitterGeom(
            width = 0.2,
            size = dotSize()
          )+
          coord_cartesian(ylim = c(0, NA)) +
          geom_smooth(method='lm', se = FALSE, formula = y ~ x) +
          boxTheme()+
          textTheme(size = fontSize())+
          earlyLifeFill(STDColor = STDFillColor(), LBNColor = LBNFillColor())+
          earlyLifeColor(STDColor = STDLineColor(), LBNColor = LBNLineColor()) + 
          # my_theme +
          theme(legend.position="bottom",
                legend.box="vertical",
                legend.margin=margin()
          )
      })
      
      regPlotInfo <- plotServer2(
        "regPlot",
        regressionPlot,
        "matRegression",
        compType = compType,
        imgType = imgType,
        units = units,
        height = height,
        width = width
      )
      
    }
  )
}