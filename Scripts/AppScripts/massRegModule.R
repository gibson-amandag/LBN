### Offspring Mass App Module

# https://shiny.rstudio.com/articles/modules.html

massRegUI <- function(
  id,
  LBN_data
){
  ns <- NS(id)
  tagList(
    
    h3("Offspring Mass"),
    
    filteringDFUI(ns("MassOff_filter"), LBN_data),
    
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
              Treatment,
              Litter_size_endPara
            ),
          selected = c(
            "Treatment"
          ),
          multiple = TRUE
        )
      ),
      column(
        4,
        varSelectInput(
          ns("varToPlot"),
          "Select mass variable",
          data = LBN_data %>% 
            select(
              Mass_P11:Mass_P24
            ),
          selected = c(
            "Mass_P11"
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
        verbatimTextOutput(ns("regTest2"))
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
        plotOutput(ns("regPlot"))
      )
    )
  )
}


massRegServer <- function(
  id,
  LBN_data
){
  moduleServer(
    id,
    function(input, output, session) {
      MassOff_react <- filteringDFServer("MassOff_filter", LBN_data)
      
      MassOff_react2 <- reactive({
        if(input$incCon & input$incLBN){
          df <- MassOff_react()
        }else if(input$incCon & !input$incLBN){
          df <- MassOff_react() %>% filter(Treatment == "Control")
        } else if(!input$incCon & input$incLBN){
          df <- MassOff_react() %>% filter(Treatment == "LBN")
        } else if(!input$incCon && !input$incLBN){
          df <- MassOff_react() %>% filter(Treatment != "Control") %>% filter(Treatment != "LBN")
        }

        df <- df %>% filter(
          !is.na(!! input$varToPlot)
        )

        if(input$byDam){
          df <- getAvgByDam(df)
        }

        return(df)
      })
      
      output$regTest2 <- renderPrint({
        df <- MassOff_react2() %>% select(
          !! input$varToPlot,
          !!! input$modelVars
        )

        matModel <- lm(
          df
        )

        summary(matModel)
      })
      
      output$regPlot <- renderPlot({
        MassOff_react2() %>%
          ggplot(
            aes(
              x = !! input$xVar, y = !! input$varToPlot,
              color = Treatment,
            )
          ) +
          geom_jitter(
            alpha = 0.6,
            width = 0.2,
            size = 2
            # position = position_jitterdodge(jitter.width = .2)
          )+
          coord_cartesian(ylim = c(0, NA)) +
          geom_smooth(method='lm', se = FALSE) +
          my_theme +
          theme(legend.position="bottom",
                legend.box="vertical",
                legend.margin=margin()
          )
      })
      
    }
  )
}

