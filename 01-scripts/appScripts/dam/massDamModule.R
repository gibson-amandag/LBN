### Dam Mass App Module

# https://shiny.rstudio.com/articles/modules.html

massDamUI <- function(
    id,
    Demo_dam
){
  ns <- NS(id)
  tagList(
    
    h3("Dam Mass"),
    
    # Filtering ---------------------------------------------------------------
    filteringDFUI(ns("mass_filter"), Demo_dam),
    
    tabsetPanel(
      tabPanel(
        "Graphs",
        
        fluidRow(
          div(
            class = "col-xs-4",
            checkboxInput(
              ns("useLineType"),
              "Use different linetypes",
              FALSE
            )
          ),
          div(
            class = "col-xs-4",
            checkboxInput(
              ns("individualLines"),
              "Individual lines",
              TRUE
            )
          ),
          div(
            class = "col-xs-4",
            checkboxInput(
              ns("meanLines"),
              "Mean lines",
              TRUE
            )
          )
        ),
        
        fluidRow(
          div(
            class = "col-xs-4",
            checkboxInput(
              ns("facetByLitter"),
              "Facet by litter",
              TRUE
            )
          ),
          div(
            class = "col-xs-4",
            checkboxInput(
              ns("facetByTrt"),
              "Facet by trt",
              FALSE
            )
          )
        ),
        
        
        
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
            class= "col-xs-4",
            colourpicker::colourInput(
              ns("STDColor"),
              "STD Color",
              "grey30"
            )
          ),
          div(
            class = "col-xs-4",
            colourpicker::colourInput(
              ns("LBNColor"),
              "LBN Color",
              "cyan4"
            )
          )
        ),
        fluidRow(
          div(
            class = "col-xs-4",
            sliderInput(
              ns("indivAlpha"),
              "individual line alpha",
              min = 0,
              max = 1,
              value = 0.5
            )
          ),
          div(
            class = "col-xs-4",
            sliderInput(
              ns("meanAlpha"),
              "mean line alpha",
              min = 0,
              max = 1,
              value = 1
            )
          ),
          div(
            class = "col-xs-4",
            sliderInput(
              ns("errorBarAlpha"),
              "error bar alpha",
              min = 0,
              max = 1,
              value = 1
            )
          )
        ),
        
        # plotOutput(ns("plot"))
        tableOutput(ns("plotInfo")),
        plotUI(ns("plot")),
        
        DTOutput(ns("demoTable"))
      ),
      tabPanel(
        "ANOVA",
        varSelectInput(
          ns("betweenVars"),
          "Select 'between' variables",
          data = Demo_dam %>% select(earlyLifeTrt, litterNum),
          multiple = TRUE
        ),
        h4("Mean summary information"),
        
        tableOutput(ns("massSummary")),
        
        h4("ANOVA"),
        
        uiOutput(
          ns("massANOVA")
        ),
        
        h4("By day ANOVA"),
        
        uiOutput(
          ns("byDayANOVA")
        ),
      )
    ),
    
      
  )
}


massDamServer <- function(
  id,
  Demo_dam,
  niceNames,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ## Filtering -------------------------------------------------------------
      
      Demo_dam_filtered <- filteringDFServer("mass_filter", Demo_dam)
      Demo_dam_react <- reactive({
        Demo_dam_filtered() %>% 
          filter(
            # remove D020 offspring, as small litter (2 pups) 
            # and female didn't survive
            damID != "D020-02" 
          )
      })
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 35)
      
      massPlot <- reactive({
        df <- Demo_dam_react()
        
        plot <- df %>%
          plot_dam_mass_lines(
            useLineType = input$useLineType, # TRUE/FALSE
            lineTypeVar = earlyLifeTrt,
            individualLines = input$individualLines, # plot individual lines
            meanLines = input$meanLines, # plot mean lines with SE
            zoom_x = zoom_x$zoom(), # Zoom to part of x axis
            xmin = zoom_x$min(),
            xmax = zoom_x$max(),
            zoom_y = zoom_y$zoom(), # Zoom to part of y axis
            ymin = zoom_y$min(),
            ymax = zoom_y$max(),
            indivLineAlpha = input$indivAlpha,
            indivLineSize = 0.8,
            errorBarWidth = 1,
            meanLineSize = 1.4,
            meanAlpha = input$meanAlpha,
            errorBarSize = 1,
            # errorBarColor = "grey10",
            errorBarAlpha = input$errorBarAlpha,
            textSize = input$fontSize,
            axisSize = 0.5,
            legendPosition = "top",
            STDColor = input$STDColor,
            LBNColor = input$LBNColor
          )
        
        if(input$facetByLitter){
          if(input$facetByTrt){
            plot <- plot + 
              facet_wrap(
                vars(earlyLifeTrt, litterNum),
                nrow = 1,
                labeller = labeller(litterNum = c("1" = "first", "2" = "second"))
              )
          } else {
            plot <- plot + 
              facet_wrap(
                vars(litterNum),
                labeller = labeller(litterNum = c("1" = "first", "2" = "second"))
              )
          }
            
        } else if(input$facetByTrt){
          plot <- plot + 
            facet_wrap(
              vars(earlyLifeTrt)
            )
        }
        return(plot)
      })
      
      plotInfo <- plotServer("plot", massPlot, "massPlot", compType)
      # To access plot click information plotInfo$click()
        # x = plotInfo$click()$x
      
      output$plotInfo <- renderTable({
        
        df <- Demo_dam_react()
        
        df_long <- df %>%
          makeDamMassLong()
        
        nearPoints(
          df_long %>% select(
            damID,
            earlyLifeTrt,
            litterNum,
            day,
            mass
          ), 
          plotInfo$click())
      })
      
      output$massSummary <- renderTable({
        df <- Demo_dam_react()
        
        df_long <- df %>%
          makeDamMassLong()
        
        df_long %>%
          group_by(
            day,
            !!! input$betweenVars
          ) %>%
          meanSummary(mass)
      })
      
      output$massANOVA <- renderUI({
        
        df <- Demo_dam_react()
        
        df_long <- df %>%
          makeDamMassLong()
        
        anova <- df_long %>%
          anova_test(
            dv = mass,
            wid = damID,
            within = day,
            between = c(!!! input$betweenVars),
            type = 3
          ) 
        
        anova$ANOVA%>%
          formatAnova() %>%
          htmltools_value()
      })
      
      output$byDayANOVA <- renderUI({
        df <- Demo_dam_react()
        
        df_long <- df %>%
          makeDamMassLong()
        
        req(length(input$betweenVars) > 0)
        
        df_long %>%
          group_by(day) %>%
          anova_test(
            dv = mass,
            wid = damID,
            between = c(!!! input$betweenVars),
            type = 3
          ) %>%
          adjust_pvalue(method = "bonferroni")%>%
          formatAdjAnova() %>%
          # colformat_num(
          #   j = "p.adj",
          #   digits = 5
          # ) %>%
          htmltools_value()
      })
      
      output$demoTable <- renderDT({
        Demo_dam_react() %>%
          select(
            damID,
            earlyLifeTrt,
            litterNum,
            Dam_Mass_P4,
            Dam_Mass_P11,
            Dam_Mass_P21
          )
      })
    }
  )
}

