### Offspring Mass App Module

# https://shiny.rstudio.com/articles/modules.html

massOffUI <- function(id,
                          Mass_off){
  ns <- NS(id)
  tagList(
    
    h3("Offspring Mass"),
    
    # Filtering ---------------------------------------------------------------
    filteringDFUI(ns("mass_filter"), Mass_off),
    
    fluidRow(
      div(
        class = "col-xs-4",
        radioButtons(
          ns("whichSex"),
          "Which sex?",
          c("Both", "Male" = "M", "Female" = "F"),
          selected = "Both"
        )
      ),
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("groupByDam"),
          "Group by dam",
          TRUE
        ),
        checkboxInput(
          ns("facetBySex"),
          "Facet by sex",
          TRUE
        )
      )
    ),
    
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
        plotUI(ns("plot"))
      ),
      tabPanel(
        "ANOVA",
        varSelectInput(
          ns("betweenVars"),
          "Select 'between' variables",
          data = Mass_off %>% select(earlyLifeTrt, sex),
          multiple = TRUE
        ),
        uiOutput(
          ns("massANOVA")
        ),
        uiOutput(
          ns("byDayANOVA")
        )
      )
    ),
    
      
  )
}


massOffServer <- function(
  id,
  Mass_off,
  Demo_dam,
  Demo_dam_for_offspring,
  niceNames,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ## Filtering -------------------------------------------------------------
      
      Mass_off_filtered <- filteringDFServer("mass_filter", Mass_off)
      Mass_off_react <- reactive({
        Mass_off_filtered() %>% 
          filter(
            # remove D020 offspring, as small litter (2 pups) 
            # and female didn't survive
            damID != "D020-02" 
          )
      })
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)
      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 18)
      
      sex_names <- c(
        "F"="female",
        "M"="male"
      )
      
      massPlot <- reactive({
        groupVar <- ifelse(input$groupByDam, expr(damID), expr(mouseID))
        
        df <- Mass_off_react()
        if(input$whichSex == "M"){
          df <- df %>%
            filter(sex == "M")
        }else if(input$whichSex == "F"){
          df <- df %>%
            filter(sex == "F")
        }
        
        plot <- df %>%
          plot_mass_lines(
            groupByDam = input$groupByDam,
            facetBySex = input$facetBySex,
            useLineType = input$useLineType, # TRUE/FALSE
            lineTypeVar = earlyLifeTrt,
            lineGroupVar = {{ groupVar }},
            xtitle = "PND", #x axis label
            ytitle = "mass (g)", #y axis label
            title = NULL, # plot title
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
            errorBarWidth = 0,
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
        return(plot)
      })
      
      plotInfo <- plotServer("plot", massPlot, "massPlot", compType)
      # To access plot click information plotInfo$click()
        # x = plotInfo$click()$x
      
      output$plotInfo <- renderTable({
        
        df <- Mass_off_react()
        if(input$whichSex == "M"){
          df <- df %>%
            filter(sex == "M")
        }else if(input$whichSex == "F"){
          df <- df %>%
            filter(sex == "F")
        }
        
        if(input$groupByDam){
          df <- df %>%
            getAvgByDam(bySex = input$facetBySex)
        }
        
        df_long <- df %>%
          makeOffMassLong()
        
        nearPoints(
          df_long %>% select(
            !! ifelse(input$groupByDam, expr(damID), expr(mouseID_spec)),
            # !! ifelse(input$groupByDam, NULL, expr(mouseID_spec)),
            !! ifelse(input$facetBySex, expr(sex), NULL),
            earlyLifeTrt,
            day,
            mass
          ), 
          plotInfo$click())
      })
      
      
      output$massANOVA <- renderUI({
        
        df <- Mass_off_react()
        if(input$whichSex == "M"){
          df <- df %>%
            filter(sex == "M")
        }else if(input$whichSex == "F"){
          df <- df %>%
            filter(sex == "F")
        }
        
        # if(input$groupByDam){
        #   df <- df %>%
        #     getAvgByDam()
        # }
        
        df_long <- df %>%
          makeOffMassLong()
        
        anova <- df_long %>%
          filter(
            day != 4 # don't include day 4, start of paradigm
          ) %>%
          anova_test(
            dv = mass,
            wid = mouseID,
            within = day,
            # between = c(earlyLifeTrt, sex),
            between = c(!!! input$betweenVars),
            type = 3
          ) 
        
        anova$ANOVA%>%
          formatAnova() %>%
          htmltools_value()
      })
      
      output$byDayANOVA <- renderUI({
        df <- Mass_off_react()
        if(input$whichSex == "M"){
          df <- df %>%
            filter(sex == "M")
        }else if(input$whichSex == "F"){
          df <- df %>%
            filter(sex == "F")
        }
        
        # if(input$groupByDam){
        #   df <- df %>%
        #     getAvgByDam()
        # }
        
        df_long <- df %>%
          makeOffMassLong()
        
        req(length(input$betweenVars) > 0)
        
        df_long %>%
          filter(
            day != 4 # don't include day 4, start of paradigm
          ) %>%
          group_by(day) %>%
          anova_test(
            dv = mass,
            wid = mouseID,
            # between = c(earlyLifeTrt, sex),
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
    }
  )
}

