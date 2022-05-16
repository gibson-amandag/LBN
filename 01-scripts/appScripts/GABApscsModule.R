### Acute Stress App Module

# https://shiny.rstudio.com/articles/modules.html



GABApscsUI <- function(
  id,
  GABApscs
){
  ns <- NS(id)
  tagList(

    h3("GABA PSCs"),
    
  # Filtering ---------------------------------------------------------------
    filteringDFUI(ns("GABA_filter"), GABApscs),

    fluidRow(
      div(
        class = "col-xs-4",
        numericInput(
          ns("proUterineCutoff"),
          "Uterine mass (mg) min for proestrus:",
          value = 125
        ),
        numericInput(
          ns("diUterineCutoff"),
          "Uterine mass (mg) max for diestrus:",
          value = 100
        )
      ),
      div(
        class = "col-xs-4",
        radioButtons(
          ns("cycleStage"),
          "Which stages?",
          choices = c("Proestrus", "Diestrus", "All"),
          selected = "Proestrus"
        ),
        checkboxInput(
          ns("useMass"),
          "Filter by mass",
          value = TRUE
        ),
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("dotSize"),
          "Dot size",
          value = 2
        )
      )
    ),
  
    fluidRow(
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("filterByFreq"),
          "Only include cells with a PSC frequency?",
          value = TRUE
        )
      ),
      div(
        class = "col-xs-4",
        
      ),
      div(
        class = "col-xs-4",
        
      )
    ),
    fluidRow(
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("filterByRseries"),
          "Filter by series resistance?",
          value = FALSE
        )
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("RseriesMin"),
          "min Rseries (MOhm):",
          value = 0
        )
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("RseriesMax"),
          "max Rseries (MOhm):",
          value = 20
        )
      )
    ),
    fluidRow(
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("filterByRinput"),
          "Filter by input resistance?",
          value = FALSE
        )
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("RinputMin"),
          "min Rinput (MOhm):",
          value = 500
        ),
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("RinputMax"),
          "max Rinput (MOhm):",
          value = 1500
        ),
      )
    ),
    fluidRow(
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("filterByHoldingCurr"),
          "Filter by holding current?",
          value = FALSE
        )
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("holdingCurrMin"),
          "min current (pA):",
          value = -50
        ),
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("holdingCurrMax"),
          "max current (pA):",
          value = 10
        ),
      )
    ),
    fluidRow(
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("filterByCapacitance"),
          "Filter by capacitance?",
          value = FALSE
        )
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("capacitanceMin"),
          "min capacitance (pF):",
          value = 5
        ),
      ),
      div(
        class = "col-xs-4",
        numericInput(
          ns("capacitanceMax"),
          "max capacitance (pF):",
          value = 20
        ),
      )
    ),
    
  # Tabset panels -------------------------------------------------------------
    tabsetPanel(
      
      ## Single Variable -----------------------------------------------------------
      tabPanel(
        "Single Variable",
        zoomAxisUI(ns("singleVarZoomY"), "y"),
        fluidRow(
          div(
            class = "col-xs-4",
            varSelectInput(
              ns("singleVar"),
              "Select variable to summarize",
              GABApscs %>%
                select(
                  frequency:holdingCurrent
                )
            )
          )
        ),
        uiOutput(ns("GABApscsANOVA")),
        plotUI(
          ns("GABApscsScatterPlot")
        ),
        # verbatimTextOutput(
        #   ns("GABApscsScatterPlot_info")
        # ),
        p("Click on a point to get info"),
        tableOutput(
          ns("GABApscsScatterPlot_info")
        ),
        shiny::dataTableOutput(ns("countTable")),
        p("Click on a row in the table to exclude from plot"),
        DTOutput(ns("GABApscsScatterTable"))
      ),
      
      ## Two-variable scatter --------------------------------------------------
      tabPanel(
        "Two Variable",
        zoomAxisUI(ns("twoVarZoomX"), "x"),
        zoomAxisUI(ns("twoVarZoomY"), "y"),
        fluidRow(
          div(
            class = "col-xs-4",
            varSelectInput(
              ns("xVar"),
              "Select x-variable",
              GABApscs %>%
                select(
                  recHr,
                  timeSinceSac,
                  frequency:holdingCurrent,
                  ReproTract_mass,
                  AgeInDays
                )
            ),
            varSelectInput(
              ns("yVar"),
              "Select y-variable",
              GABApscs %>%
                select(
                  frequency:holdingCurrent
                )
            )
          )
        ),
        plotUI(
          ns("GABApscsScatterPlot2")
        ),
        verbatimTextOutput(
          ns("GABApscsScatterPlot2_info")
        ),
        p("Click on a row in the table to exclude from plot"),
        DTOutput(ns("GABApscsScatterTable2"))
      ),
      
      ## Mean Tables -----------------------------------------------------------
      tabPanel(
        "summary tables",
        fluidRow(
          div(
            class = "col-xs-4",
            varSelectInput(
              ns("summaryVars"),
              "Select variables to summarize",
              GABApscs %>%
                select(
                  frequency:holdingCurrent
                ),
              multiple = TRUE
            )
          )
        ),
        shiny::dataTableOutput(ns("summaryTable"))
      )
    )
  )
}


GABApscsServer <- function(
  id,
  GABApscs,
  AcuteStress_off,
  LH_off,
  Cort_off,
  Demo_dam,
  niceNames,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {


      ## Filtering -------------------------------------------------------------

      GABApscs_react <- filteringDFServer("GABA_filter", GABApscs)
      AcuteStress_off_react <- filteringDFServer("GABA_filter", AcuteStress_off)
      LH_off_react <- filteringDFServer("GABA_filter", LH_off)
      Cort_off_react <- filteringDFServer("GABA_filter", Cort_off)
      
      filterByCycleStage <- function(df){
        if(input$cycleStage == "Proestrus"){
          df <- df %>%
            filter(
              if(input$useMass == TRUE) {ReproTract_mass > input$proUterineCutoff & Sac_cycle == "proestrus"}
              else {Sac_cycle == "proestrus"}
            )
        } else if(input$cycleStage == "Diestrus"){
          df <- df %>%
            filter(
              if(input$useMass == TRUE) {ReproTract_mass < input$diUterineCutoff & Sac_cycle == "diestrus"}
              else {Sac_cycle == "diestrus"}
            )
        }
        return(df)
      }
      
      filterByPassives <- function(df){
        if(input$filterByRseries){
          df <- df %>%
            filter(
              Rseries <= input$RseriesMax & Rseries >= input$RseriesMin
            )
        }
        if(input$filterByRinput){
          df <- df %>%
            filter(
              Rinput <= input$RinputMax & Rinput >= input$RinputMin
            )
        }
        if(input$filterByHoldingCurr){
          df <- df %>%
            filter(
              holdingCurrent <= input$holdingCurrMax & holdingCurrent >= input$holdingCurrtMin
            )
        }
        if(input$filterByCapacitance){
          df <- df %>%
            filter(
              capacitance <= input$capacitanceMax & capacitance >= input$capacitanceMin
            )
        }
        return(df)
      }
      
      GABApscs_filtered_react <- reactive({
        df <- GABApscs_react() %>%
          filter(
            sex == "F",
            damStrain == "CBA"
          ) %>%
          filterByCycleStage() %>%
          filterByPassives()
        
        if(input$filterByFreq){
          df <- df %>%
            filter(
              !is.na(frequency)
            )
        }
        
        return(df)
      })
      
      
      ## Single Var -----------------------------------------------------------------------
      singleVarZoomY <- zoomAxisServer("singleVarZoomY", "y", minVal = 0, maxVal = 500)
      
      GABApscsScatterPlot <- reactive({
        yVar <- as.character(input$singleVar)
        yText <- case_when(
          yVar == "frequency" ~ "frequency (Hz)",
          yVar == "relPeak" ~ "relative amplitude (pA)",
          yVar == "interval" ~ "interval (s)",
          yVar == "derivative" ~ "derivative (pA/ms)",
          yVar == "riseTime" ~ "rise time (ms)",
          yVar == "fwhm" ~ "full width at half max. (ms)",
          yVar == "Rinput" ~ "input resistance (MOhm)",
          yVar == "Rseries" ~ "series resistance (MOhm)",
          yVar == "capacitance" ~ "capacitance (pF)",
          yVar == "holdingCurrent" ~ "holding current (pA)",
          yVar == "AgeInDays" ~ "age (days)",
          TRUE ~ as.character(yVar)
        )
        GABApscs_filtered_react() %>%
          filter(
            ! is.na(!! input$singleVar)
          ) %>%
          filter(
            ! (row_number() %in% input$GABApscsScatterTable_rows_selected),
            ! (is.na(adultTrt)) # added 2022-03-14
          ) %>%
          scatterPlotComboTrt(
            yVar = !! input$singleVar,
            yLab = yText,
            dotSize = input$dotSize,
            fontSize = 16,
            zoom_y = singleVarZoomY$zoom(),
            ymin = singleVarZoomY$min(),
            ymax = singleVarZoomY$max()
          # ) + 
          # theme(
          #   legend.position = "top"
          )
      })
      
      GABApscsScatterPlot_info <- plotServer("GABApscsScatterPlot", GABApscsScatterPlot, "GABApscsPlot", compType)
      
      output$GABApscsScatterPlot_info <- renderTable({
        if(is.null(GABApscsScatterPlot_info$click())){
          # "Click on a point to display values - click in the center of the horizontal spread"
        }else({
          x <- GABApscsScatterPlot_info$click()$x
          xRound <- round(x)
          y <- GABApscsScatterPlot_info$click()$y
          catLevel <- GABApscsScatterPlot_info$click()$domain$discrete_limits$x[[xRound]]
          
          GABApscs_filtered <- reactive({GABApscs_filtered_react() %>%
            filter(
              ! is.na(!! input$singleVar)
            ) %>%
            filter(
              ! (row_number() %in% input$GABApscsScatterTable_rows_selected)
            )
          })
          
          if(singleVarZoomY$zoom()){
            yRange = singleVarZoomY$max() - singleVarZoomY$min()
          } else {
            yMin <- min(GABApscs_filtered() %>% select(!! input$singleVar), na.rm = TRUE)
            yMax <- max(GABApscs_filtered() %>% select(!! input$singleVar), na.rm = TRUE)
            yRange <- yMax - yMin
          }
          
          yError <- yRange * 0.03
          
          GABApscs_filtered() %>%
            filter(
              comboTrt == catLevel,
              !! input$singleVar <= y + yError & !! input$singleVar >= y - yError
            ) %>%
            select(
              cellID,
              mouseID,
              earlyLifeTrt,
              adultTrt,
              comboTrt,
              !! input$singleVar,
              ReproTract_mass,
              Rseries,
              Rinput,
              capacitance,
              holdingCurrent
            )
        })
      })
      
      output$GABApscsScatterTable <- renderDT({
        GABApscs_filtered_react() %>%
          filter(
            ! is.na(!! input$singleVar)
          ) %>%
          select(
            cellID,
            mouseID,
            earlyLifeTrt,
            adultTrt,
            comboTrt,
            !! input$singleVar,
            ReproTract_mass,
            Rseries,
            Rinput,
            capacitance,
            holdingCurrent
          )
      })
      
      output$GABApscsANOVA <- renderUI({
        GABApscs_filtered_react() %>%
          filter(
            ! is.na(!! input$singleVar)
          ) %>%
          filter(
            ! (row_number() %in% input$GABApscsScatterTable_rows_selected)
          ) %>%
          anova_test(
            dv = !! input$singleVar,
            between = c(earlyLifeTrt, adultTrt),
            type = 3
          ) %>%
          formatAnova() %>%
          htmltools_value()
      })
      
      output$countTable <- shiny::renderDataTable({
        countTbl <-  GABApscs_filtered_react() %>%
          # filter(
          #   !is.na(!! input$singleVar)
          # ) %>%
          filter(
            ! (row_number() %in% input$GABApscsScatterTable_rows_selected)
          ) %>%
          countMiceAndLitters(
            !! input$singleVar,
            c(expr(earlyLifeTrt), expr(adultTrt))
          )
        
        meanTbl <- GABApscs_filtered_react() %>%
          filter(
            !is.na(!! input$singleVar)
          ) %>%
          filter(
            ! (row_number() %in% input$GABApscsScatterTable_rows_selected)
          ) %>%
          group_by(earlyLifeTrt, adultTrt) %>%
          meanSummary(!! input$singleVar)
        
        countTbl %>%
          left_join(
            meanTbl %>% select(-n),
            by = c("earlyLifeTrt", "adultTrt")
          )
          
      })
      
      ## Two variable ----------------------------------------------------------
      twoVarZoomX <- zoomAxisServer("twoVarZoomX", "x", minVal = 0, maxVal = 500)
      twoVarZoomY <- zoomAxisServer("twoVarZoomY", "y", minVal = 0, maxVal = 500)
      
      GABApscsScatterPlot2 <- reactive({
        yVar <- as.character(input$yVar)
        yText <- case_when(
          yVar == "frequency" ~ "frequency (Hz)",
          yVar == "relPeak" ~ "relative amplitude (pA)",
          yVar == "interval" ~ "interval (s)",
          yVar == "derivative" ~ "derivative (pA/ms)",
          yVar == "riseTime" ~ "rise time (ms)",
          yVar == "fwhm" ~ "full width at half max. (ms)",
          yVar == "Rinput" ~ "input resistance (MOhm)",
          yVar == "Rseries" ~ "series resistance (MOhm)",
          yVar == "capacitance" ~ "capacitance (pF)",
          yVar == "holdingCurrent" ~ "holding current (pA)",
          TRUE ~ as.character(yVar)
        )
        xVar <- as.character(input$xVar)
        xText <- case_when(
          xVar == "frequency" ~ "frequency (Hz)",
          xVar == "relPeak" ~ "relative amplitude (pA)",
          xVar == "interval" ~ "interval (s)",
          xVar == "derivative" ~ "derivative (pA/ms)",
          xVar == "riseTime" ~ "rise time (ms)",
          xVar == "fwhm" ~ "full width at half max. (ms)",
          xVar == "Rinput" ~ "input resistance (MOhm)",
          xVar == "Rseries" ~ "series resistance (MOhm)",
          xVar == "capacitance" ~ "capacitance (pF)",
          xVar == "holdingCurrent" ~ "holding current (pA)",
          xVar == "ReproTract_mass" ~ "uterine mass (mg)",
          xVar == "recHr" ~ "hr since lights on",
          xVar == "timeSinceSac" ~ "hr since sacrifice",
          TRUE ~ as.character(yVar)
        )
        GABApscs_filtered_react() %>%
          filter(
            ! is.na(!! input$xVar),
            ! is.na(!! input$yVar)
          ) %>%
          filter(
            ! (row_number() %in% input$GABApscsScatterTable2_rows_selected),
            ! (is.na(adultTrt)) # added 2022-03-14
          ) %>%
          scatterPlotTwoVars_byComboTrt(
            yVar = !! input$yVar,
            yLab = yText,
            xVar = !! input$xVar,
            xLab = xText,
            dotSize = input$dotSize,
            fontSize = 16,
            # zoom_x = TRUE,
            # xmin = 13,
            # xmax = 17,
            zoom_x = twoVarZoomX$zoom(),
            xmin = twoVarZoomX$min(),
            xmax = twoVarZoomX$max(),
            zoom_y = twoVarZoomY$zoom(),
            ymin = twoVarZoomY$min(),
            ymax = twoVarZoomY$max()
            # ) + 
            # theme(
            #   legend.position = "top"
          )
      })
      
      GABApscsScatterPlot2_info <- plotServer("GABApscsScatterPlot2", GABApscsScatterPlot2, "GABApscsScatterPlot", compType)
      
      output$GABApscsScatterPlot2_info <- renderPrint({
        if(is.null(GABApscsScatterPlot2_info$click())){
          "Click on a point to display values"
        }else(
          nearPoints(
            GABApscs_filtered_react() %>%
              filter(
                ! is.na(!! input$xVar),
                ! is.na(!! input$yVar)
              ) %>%
              filter(
                ! (row_number() %in% input$GABApscsScatterTable2_rows_selected)
              ) %>%
              select(
                cellID,
                mouseID,
                earlyLifeTrt,
                adultTrt,
                comboTrt,
                !! input$xVar,
                !! input$yVar,
                ReproTract_mass,
                Rseries,
                Rinput,
                capacitance,
                holdingCurrent
              ),
            GABApscsScatterPlot2_info$click()
          )
        )
      })
      
      output$GABApscsScatterTable2 <- renderDT({
        GABApscs_filtered_react() %>%
          filter(
            ! is.na(!! input$xVar),
            ! is.na(!! input$yVar)
          ) %>%
          select(
            cellID,
            mouseID,
            earlyLifeTrt,
            adultTrt,
            comboTrt,
            !! input$xVar,
            !! input$yVar,
            ReproTract_mass,
            Rseries,
            Rinput,
            capacitance,
            holdingCurrent
          )
      })
      
      ## Mean Summary Tables ----------------------------------------------------------
      AcuteStress_males_summary <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "M",
            damStrain == "CBA",
            if_any(c(!!! input$summaryVars), ~ !is.na(.x)),
            !(exclude_cort_hr0 | exclude_cort_hr5)
          )
        if(input$removeExtraMales){
          df <- df %>%
            filter(
              includeMaleCort
            )
        }
        return(df)
      })

      AcuteStress_females_summary <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "F",
            damStrain == "CBA",
            if_any(c(!!! input$summaryVars), ~ !is.na(.x))
          ) %>%
          filterByCycleStage()
        return(df)
      })

      output$summaryTable_males <- shiny::renderDataTable({
        if(length(input$summaryVars)>0 & length(AcuteStress_males_summary()$mouseID) > 0){
          AcuteStress_males_summary() %>%
            group_by(earlyLifeTrt, adultTrt) %>%
            meanSummary(c(!!! input$summaryVars))
        }
      })
      output$summaryTable_females <- shiny::renderDataTable({
        if(length(input$summaryVars)>0 & length(AcuteStress_females_summary()$mouseID) > 0){
          AcuteStress_females_summary() %>%
            group_by(earlyLifeTrt, adultTrt) %>%
            meanSummary(c(!!! input$summaryVars))
        }
      })
    }
  )
}

