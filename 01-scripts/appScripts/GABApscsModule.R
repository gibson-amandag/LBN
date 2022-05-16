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
        catPlotComboTrtUI(
          ns("singleVarGABA"),
          GABApscs %>%
            select(
              frequency:holdingCurrent
            )
        )
      ),
      
      ## Two-variable scatter --------------------------------------------------
      tabPanel(
        "Two Variable",
        scatterPlotComboTrtUI(
          ns("twoVarGABA"),
          GABApscs %>%
            select(
              recHr,
              timeSinceSac,
              frequency:holdingCurrent,
              ReproTract_mass,
              AgeInDays
            ),
          GABApscs %>%
            select(
              frequency:holdingCurrent
            )
        )
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
      
      # GABAcaseFunc <- function(colName){
      #   case_when(
      #     colName == "frequency" ~ "frequency (Hz)",
      #     colName == "relPeak" ~ "relative amplitude (pA)",
      #     colName == "interval" ~ "interval (s)",
      #     colName == "derivative" ~ "derivative (pA/ms)",
      #     colName == "riseTime" ~ "rise time (ms)",
      #     colName == "fwhm" ~ "full width at half max. (ms)",
      #     colName == "Rinput" ~ "input resistance (MOhm)",
      #     colName == "Rseries" ~ "series resistance (MOhm)",
      #     colName == "capacitance" ~ "capacitance (pF)",
      #     colName == "holdingCurrent" ~ "holding current (pA)",
      #     colName == "AgeInDays" ~ "age (days)",
      #     colName == "ReproTract_mass" ~ "uterine mass (mg)",
      #     colName == "recHr" ~ "hr since lights on",
      #     colName == "timeSinceSac" ~ "hr since sacrifice",
      #     TRUE ~ as.character(colName)
      #   )
      # }
      
      ## Single Var -----------------------------------------------------------------------
      
      observeEvent(
        c(GABApscs_filtered_react(),
          input$dotSize),
        {
          catPlotComboTrtServer(
            "singleVarGABA",
            GABApscs_filtered_react(),
            getNiceName,
            c(
              !!! exprs(
                cellID,
                mouseID,
                earlyLifeTrt,
                adultTrt,
                comboTrt,
                ReproTract_mass,
                Rseries,
                Rinput,
                capacitance,
                holdingCurrent
              )
            ),
            dotSize = input$dotSize,
            compType = compType
          )
        }
      )
      
      ## Two variable ----------------------------------------------------------
      observeEvent(
        c(GABApscs_filtered_react(),
          input$dotSize),
        {
          scatterPlotComboTrtServer(
            "twoVarGABA",
            GABApscs_filtered_react(),
            getNiceName,
            # GABAcaseFunc,
            c(
              !!! exprs(
                cellID,
                mouseID,
                earlyLifeTrt,
                adultTrt,
                comboTrt,
                ReproTract_mass,
                Rseries,
                Rinput,
                capacitance,
                holdingCurrent
              )
            ),
            dotSize = input$dotSize,
            compType = compType
          )
        }
      )
      
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

