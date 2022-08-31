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
    
    filteringEphysUI(ns("ephysFilter")),
    
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
        shiny::dataTableOutput(ns("summaryTableCount")),
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
      
      # Change to expect reactive within function
      GABApscs_filterEphys_react <- filteringEphysServer(
          "ephysFilter",
          GABApscs_react
        )
      
    
      GABApscs_filtered_react <- reactive({
        df <- GABApscs_filterEphys_react() %>%
          filter(
            sex == "F",
            damStrain == "CBA"
          ) %>%
          filterByCycleStage()
        
        if(input$filterByFreq){
          df <- df %>%
            filter(
              !is.na(frequency)
            )
        }
        
        return(df)
      })
      
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
      output$summaryTable <- shiny::renderDataTable({
        if(length(input$summaryVars)>0 & length(GABApscs_filtered_react()$cellID) > 0){
        GABApscs_filtered_react() %>%
          group_by(earlyLifeTrt, adultTrt) %>%
          meanSummary(c(!!! input$summaryVars))
        }
      })
      
      output$summaryTableCount <- shiny::renderDataTable({
        if(length(input$summaryVars)>0 & length(GABApscs_filtered_react()$cellID) > 0){
          GABApscs_filtered_react() %>%
            group_by(earlyLifeTrt, adultTrt) %>%
            summarise(
              numCells = n(),
              numMice = length(unique(mouseID)),
              numLitters = length(unique(damID)),
              .groups = "drop"
            )
        }
      })
      
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

