### Acute Stress App Module

# https://shiny.rstudio.com/articles/modules.html

acuteStressUI <- function(id,
                          AcuteStress_off){
  ns <- NS(id)
  tagList(

    h3("Acute, Layered, Psychosocial Stress Paradigm"),
    
  # Filtering ---------------------------------------------------------------
    filteringDFUI(ns("ALPS_filter"), AcuteStress_off),

    fluidRow(
      column(
        4,
        numericInput(
          ns("proUterineCutoff"),
          "Uterine mass (mg) min for proestrus:",
          value = 135
        ),
        numericInput(
          ns("diUterineCutoff"),
          "Uterine mass (mg) max for diestrus:",
          value = 100
        )
      ),
      column(
        4,
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
      column(
        4,
        numericInput(
          ns("dotSize"),
          "Dot size",
          value = 2
        )
        # p("Currently have it set so that intended stage and mass have to match for selected stage")
      )
    ),
    
  # Tabset panels -------------------------------------------------------------
    tabsetPanel(

      ## Uterine Mass ---------------------------------------------------
      tabPanel(
        "Uterine Mass By Trt",
        plotOutput(
          ns("uterineMassByTrt"),
          click = ns("uterineMassByTrt_click")
        ),
        verbatimTextOutput(
          ns("uterineMassByTrt_infoText")
        ),
        shiny::dataTableOutput(ns("uterineMassByTrt_info")),
        p("Click on a row in the table to exclude from plot"),
        DTOutput(ns("uterineMassByTrt_table"))
      ),
      tabPanel(
        "By Uterine Mass",
        fluidRow(
          column(
            4,
            varSelectInput(
              ns("uterineMass_yVar"),
              "Select y-variable",
              AcuteStress_off %>%
                select(
                  maxLH,
                  starts_with("cort_hr"),
                  starts_with("LH_hr")
                )
            )
          ),
          column(
            4,
            selectInput(
              ns("earlyLifeTrt"),
              "Which early-life treatment groups?",
              choices = unique(AcuteStress_off$earlyLifeTrt), # Changed from levels to unique
              multiple = TRUE,
              selected = unique(AcuteStress_off$earlyLifeTrt)
            )
          ),
          column(
            4,
            selectInput(
              ns("adultTrt"),
              "Which adult treatment groups?",
              choices = unique(AcuteStress_off$adultTrt),
              multiple = TRUE,
              selected = unique(AcuteStress_off$adultTrt),
            )
          )
        ),
        plotOutput(
          ns("plotByUterineMass"),
          click = ns("plotByUterineMass_click")
        ),
        verbatimTextOutput(
          ns("plotByUterineMass_info")
        ),
        p("Click on a row in the table to exclude from plot"),
        DTOutput(ns("plotByUterineMass_table"))
      ),

      ## LH Profile --------------------------------------------------------------

      tabPanel(
        "LH Profile",
        plotOutput(
          ns("LHprofile"),
          click = ns("LHprofile_click")
        ),
        verbatimTextOutput(
          ns("LHprofile_info")
        )
      ),
      
      ## Corticosterone ----------------------------------------------------------
      
      tabPanel(
        "Corticosterone",
        fluidRow(
          column(
            4,
            checkboxInput(
              ns("cortPlotLong"),
              "Use long cort plot style",
              value = FALSE
            )
          )
        ),
        tabsetPanel(
          tabPanel(
            "males",
            uiOutput(
              ns("cortANOVA_males")
            ),
            plotOutput(
              ns("cortPlot_males"),
              click = ns("cortPlot_males_click")
            ),
            verbatimTextOutput(
              ns("cortPlot_males_info")
            ),
            p("Click on a row in the table to exclude from plot"),
            DTOutput(ns("cortPlot_males_table"))
          ),
          tabPanel(
            "females",
            fluidRow(
              
            ),
            uiOutput(
              ns("cortANOVA_females")
            ),
            plotOutput(
              ns("cortPlot_females"),
              click = ns("cortPlot_females_click")
            ),
            verbatimTextOutput(
              ns("cortPlot_females_info")
            ),
            p("Click on a row in the table to exclude from plot"),
            DTOutput(ns("cortPlot_females_table"))
          )
        ),
      ),
      
      ## masses -----------------------------------------------------------
      tabPanel(
        "masses",
        fluidRow(
          column(
            4,
            varSelectInput(
              ns("massVar"),
              "Select variables to summarize",
              AcuteStress_off %>%
                select(
                  Body_mass_sac,
                  ReproTract_mass,
                  ReproTract_mass_perBody_g,
                  Gonad_mass,
                  Gonad_mass_perBody_g,
                  Adrenal_mass,
                  Adrenal_mass_perBody_g
                )
            )
          )
        ),
        tabsetPanel(
          tabPanel(
            "males",
            uiOutput(ns("massANOVA_males")),
            plotOutput(ns("massPlot_males"),
                       click = ns("massPlot_males_click")),
            verbatimTextOutput(
              ns("massPlot_males_infoText")
            ),
            shiny::dataTableOutput(ns("massPlot_males_info")),
            p("Click on a row in the table to exclude from plot"),
            DTOutput(ns("mass_males_table"))
          ),
          tabPanel(
            "females",
            uiOutput(ns("massANOVA_females")),
            plotOutput(ns("massPlot_females"),
                       click = ns("massPlot_females_click")),
            verbatimTextOutput(
              ns("massPlot_females_infoText")
            ),
            shiny::dataTableOutput(ns("massPlot_females_info")),
            p("Click on a row in the table to exclude from plot"),
            DTOutput(ns("mass_females_table"))
          )
        )
      ),
      
      ## Mean Tables -----------------------------------------------------------
      tabPanel(
        "summary tables",
        fluidRow(
          column(
            4,
            varSelectInput(
              ns("summaryVars"),
              "Select variables to summarize",
              AcuteStress_off %>%
                select(
                  Body_mass_sac,
                  ReproTract_mass,
                  ReproTract_mass_perBody_g,
                  Gonad_mass,
                  Gonad_mass_perBody_g,
                  Adrenal_mass,
                  Adrenal_mass_perBody_g,
                  maxLH,
                  starts_with("cort_hr"),
                  starts_with("LH_hr")
                ),
              multiple = TRUE
            )
          )
        ),
        tabsetPanel(
          tabPanel(
            "males",
            shiny::dataTableOutput(ns("summaryTable_males"))
          ),
          tabPanel(
            "females",
            shiny::dataTableOutput(ns("summaryTable_females"))
          )
        )
      )
    )
  )
}


acuteStressServer <- function(
  id,
  AcuteStress_off,
  LH_off,
  Cort_off,
  Demo_dam,
  niceNames
){
  moduleServer(
    id,
    function(input, output, session) {


      ## Filtering -------------------------------------------------------------

      AcuteStress_off_react <- filteringDFServer("ALPS_filter", AcuteStress_off)
      LH_off_react <- filteringDFServer("ALPS_filter", LH_off)
      Cort_off_react <- filteringDFServer("ALPS_filter", Cort_off)
      
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

      ## Uterine Mass By Trt---------------------------------------------
      AcuteStress_femaleCBA_mass_react <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "F",
            Dam_Strain == "CBA",
            !is.na(ReproTract_mass)
          ) %>%
          filterByCycleStage()
        return(df)
      })

      output$uterineMassByTrt <- renderPlot({
        AcuteStress_femaleCBA_mass_react() %>%
          filter(
            ! (row_number() %in% input$uterineMassByTrt_table_rows_selected)
          ) %>%
          plotUterineMassByGroup(
            hLineVal = input$proUterineCutoff,
            fontSize = 16,
            dotSize = input$dotSize
          ) +
          geom_hline(
            yintercept = input$diUterineCutoff,
            color = "blue"
          )
      })

      output$uterineMassByTrt_infoText <- renderPrint({
        if(is.null(input$uterineMassByTrt_click$y)){
          "Click on a point to display values"
        }else(
          paste0(
            "Looking for values between ", #one binwidth +/- click y value
            round(input$uterineMassByTrt_click$y - 2, 3),
            " and ",
            round(input$uterineMassByTrt_click$y + 2, 3)
          )
        )
      })
      
      output$uterineMassByTrt_info <- shiny::renderDataTable({
        if(!is.null(input$uterineMassByTrt_click$y)){
          AcuteStress_femaleCBA_mass_react() %>%
            filter(
              ! (row_number() %in% input$uterineMassByTrt_table_rows_selected),
              near(ReproTract_mass, input$uterineMassByTrt_click$y, tol = 2)
            ) %>%
            select(
              mouseID,
              num_ID,
              earlyLifeTrt,
              adultTrt,
              Sac_cycle,
              ReproTract_mass,
              maxLH
            )
        }
      })

      output$uterineMassByTrt_table <- renderDT({
        AcuteStress_femaleCBA_mass_react() %>%
          select(
            mouseID,
            num_ID,
            earlyLifeTrt,
            adultTrt,
            Sac_cycle,
            ReproTract_mass
          )
      })
      ## By Uterine Mass ---------------------------------------------
      AcuteStress_femaleCBA_react <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "F",
            Dam_Strain == "CBA",
            !is.na(!! input$uterineMass_yVar),
            earlyLifeTrt %in% as.character(input$earlyLifeTrt),
            adultTrt %in% as.character(input$adultTrt)
          ) %>%
          filterByCycleStage()
        return(df)
      })

      output$plotByUterineMass <- renderPlot({
        if(input$uterineMass_yVar == "maxLH"){
          yLabText <- "max evening LH (ng/mL)"
        }else if(grepl("^cort_", input$uterineMass_yVar)){
          yLabText <- "corticosterone (ng/mL)"
        }else if(grepl("^LH_", input$uterineMass_yVar)){
          yLabText <- "LH (ng/mL)"
        }
        print(yLabText)
        AcuteStress_femaleCBA_react() %>%
          filter(
            ! (row_number() %in% input$plotByUterineMass_table_rows_selected)
          ) %>%
          plotByUterineMass(
            yVar = !! input$uterineMass_yVar,
            yLab = yLabText,
            fontSize = 16,
            dotSize = input$dotSize
          ) +
          geom_vline(
            xintercept = input$proUterineCutoff,
            color = "red"
          ) +
          geom_vline(
            xintercept = input$diUterineCutoff,
            color = "blue"
          )
      })

      output$plotByUterineMass_info <- renderPrint({
        if(is.null(input$plotByUterineMass_click)){
          "Click on a point to display values"
        }else(
          nearPoints(
            AcuteStress_femaleCBA_react() %>%
              filter(
                ! (row_number() %in% input$plotByUterineMass_table_rows_selected)
              ) %>%
              select(
                mouseID,
                num_ID,
                earlyLifeTrt,
                adultTrt,
                ReproTract_mass,
                !! input$uterineMass_yVar
              ),
            input$plotByUterineMass_click
          )
        )
      })

      output$plotByUterineMass_table <- renderDT({
        AcuteStress_femaleCBA_react() %>%
          select(
            mouseID,
            num_ID,
            earlyLifeTrt,
            adultTrt,
            ReproTract_mass,
            !! input$uterineMass_yVar
          )
      })

      ## LH Profile -----------------------------------------------------------
      output$LHprofile <- renderPlot({
        df <- LH_off_react() %>%
          filter(
            sex == "F"
          ) %>%
          filterByCycleStage()
        
        plot <- df%>%
          LHPlot(
            fontSize = 16,
            dotSize = input$dotSize
          )+
          facet_wrap(
            ~adultTrt
          )
        return(plot)
      })

      output$LHprofile_info <- renderPrint({
        if(is.null(input$LHprofile_click)){
          "Click on a point to display values - click in the middle of the horizontal scatter"
        }else(
          nearPoints(
            LH_off_react() %>%
              select(
                mouseID,
                num_ID,
                earlyLifeTrt,
                adultTrt,
                time,
                LH,
                ReproTract_mass
              ),
            input$LHprofile_click
          )
        )
      })
      
      ## Corticosterone ----------------------------------------------------------
      Cort_off_males <- reactive({
        df <- Cort_off_react() %>%
          filter(
            sex == "M",
            Dam_Strain == "CBA",
            !is.na(cort)
          )
      })
      
      output$cortANOVA_males <- renderUI({
        Cort_off_males() %>%
          filter(
            ! (row_number() %in% input$cortPlot_males_table_rows_selected)
          ) %>%
          cortAnova() %>%
          htmltools_value()
      })
      
      output$cortPlot_males <- renderPlot({
        if(input$cortPlotLong){
          basePlot <- Cort_off_males() %>%
            filter(
              ! (row_number() %in% input$cortPlot_males_table_rows_selected)
            ) %>%
            baseCortPlot(dotSize = input$dotSize)
          plot <- basePlot %>%
            longCortPlot(fontSize = 16)
        } else {
          plot <- Cort_off_males() %>%
            filter(
              ! (row_number() %in% input$cortPlot_males_table_rows_selected)
            ) %>%
            cortPlot(pointSize = 2, fontSize = 16)
        }
        
        return(plot)
      })
      
      output$cortPlot_males_info <- renderPrint({
        if(is.null(input$cortPlot_males_click)){
          "Click on a point to display values - click in the center of the horizontal spread"
        }else(
          nearPoints(
            Cort_off_males() %>%
              filter(
                ! (row_number() %in% input$cortPlot_males_table_rows_selected)
              ) %>%
              select(
                mouseID,
                num_ID,
                earlyLifeTrt,
                adultTrt,
                ReproTract_mass,
                time,
                cort
              ),
            input$cortPlot_males_click
          )
        )
      })
      
      output$cortPlot_males_table <- renderDT({
        Cort_off_males() %>%
          select(
            mouseID,
            num_ID,
            earlyLifeTrt,
            adultTrt,
            ReproTract_mass,
            time,
            cort
          )
      })
      
      
      Cort_off_females <- reactive({
        df <- Cort_off_react() %>%
          filter(
            sex == "F",
            Dam_Strain == "CBA",
            !is.na(cort)
          ) %>%
          filterByCycleStage()
        return(df)
      })
      
      output$cortANOVA_females <- renderUI({
        Cort_off_females() %>%
          filter(
            ! (row_number() %in% input$cortPlot_females_table_rows_selected)
          ) %>%
          cortAnova() %>%
          htmltools_value()
      })
      
      output$cortPlot_females <- renderPlot({
        if(input$cortPlotLong){
          basePlot <- Cort_off_females() %>%
            filter(
              ! (row_number() %in% input$cortPlot_females_table_rows_selected)
            ) %>%
            baseCortPlot(dotSize = input$dotSize)
          plot <- basePlot %>%
            longCortPlot(fontSize = 16)
        } else {
          plot <- Cort_off_females() %>%
            filter(
              ! (row_number() %in% input$cortPlot_females_table_rows_selected)
            ) %>%
            cortPlot(pointSize = 2, fontSize = 16)
        }
        return(plot)
      })
      
      output$cortPlot_females_info <- renderPrint({
        if(is.null(input$cortPlot_females_click)){
          "Click on a point to display values - click in the center of the horizontal spread"
        }else(
          nearPoints(
            Cort_off_females() %>%
              filter(
                ! (row_number() %in% input$cortPlot_females_table_rows_selected)
              ) %>%
              select(
                mouseID,
                num_ID,
                earlyLifeTrt,
                adultTrt,
                ReproTract_mass,
                time,
                cort
              ),
            input$cortPlot_females_click
          )
        )
      })
      
      output$cortPlot_females_table <- renderDT({
        Cort_off_females() %>%
          select(
            mouseID,
            num_ID,
            earlyLifeTrt,
            adultTrt,
            ReproTract_mass,
            time,
            cort
          )
      })
      
      ## Masses -----------------------------------------------------------------------
      AcuteStress_males_masses <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "M",
            Dam_Strain == "CBA",
            !is.na(!! input$massVar)
          )
        return(df)
      })
      
      output$massPlot_males <- renderPlot({
        yVar <- as.character(input$massVar)
        yText <- case_when(
          yVar == "ReproTract_mass" ~ "seminal vesicle mass (mg)",
          yVar == "ReproTract_mass_perBody_g" ~ "seminal vesicle mass / body mass (mg/g)",
          yVar == "Gonad_mass" ~ "testicular mass (mg)",
          yVar == "Gonad_mass_perBody_g" ~ "testicular mass / body mass (mg/g)",
          yVar == "Adrenal_mass" ~ "adrenal mass (mg)",
          yVar == "Adrenal_mass_perBody_g" ~ "adrenal mass / body mass (mg/g)",
          yVar == "Body_mass_sac" ~ "body mass (g)",
          TRUE ~ as.character(yVar)
        )
        AcuteStress_males_masses() %>%
          filter(
            ! (row_number() %in% input$mass_males_table_rows_selected)
          ) %>%
          scatterPlotComboTrt(
            yVar = !! input$massVar,
            yLab = yText,
            dotSize = input$dotSize,
            fontSize = 16
          )
      })
      
      output$massPlot_males_infoText <- renderPrint({
        tol <- max(AcuteStress_males_masses() %>% select(!! input$massVar), na.rm = TRUE) / 40
        if(is.null(input$massPlot_males_click$y)){
          "Click on a point to display values"
        }else(
          paste0(
            "Looking for values between ", #one binwidth +/- click y value
            round(input$massPlot_males_click$y - tol, 3),
            " and ",
            round(input$massPlot_males_click$y + tol, 3)
          )
        )
      })
      
      output$massPlot_males_info <- shiny::renderDataTable({
        if(!is.null(input$massPlot_males_click$y)){
          AcuteStress_males_masses() %>%
            filter(
              ! (row_number() %in% input$mass_males_table_rows_selected),
              near(!! input$massVar, input$massPlot_males_click$y, tol = max(!! input$massVar, na.rm = TRUE)/40)
            ) %>%
            select(
              mouseID,
              num_ID,
              earlyLifeTrt,
              adultTrt,
              !! input$massVar
            )
        }
      })
      
      output$mass_males_table <- renderDT({
        AcuteStress_males_masses() %>%
          select(
            mouseID,
            num_ID,
            earlyLifeTrt,
            adultTrt,
            !! input$massVar
          )
      })
      
      output$massANOVA_males <- renderUI({
        AcuteStress_males_masses() %>%
          filter(
            ! (row_number() %in% input$mass_males_table_rows_selected)
          ) %>%
          anova_test(
            dv = !! input$massVar,
            between = c(earlyLifeTrt, adultTrt),
            type = 3
          ) %>%
          formatAnova() %>%
          htmltools_value()
      })
      
      AcuteStress_females_masses <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "F",
            Dam_Strain == "CBA",
            !is.na(!! input$massVar)
          ) %>%
          filterByCycleStage()
        return(df)
      })
      
      output$massPlot_females <- renderPlot({
        yVar <- as.character(input$massVar)
        yText <- case_when(
          yVar == "ReproTract_mass" ~ "uterine mass (mg)",
          yVar == "ReproTract_mass_perBody_g" ~ "uterine mass / body mass (mg/g)",
          yVar == "Gonad_mass" ~ "testicular mass (mg)",
          yVar == "Gonad_mass_perBody_g" ~ "ovarian mass / body mass (mg/g)",
          yVar == "Adrenal_mass" ~ "adrenal mass (mg)",
          yVar == "Adrenal_mass_perBody_g" ~ "adrenal mass / body mass (mg/g)",
          yVar == "Body_mass_sac" ~ "body mass (g)",
          TRUE ~ as.character(yVar)
        )
        AcuteStress_females_masses() %>%
          filter(
            ! (row_number() %in% input$mass_females_table_rows_selected)
          ) %>%
          scatterPlotComboTrt(
            yVar = !! input$massVar,
            yLab = yText,
            dotSize = input$dotSize,
            fontSize = 16
          )
      })
      
      output$massPlot_females_infoText <- renderPrint({
        tol <- max(AcuteStress_females_masses() %>% select(!! input$massVar), na.rm = TRUE) / 40
        if(is.null(input$massPlot_females_click$y)){
          "Click on a point to display values"
        }else(
          paste0(
            "Looking for values between ", #one binwidth +/- click y value
            round(input$massPlot_females_click$y - tol, 3),
            " and ",
            round(input$massPlot_females_click$y + tol, 3)
          )
        )
      })
      
      output$massPlot_females_info <- shiny::renderDataTable({
        if(!is.null(input$massPlot_females_click$y)){
          AcuteStress_females_masses() %>%
            filter(
              ! (row_number() %in% input$mass_females_table_rows_selected),
              near(!! input$massVar, input$massPlot_females_click$y, tol = max(!! input$massVar, na.rm = TRUE)/40)
            ) %>%
            select(
              mouseID,
              num_ID,
              earlyLifeTrt,
              adultTrt,
              !! input$massVar
            )
        }
      })
      
      output$mass_females_table <- renderDT({
        AcuteStress_females_masses() %>%
          select(
            mouseID,
            num_ID,
            earlyLifeTrt,
            adultTrt,
            !! input$massVar
          )
      })
      
      output$massANOVA_females <- renderUI({
        AcuteStress_females_masses() %>%
          filter(
            ! (row_number() %in% input$mass_females_table_rows_selected)
          ) %>%
          anova_test(
            dv = !! input$massVar,
            between = c(earlyLifeTrt, adultTrt),
            type = 3
          ) %>%
          formatAnova() %>%
          htmltools_value()
      })
      
      ## Mean Summary Tables ----------------------------------------------------------
      AcuteStress_males_summary <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "M",
            Dam_Strain == "CBA",
            if_any(c(!!! input$summaryVars), ~ !is.na(.x))
          )
        return(df)
      })
      
      AcuteStress_females_summary <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "F",
            Dam_Strain == "CBA",
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

