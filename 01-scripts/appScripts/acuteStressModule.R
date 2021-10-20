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
    
  # Tabset panels -------------------------------------------------------------
    tabsetPanel(

      ## Uterine Mass ---------------------------------------------------
      tabPanel(
        "Uterine Mass By Trt",
        uterineMassByTrtUI(ns("uterineMassByTrt"), AcuteStress_off)
      ),
      tabPanel(
        "By Uterine Mass",
        byUterineMassUI(ns("byUterineMass"), AcuteStress_off)
      ),

      ## LH Profile --------------------------------------------------------------

      tabPanel(
        "LH Profile",
        LHprofileUI(ns("LHprofile"))
        # zoomAxisUI(ns("LHprofile_zoom_y"), "y"),
        # plotOutput(
        #   ns("LHprofile"),
        #   click = ns("LHprofile_click")
        # ),
        # verbatimTextOutput(
        #   ns("LHprofile_info")
        # )
      ),
      
      ## Corticosterone ----------------------------------------------------------
      
      tabPanel(
        "Corticosterone",
        fluidRow(
          div(
            class = "col-xs-4",
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
            checkboxInput(
              ns("removeExtraMales"),
              label = "Remove Extra Males",
              value = FALSE
            ),
            uiOutput(
              ns("cortANOVA_males")
            ),
            plotUI(
              ns("cortPlot_males")
            ),
            # plotOutput(
            #   ns("cortPlot_males"),
            #   click = ns("cortPlot_males_click")
            # ),
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
          div(
            class = "col-xs-4",
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
                  Adrenal_mass_perBody_g,
                  maxLH
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
          div(
            class = "col-xs-4",
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
  niceNames,
  compType
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
      
      AcuteStress_femaleCBA_react <- reactive({
        df <- AcuteStress_off_react() %>%
          filter(
            sex == "F",
            damStrain == "CBA"
          ) %>%
          filterByCycleStage()
        return(df)
      })
      
      LH_femalesCBA_react <- reactive({
        df <- LH_off_react() %>%
          filter(
            sex == "F",
            damStrain == "CBA"
          ) %>%
          filterByCycleStage()
        return(df)
      })

      ## Uterine Mass By Trt---------------------------------------------
      AcuteStress_femaleCBA_mass_react <- reactive({
        df <- AcuteStress_femaleCBA_react() %>%
          filter(
            !is.na(ReproTract_mass)
          )
        return(df)
      })
      
      observeEvent(
        c(AcuteStress_femaleCBA_mass_react(),
          input$proUterineCutoff,
          input$diUterineCutoff,
          input$dotSize),
        {
        uterineMassByTrtServer(
          "uterineMassByTrt",
          df = AcuteStress_femaleCBA_mass_react(),
          input$proUterineCutoff,
          input$diUterineCutoff,
          input$dotSize
        )
      })

      
      ## By Uterine Mass ---------------------------------------------
      observeEvent(
        c(AcuteStress_femaleCBA_react(),
          input$proUterineCutoff,
          input$diUterineCutoff,
          input$dotSize),
        {
          byUterineMassServer(
            "byUterineMass",
            df = AcuteStress_femaleCBA_react(),
            input$proUterineCutoff,
            input$diUterineCutoff,
            input$dotSize
          )
        })

      ## LH Profile -----------------------------------------------------------
      observeEvent(
        c(LH_femalesCBA_react(),
          AcuteStress_femaleCBA_react(),
          input$proUterineCutoff,
          input$diUterineCutoff,
          input$dotSize),
        {
          LHprofileServer(
            "LHprofile",
            LH_long = LH_femalesCBA_react(),
            AcuteStressDF = AcuteStress_femaleCBA_react(),
            input$proUterineCutoff,
            input$diUterineCutoff,
            input$dotSize
          )
        })
      # LHprofile_zoom_y <- zoomAxisServer("LHprofile_zoom_y", "y", minVal = 0, maxVal = 45)
      # 
      # output$LHprofile <- renderPlot({
      #   df <- LH_off_react() %>%
      #     filter(
      #       sex == "F"
      #     ) %>%
      #     filterByCycleStage()
      #   
      #   plot <- df%>%
      #     LHPlot(
      #       fontSize = 16,
      #       dotSize = input$dotSize,
      #       zoom_y = LHprofile_zoom_y$zoom(),
      #       ymin = LHprofile_zoom_y$min(),
      #       ymax = LHprofile_zoom_y$max()
      #     )+
      #     facet_wrap(
      #       # ~adultTrt
      #       ~comboTrt
      #     )
      #   return(plot)
      # })
      # 
      # output$LHprofile_info <- renderPrint({
      #   if(is.null(input$LHprofile_click)){
      #     "Click on a point to display values - click in the middle of the horizontal scatter"
      #   }else(
      #     nearPoints(
      #       LH_off_react() %>%
      #         select(
      #           mouseID,
      #           num_ID,
      #           earlyLifeTrt,
      #           adultTrt,
      #           time,
      #           LH,
      #           ReproTract_mass
      #         ),
      #       input$LHprofile_click
      #     )
      #   )
      # })
      
      ## Corticosterone ----------------------------------------------------------
      Cort_off_males <- reactive({
        df <- Cort_off_react() %>%
          filter(
            sex == "M",
            damStrain == "CBA",
            !is.na(cort),
            !exclude
          )
        
        if(input$removeExtraMales){
          df <- df %>%
            filter(
              includeMaleCort
            )
        }
        return(df)
      })
      
      output$cortANOVA_males <- renderUI({
        Cort_off_males() %>%
          filter(
            ! (row_number() %in% input$cortPlot_males_table_rows_selected)
          ) %>%
          cortAnova() %>%
          htmltools_value()
      })
      
      cortPlot_males <- reactive({
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
      
      # output$cortPlot_males <- renderPlot({
      # })
      
      cortPlot_males_info <- plotServer("cortPlot_males", cortPlot_males, "cortPlot_males", compType)
      
      output$cortPlot_males_info <- renderPrint({
        if(is.null(cortPlot_males_info$click())){
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
                comboTrt,
                ReproTract_mass,
                time,
                cort
              ),
            # input$cortPlot_males_click
            cortPlot_males_info$click()
          )
          # cortPlot_males_info$click()$y
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
            damStrain == "CBA",
            !is.na(cort),
            !exclude
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
                comboTrt,
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
            damStrain == "CBA",
            !is.na(!! input$massVar)
          )
        if(input$removeExtraMales){
          df <- df %>%
            filter(
              includeMaleCort
            )
        }
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
          yVar == "maxLH" ~ "max evening LH (ng/mL)",
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
            damStrain == "CBA",
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

