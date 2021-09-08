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
    ),

    ### OLD ---------
    fluidRow(
      column(
        4,
        radioButtons(
          ns("WhichSex"),
          "Which sex?",
          c("Both", "Male" = "M", "Female" = "F"),
          selected = "Both"
        )
      ),
      column(
        4,
        #plot individual lines?
        checkboxInput(
          ns("Individual_lines"),
          "Plot individual lines?",
          value = TRUE
        ),
        #plot means?
        checkboxInput(
          ns("Mean_lines"),
          "Plot mean lines?",
          value = TRUE
        ),
        #plot by dam strain?
        checkboxInput(
          ns("By_strain"),
          "Plot by strain?",
          value = TRUE
        )

      ),
      column(
        4,
        #Add a title
        textInput(
          ns("Title"),
          "Graph Title:"
        ),
        #plot by dam? - can't diff treatments
        # checkboxInput(
        #   ns("By_dam"),
        #   "Plot by litter?",
        #   value = FALSE
        # )
      )
    ),


    zoomAxisUI(ns("zoom_x"), "x"),

    zoomAxisUI(ns("zoom_y"), "y"),

    #plot dam mass
    plotOutput(ns("Plot"), height = "600px"),

    h3("ANOVA"),

    tableOutput(ns("anova")),

    h4("Interaction of LBN and ALPS Treatment"),

    p("This explores the two-way ANOVA at each time point"),

    tableOutput(ns("twowayTreat")),

    # h4("Interaction of ALPS Treatment and Time"),
    #
    # p("This explores the two-way ANOVA at each level of early-life treatment"),
    #
    # tableOutput(ns("twoway")),
    #
    # h4("Effect of time"),
    #
    # p("This further explores the effect of time for each treatment group. p-value for significance needs to be adjusted for multiple comparison by Bonferroni"),
    #
    # tableOutput(ns("timeEffect")),
    #
    # h4("Pairwise Comparisons"),
    #
    # tableOutput(ns("pairwise")),

    h3("Summary Table"),

    # summaryTableUI(
    #   id = ns("ALPSSum"),
    #   df_sum = AcuteStress_off %>%
    #     select(Cort_pre:LH_5.5), #data frame with possible columns
    #   selected_sum = c("Cort_pre", "Cort_post"), # c(" ", " ") vector with selected variables
    #   df_group = AcuteStress_off %>%
    #     select(
    #       Sex:Treatment,
    #       Dam_ID,
    #       Dam_Strain:ParaType,
    #       Stress_treatment
    #     ),
    #   selected_group = c(
    #     "Treatment",
    #     "Stress_treatment"
    #   )
    # )

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
              Mouse_ID,
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
            Mouse_ID,
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
                Mouse_ID,
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
            Mouse_ID,
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
                Mouse_ID,
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
                Mouse_ID,
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
            Mouse_ID,
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
                Mouse_ID,
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
            Mouse_ID,
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
              Mouse_ID,
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
            Mouse_ID,
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
              Mouse_ID,
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
            Mouse_ID,
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
        if(length(input$summaryVars)>0 & length(AcuteStress_males_summary()$Mouse_ID) > 0){
          AcuteStress_males_summary() %>%
            group_by(earlyLifeTrt, adultTrt) %>%
            meanSummary(c(!!! input$summaryVars))
        }
      })
      output$summaryTable_females <- shiny::renderDataTable({
        if(length(input$summaryVars)>0 & length(AcuteStress_females_summary()$Mouse_ID) > 0){
          AcuteStress_females_summary() %>%
            group_by(earlyLifeTrt, adultTrt) %>%
            meanSummary(c(!!! input$summaryVars))
        }
      })
      
      ## Extra -------------------------------------------------------------------
      zoom_x <- zoomAxisServer("zoom_x", "x", minVal = 0, maxVal = 21)

      zoom_y <- zoomAxisServer("zoom_y", "y", minVal = 0, maxVal = 15)

      # AcuteStress_off_react <- reactive({
      #   #needs to be before the averaging by litter
      #   if(input$WhichSex == "M"){
      #     AcuteStress_off <- AcuteStress_off %>%
      #       filter(Sex == "M")
      #   }else if(input$WhichSex == "F"){
      #     AcuteStress_off <- AcuteStress_off %>%
      #       filter(Sex == "F")
      #   }
      #
      #   AcuteStress_off  <- AcuteStress_off %>%
      #     filter(!is.na(Cort_pre) & !is.na(Cort_post))
      #
      #   AcuteStress_off_react <- filteringDFServer("ALPS_filter", AcuteStress_off)
      #   return(AcuteStress_off_react())
      # })
      #
      # AcuteStress_off_long_react <- reactive({
      #   AcuteStress_off_long <- makeCortLong(AcuteStress_off_react())
      #
      #   AcuteStress_off_long %>%
      #     convert_as_factor(
      #       Mouse_ID, Time, Treatment, Stress_treatment
      #     )
      #
      #   AcuteStress_off_long$Time <- factor(
      #     AcuteStress_off_long$Time,
      #     levels = c("Cort_pre","Cort_post"),
      #     labels = c("pre", "post")
      #   )
      #
      #   return(AcuteStress_off_long)
      # })
      #
      # output$Plot <- renderPlot({
      #   stress_interaction_plot(AcuteStress_off_long_react(), Cort, "Cort (ng/mL)", plotMean = input$Mean_lines)
      # })
      #
      # ALPSSum <- summaryTableServer("ALPSSum", AcuteStress_off_react)
      #
      # output$anova <- renderTable({
      #   res.aov <- AcuteStress_off_long_react() %>%
      #     anova_test(
      #       dv = Cort,
      #       wid = Mouse_ID,
      #       within = Time,
      #       between = c(Treatment, Stress_treatment),
      #       type = 3
      #     )
      #
      #   return(get_anova_table(res.aov))
      # })
      #
      # output$twoway <- renderTable({
      #   two.way <- AcuteStress_off_long_react() %>%
      #     group_by(
      #       Treatment
      #     ) %>%
      #     anova_test(
      #       dv = Cort,
      #       wid = Mouse_ID,
      #       between = Stress_treatment,
      #       within = Time
      #     ) %>%
      #     get_anova_table() %>%
      #     adjust_pvalue(
      #       method = "bonferroni"
      #     )%>%
      #     add_significance("p.adj")
      #
      #   colnames(two.way)[1] <- "Early.Life"
      #
      #   return(two.way)
      # })
      # output$twowayTreat <- renderTable({
      #   two.way <- AcuteStress_off_long_react() %>%
      #     group_by(
      #       Time
      #     ) %>%
      #     anova_test(
      #       dv = Cort,
      #       between = c(Treatment, Stress_treatment)
      #     ) %>%
      #     get_anova_table() %>%
      #     adjust_pvalue(
      #       method = "bonferroni"
      #     )%>%
      #     add_significance("p.adj")
      #
      #   return(two.way)
      # })
      #
      # output$timeEffect <- renderTable({
      #   time.effect <- AcuteStress_off_long_react() %>%
      #     group_by(
      #       Treatment,
      #       Stress_treatment
      #     ) %>%
      #     anova_test(
      #       dv = Cort,
      #       wid = Mouse_ID,
      #       within = Time
      #     ) %>%
      #     get_anova_table() %>%
      #     adjust_pvalue(
      #       method = "bonferroni"
      #     )%>%
      #     add_significance("p.adj")
      #
      #
      #   colnames(time.effect)[1] <- "Early.Life"
      #   colnames(time.effect)[2] <- "Adult.Treatment"
      #
      #   return(time.effect)
      # })
      #
      # output$pairwise <- renderTable({
      #   pwc <- AcuteStress_off_long_react() %>%
      #     group_by(
      #       Treatment,
      #       Stress_treatment
      #     ) %>%
      #     pairwise_t_test(
      #       Cort ~ Time,
      #       paired = TRUE,
      #       p.adjust.method = "bonferroni"
      #     ) %>%
      #     select(
      #       -statistic,
      #       -df,
      #       -p
      #     )
      #
      #   colnames(pwc)[1] <- "Early.Life"
      #   colnames(pwc)[2] <- "Adult.Treatment"
      #
      #   pwc
      # })

    }
  )
}

