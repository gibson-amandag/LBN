#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(readr)
library(rlang)
library(purrr)
library(scales)
library(knitr)
#library(flextable) #error with data.table
library(officer)
library(GGally)
library(dplyr)
library(ggfortify)
library(openxlsx)
library(lubridate)

#Define paths in .Renviron on each computer
source("./Scripts/LBN_0002_AGG_setUp.R")

DFs <- load_LBN_data(
    dataFolder = DataFolder,
    excelName = LBN_DataName
)

Demo_dam <- DFs$Demo_dam
Demo_off <- DFs$Demo_off
Mass_off <- DFs$Mass_off
Maturation_off <- DFs$Maturation_off
EndPara_off <- DFs$EndPara_off
Cycles_off <- DFs$Cycles_off
AcuteStress_off <- DFs$AcuteStress_off
ChronicStress_off <- DFs$ChronicStress_off
LBN_all <- DFs$LBN_all
LBN_data <- DFs$LBN_data

Dam_dates <<- damDatesFunc(Demo_dam)

Off_dates <<- offDatesFunc(LBN_data)

blueText <<-  function(text){
    paste0("<span style='color:blue'>", text, "</span>")
}

LBN_varNames <<- LBN_varNames_func(LBN_all)

#Load Modules
source("./Scripts/AppScripts/zoomAxisModule.R")
source("./Scripts/AppScripts/taskTrackingModule.R")
source("./Scripts/AppScripts/rawDataModule.R")
source("./Scripts/AppScripts/damMassModule.R")
source("./Scripts/AppScripts/pupLossModule.R")
source("./Scripts/AppScripts/damCortModule.R")


# Define UI for application that draws a histogram
ui <- navbarPage("LBN",
                 
                 ### TASK TRACKING PANEL ----------------------
                 tabPanel(
                     "Tasks",
                     fluidPage(
                         taskTrackingUI("tasks")
                     )
                 ),
                 
                 ### DATA FRAMES -----------------------
                 tabPanel(
                     "Data",
                     rawDataUI("rawData",
                               Demo_dam,
                               LBN_data)
                 ),
                 
                 ### ANALYSIS-------------------------
                 tabPanel(
                     "Analysis",
                     titlePanel("LBN Analysis"),
                     h2("Offspring Date of Birth"),
                     plotOutput("Offspring_DOB_hist",
                                height = "200px"),
                     uiOutput("Offspring_DOB_range"),
                     
                     tabsetPanel(
                     
                     #Dams --------
                     tabPanel("Dam",
                              tabsetPanel(
                                  
                                  #Mass
                                  tabPanel(
                                      "Dam Mass",
                                      damMassUI("damMass", Demo_dam)
                                  ),
                                  
                                  #Pup Loss
                                  tabPanel(
                                      "Pup Loss",
                                      pupLossUI("pupLoss", Demo_dam)
                                  ),
                                  
                                  #Corticosterone
                                  tabPanel(
                                      "Dam Corticosterone",
                                      damCortUI("damCort")
                                      # h3("Dam Corticosterone on P21"),
                                      # 
                                      # fluidRow(
                                      #     column(4,
                                      #            radioButtons("Cort_dams_ParaTypes",
                                      #                         "Which paradigm type?",
                                      #                         c("Both", "P2-P9" = 2, "P4-P11" = 4),
                                      #                         selected = "Both")
                                      #     ),
                                      #     column(4,
                                      #            radioButtons("Cort_dams_whichStrain",
                                      #                         "Which dam strains?",
                                      #                         c("Both", "B6", "CBA"))
                                      #     ),
                                      #     column(4,
                                      #            dateRangeInput("Cort_dams_DOBs",
                                      #                           "Select range of birth dates",
                                      #                           start = "2019-12-15",
                                      #                           end = Sys.Date())
                                      #     )
                                      # ),
                                      # 
                                      # fluidRow(
                                      #     column(4,
                                      #            checkboxInput("Cort_dams_zoom_y",
                                      #                          "Zoom y axis?")),
                                      #     column(4,
                                      #            conditionalPanel(
                                      #                condition = "input.Cort_dams_zoom_y == true",
                                      #                numericInput("Cort_dams_ymin",
                                      #                             "Lower Limit y-axis:",
                                      #                             0)
                                      #                )),
                                      #     column(4,
                                      #            conditionalPanel(
                                      #                condition = "input.Cort_dams_zoom_y == true",
                                      #                numericInput("Cort_dams_ymax",
                                      #                             "Upper Limit y-axis:",
                                      #                             15))   
                                      #     )
                                      #     ),
                                      # #add filtering options
                                      # plotOutput("Dam_cort21")
                                  )
                              ),
                              ),
                     
                     #Offspring Mass -----------
                     tabPanel("Offspring Mass",
                              
                              h3("Offspring Mass"),
                              fluidRow(
                                  column(4,
                                         varSelectInput("Mass_vars_to_sum",
                                                        "Select variables to summarize",
                                                        data = Mass_off %>%
                                                            select(Avg_litter_mass_startPara:Mass_P72),
                                                        selected = c("Mass_P2",
                                                                     "Mass_P4",
                                                                     "Mass_P9",
                                                                     "Mass_P11"),
                                                        multiple = TRUE)
                                         ),
                                  column(4,
                                         varSelectInput("Mass_grouping_vars",
                                                        "Select variables to group by",
                                                        data = Mass_off %>%
                                                            select(Sex:Treatment,
                                                                   Dam_ID,
                                                                   Dam_Strain:ParaType),
                                                        selected = c("Treatment",
                                                                     "Dam_Strain"),
                                                        multiple = TRUE))
                              ),
                              dataTableOutput("Mass_off_summary"),
                              fluidRow(
                                 column(4,
                                        radioButtons("Mass_off_ParaTypes",
                                              "Which paradigm type?",
                                              c("Both", "P2-P9" = 2, "P4-P11" = 4),
                                              selected = "Both"),
                                        radioButtons("Mass_off_whichStrain",
                                                     "Which dam strains?",
                                                     c("Both", "B6", "CBA"),
                                                     selected = "Both"),
                                        radioButtons("Mass_off_whichSex",
                                                     "Which sex?",
                                                     c("Both", "Male" = "M", "Female" = "F"),
                                                     selected = "Both")
                                        ),
                                 column(4,
                                        checkboxInput("Mass_off_by_dam",
                                                      "Plot by litter?",
                                                      value = FALSE),
                                        checkboxInput("Mass_off_by_strain",
                                                      "Plot by strain?",
                                                      value = TRUE),
                                        checkboxInput("Mass_off_individual_lines",
                                                      "Plot individual lines?",
                                                      value = TRUE),
                                        checkboxInput("Mass_off_mean_lines",
                                                      "Plot mean lines?",
                                                      value = TRUE),
                                        textInput("Mass_off_title",
                                                  "Graph Title:"),
                                        dateRangeInput("Mass_off_DOBs",
                                                       "Select range of birth dates",
                                                       start = "2019-12-15",
                                                       end = Sys.Date())
                                        ),
                                 column(2,
                                        checkboxInput("Mass_off_zoom_x",
                                                      "Zoom x axis?"),
                                        conditionalPanel(
                                            condition = "input.Mass_off_zoom_x == true",
                                            numericInput("Mass_off_xmin",
                                                         "Lower Limit x-axis:",
                                                         0),
                                            numericInput("Mass_off_xmax",
                                                         "Upper Limit x-axis:",
                                                         21)
                                        )
                                        ),
                                 column(2,
                                        checkboxInput("Mass_off_zoom_y",
                                                      "Zoom y axis?"),
                                        conditionalPanel(
                                            condition = "input.Mass_off_zoom_y == true",
                                            numericInput("Mass_off_ymin",
                                                         "Lower Limit y-axis:",
                                                         0),
                                            numericInput("Mass_off_ymax",
                                                         "Upper Limit y-axis:",
                                                         15))   
                                        )
                                 
                              ),
                              plotOutput("Mass_off_plot",
                                         height = "600px")
                              ),
                     ### Offspring Maturation ----
                     tabPanel("Offspring Maturation",
                              h3("Offspring Maturation"),
                              
                              ### Options for filtering for both freq plots and dot plots ----
                              
                              fluidRow(
                                  column(4,
                                         radioButtons("Mat_ParaTypes",
                                                      "Which paradigm type?",
                                                      c("Both", "P2-P9" = 2, "P4-P11" = 4),
                                                      selected = "Both")
                                         ),
                                  column(4,
                                         radioButtons("Mat_whichStrain",
                                                      "Which dam strains?",
                                                      c("Both", "B6", "CBA"))
                                         ),
                                  column(4,
                                         dateRangeInput("Mat_DOBs",
                                                        "Select range of birth dates",
                                                        start = "2019-12-15",
                                                        end = Sys.Date())
                                  )
                                  ),
                              
                              tabsetPanel(
                                  tabPanel("Plots",
                                           
                                           ### Cumulative Frequency Plots ----
                                           h4("Cumulative Frequency Plots"),
                                           fluidRow(
                                               column(4,
                                                      checkboxInput("Mat_cumFreq_same",
                                                                    "Same x-axis?",
                                                                    value = TRUE)),
                                               column(4,
                                                      conditionalPanel(
                                                          condition = "input.Mat_cumFreq_same == true",
                                                          numericInput("Mat_cumFreq_xmax",
                                                                       "Enter x-axis max:",
                                                                       value = 50))
                                               )
                                           ),
                                           fluidRow(
                                               column(4,
                                                      plotOutput("Mat_VO_cumFreq")),
                                               column(4,
                                                      plotOutput("Mat_1E_cumFreq")),
                                               column(4,
                                                      plotOutput("Mat_PPS_cumFreq"))
                                           ),
                                           
                                           #Dot Plots ----
                                           h4("Puberty Dot Plots - Age"),
                                           
                                           fluidRow(
                                               column(4,
                                                      checkboxInput("Mat_dot_same",
                                                                    "Same y-axis?",
                                                                    value = TRUE)),
                                               column(4,
                                                      conditionalPanel(
                                                          condition = "input.Mat_dot_same == true",
                                                          numericInput("Mat_dot_ymax",
                                                                       "Enter y-axis max:",
                                                                       value = 50))
                                               )
                                           ),
                                           
                                           fluidRow(
                                               column(4,
                                                      plotOutput("Mat_VO_dot")
                                                      ),
                                               column(4,
                                                      plotOutput("Mat_1E_dot")
                                                      ),
                                               column(4,
                                                      plotOutput("Mat_PPS_dot")
                                                      )
                                           ),
                                           
                                           h4("Puberty Dot Plots - Mass"),
                                           
                                           fluidRow(
                                               column(4,
                                                      checkboxInput("Mat_dot_mass_same",
                                                                    "Same y-axis?",
                                                                    value = TRUE)),
                                               column(4,
                                                      conditionalPanel(
                                                          condition = "input.Mat_dot_mass_same == true",
                                                          numericInput("Mat_dot_mass_ymax",
                                                                       "Enter y-axis max:",
                                                                       value = 25))
                                               )
                                           ),
                                           
                                           fluidRow(
                                               column(4,
                                                      plotOutput("Mat_VO_dot_mass")),
                                               column(4,
                                                      plotOutput("Mat_1E_dot_mass")),
                                               column(4,
                                                      plotOutput("Mat_PPS_dot_mass"))
                                           ),),
                                  
                                  tabPanel("AGD Summary",
                                           h4("Ano-genital distance"),
                                           fluidRow(
                                               column(4,
                                                      varSelectInput("Mat_AGD_vars_to_sum",
                                                                     "Select variables to summarize",
                                                                     data = Maturation_off %>%
                                                                         select(AGD_wean:AGD_adult_by_mass, AGD_P22:AGD_P72),
                                                                     selected = c("AGD_wean",
                                                                                  "AGD_adult",
                                                                                  "Mass_P9",
                                                                                  "Mass_P11"),
                                                                     multiple = TRUE)
                                               ),
                                               column(4,
                                                      varSelectInput("Mat_AGD_grouping_vars",
                                                                     "Select variables to group by",
                                                                     data = Maturation_off %>%
                                                                         select(Sex:Treatment,
                                                                                Dam_ID,
                                                                                Dam_Strain:ParaType),
                                                                     selected = c("Sex",
                                                                                  "Treatment"),
                                                                     multiple = TRUE))
                                           ),
                                           dataTableOutput("Mat_AGD_summary")
                                           ),
                                  
                                  tabPanel("VO Summary",
                                           h4("Vaginal Opening"),
                                           fluidRow(
                                               column(4,
                                                      varSelectInput("Mat_VO_vars_to_sum",
                                                                     "Select variables to summarize",
                                                                     data = Maturation_off %>%
                                                                         select(VO_age, VO_mass),
                                                                     selected = c("VO_age", "VO_mass"),
                                                                     multiple = TRUE)
                                               ),
                                               column(4,
                                                      varSelectInput("Mat_VO_grouping_vars",
                                                                     "Select variables to group by",
                                                                     data = Maturation_off %>%
                                                                         select(Treatment,
                                                                                Dam_ID,
                                                                                Dam_Strain:ParaType),
                                                                     selected = c("Treatment"),
                                                                     multiple = TRUE))
                                           ),
                                           dataTableOutput("Mat_VO_summary"),
                                           verbatimTextOutput("Mat_VO_tTest")
                                           ),
                                  
                                  tabPanel("1E Summary",
                                           h4("First Estrus"),
                                           fluidRow(
                                               column(4,
                                                      varSelectInput("Mat_1E_vars_to_sum",
                                                                     "Select variables to summarize",
                                                                     data = Maturation_off %>%
                                                                         select(Estrus_age, Estrus_mass),
                                                                     selected = c("Estrus_age", "Estrus_mass"),
                                                                     multiple = TRUE)
                                               ),
                                               column(4,
                                                      varSelectInput("Mat_1E_grouping_vars",
                                                                     "Select variables to group by",
                                                                     data = Maturation_off %>%
                                                                         select(Treatment,
                                                                                Dam_ID,
                                                                                Dam_Strain:ParaType),
                                                                     selected = c("Treatment"),
                                                                     multiple = TRUE))
                                           ),
                                           dataTableOutput("Mat_1E_summary"),
                                           verbatimTextOutput("Mat_1E_tTest")
                                           ),
                                  
                                  tabPanel("PPS Summary",
                                           h4("Preputial Separation"),
                                           fluidRow(
                                               column(4,
                                                      varSelectInput("Mat_PPS_vars_to_sum",
                                                                     "Select variables to summarize",
                                                                     data = Maturation_off %>%
                                                                         select(PreputialSep_age, PreputialSep_mass),
                                                                     selected = c("PreputialSep_age", "PreputialSep_mass"),
                                                                     multiple = TRUE)
                                               ),
                                               column(4,
                                                      varSelectInput("Mat_PPS_grouping_vars",
                                                                     "Select variables to group by",
                                                                     data = Maturation_off %>%
                                                                         select(Treatment,
                                                                                Dam_ID,
                                                                                Dam_Strain:ParaType),
                                                                     selected = c("Treatment"),
                                                                     multiple = TRUE))
                                           ),
                                           dataTableOutput("Mat_PPS_summary"),
                                           verbatimTextOutput("Mat_PPS_tTest")
                                           )
                              ) #End tabset panel within Off Maturation
                              ), #end offspring maturation tabPanel
                     
                     ### Offspring Corticosterone ----
                     tabPanel("Acute Stress Paradigm",
                              h3("Corticosterone")
                              ), #End off cort panel
                     
                     ### Offspring Cycles ----
                     tabPanel("Offspring Cycles",
                              h3("Offspring Cycles"),
                              
                              fluidRow(
                                  column(4,
                                         radioButtons("Cycles_off_ParaTypes",
                                                      "Which paradigm type?",
                                                      c("Both", "P2-P9" = 2, "P4-P11" = 4),
                                                      selected = "Both")
                                  ),
                                  column(4,
                                         radioButtons("Cycles_off_whichStrain",
                                                      "Which dam strains?",
                                                      c("Both", "B6", "CBA"))
                                  ),
                                  column(4,
                                         dateRangeInput("Cycles_off_DOBs",
                                                        "Select range of birth dates",
                                                        start = "2019-12-15",
                                                        end = Sys.Date())
                                  )
                              ),
                              
                              h4("Control offspring"),
                              plotOutput("Cycles_off_control_plot",
                                         height = "600px"),
                              
                              h4("LBN offspring"),
                              plotOutput("Cycles_off_LBN_plot",
                                         height = "600px")
                              ) #End cycles tabPanel
                     ) #end analysis tabsetPanel
                 ### END ANALYSIS ----    
                 ) #end analysis tabPanel
############                 
)

############# SERVER #########################################################
server <- function(input, output) {
    ### OFFSPRING DOB ----------------------
    output$Offspring_DOB_hist <- renderPlot(
        ggplot(Mass_off %>% filter(!is.na(DOB)), aes(DOB)) +
            geom_histogram(binwidth = 7)+
            labs(x = "Date of Birth", y = "# of mice")+
            my_theme
    )
    
    output$Offspring_DOB_range <- renderUI({
        str <-  paste("<h4> The range of offspring DOBs is from", blueText(min(Mass_off$DOB, na.rm = TRUE)), 
                      "to", blueText(max(Mass_off$DOB, na.rm = TRUE)), "</h4>")
        HTML(str)
    })
    
    #### TASK TRACKING HTML TEXT------------------
    taskTrackingServer("tasks")
    
    #### RENDER DATA FRAMES----------------------
    rawDataServer("rawData",
                  Demo_dam,
                  Demo_off,
                  Mass_off,
                  Maturation_off,
                  EndPara_off,
                  Cycles_off,
                  AcuteStress_off,
                  ChronicStress_off,
                  LBN_data)
    
    #### ANALYSIS MODULES ----------------------
    damMassServer("damMass", Demo_dam)
    
    pupLossServer("pupLoss", Demo_dam)
    
    damCortServer("damCort", Demo_dam)

    #### RENDER ANALYSIS -----------------------------
    ### Summary Data Frames -----------------
    output$Mass_off_summary <- renderDataTable(
        map_dfr(input$Mass_vars_to_sum, LBN_summary_byGroup, Mass_off, input$Mass_grouping_vars)
    )
    
    #Add filters for paradigm types, birth dates, etc
    output$Mat_AGD_summary <- renderDataTable(
        map_dfr(input$Mat_AGD_vars_to_sum, LBN_summary_byGroup, Maturation_off_forPlots(), input$Mat_AGD_grouping_vars)
    )
    
    output$Mat_VO_summary <- renderDataTable(
        map_dfr(input$Mat_VO_vars_to_sum, LBN_summary_byGroup, Maturation_off_forPlots(), input$Mat_VO_grouping_vars)
    )
    
    output$Mat_1E_summary <- renderDataTable(
        map_dfr(input$Mat_1E_vars_to_sum, LBN_summary_byGroup, Maturation_off_forPlots(), input$Mat_1E_grouping_vars)
    )
    
    output$Mat_PPS_summary <- renderDataTable(
        map_dfr(input$Mat_PPS_vars_to_sum, LBN_summary_byGroup, Maturation_off_forPlots(), input$Mat_PPS_grouping_vars)
    )
    
    ### Plots ------------------
    
    # #Dam Cort Plot
    # output$Dam_cort21 <- renderPlot({
    #     Cort_dams <- Demo_dam %>%
    #         drop_na(Treatment, Cort_dam_P21)
    #     
    #     #Filter for paradigm type
    #     if(input$Cort_dams_ParaTypes == 2){
    #         Cort_dams <- Cort_dams %>%
    #             filter(ParaType == 2)
    #     }else if(input$Cort_dams_ParaTypes == 4){
    #         Cort_dams <- Cort_dams %>%
    #             filter(ParaType == 4)
    #     }
    #     
    #     #Filter for DOB
    #     Cort_dams <- Cort_dams %>%
    #         filter(DOB >= input$Cort_dams_DOBs[1] & DOB <= input$Cort_dams_DOBs[2])
    #     
    #     #Filter for Strain - By Dam Strain
    #     if(input$Cort_dams_whichStrain == "B6"){
    #         Cort_dams <- Cort_dams %>%
    #             filter(Dam_Strain == "B6")
    #     }else if(input$Cort_dams_whichStrain == "CBA"){
    #         Cort_dams <- Cort_dams %>%
    #             filter(Dam_Strain == "CBA")
    #     }
    #     
    #     my_puberty_dot_plot(
    #         df = Cort_dams,
    #         var_to_plot = expr(Cort_dam_P21), #expr()
    #         phenotype_name = NULL,
    #         shape = expr(Dam_Strain),
    #         colour = expr(Dam_Strain),
    #         width = 0.3,
    #         change_ymax = FALSE,
    #         ymax = NA,
    #         alt_ytitle = TRUE,
    #         ytitle = "Corticosterone (ng/mL)" #alternative y title
    #     )
    # })
    
    #Offspring Mass Plot
    output$Mass_off_plot <- renderPlot({
        #needs to be before the averaging by litter
        if(input$Mass_off_whichSex == "M"){
            Mass_off <- Mass_off %>%
                filter(Sex == "M")
        }else if(input$Mass_off_whichSex == "F"){
            Mass_off <- Mass_off %>%
                filter(Sex == "F")
        }
        
        if(input$Mass_off_by_dam == FALSE){
            Mass_off_long <- reshapeForMassPlot(Mass_off)
        }
        
        if(input$Mass_off_by_dam == TRUE){
            Mass_off_long <- Mass_off %>%
                getAvgByDam(Demo_dam) %>%
                reshapeForMassPlot()
        }
        
        #Filter for paradigm type
        if(input$Mass_off_ParaTypes == 2){
            Mass_off_long <- Mass_off_long %>%
                filter(ParaType == 2)
        }else if(input$Mass_off_ParaTypes == 4){
            Mass_off_long <- Mass_off_long %>%
                filter(ParaType == 4)
        }
        
        #Filter for DOB
        Mass_off_long <- Mass_off_long %>%
            filter(DOB >= input$Mass_off_DOBs[1] & DOB <= input$Mass_off_DOBs[2])
       
        #Filter for Strain - By Dam Strain
        if(input$Mass_off_whichStrain == "B6"){
            Mass_off_long <- Mass_off_long %>%
                filter(Dam_Strain == "B6")
        }else if(input$Mass_off_whichStrain == "CBA"){
            Mass_off_long <- Mass_off_long %>%
                filter(Dam_Strain == "CBA")
        }
        
        
        mass_plot_lines(Mass_off_long,
                        line_group = ifelse(input$Mass_off_by_dam, expr(Dam_ID), expr(Mouse_ID)),
                        by_strain = input$Mass_off_by_strain,
                        individualLines = input$Mass_off_individual_lines,
                        mean_lines = input$Mass_off_mean_lines,
                        title = input$Mass_off_title,
                        zoom_x = input$Mass_off_zoom_x,
                        xmin = input$Mass_off_xmin,
                        xmax = input$Mass_off_xmax,
                        zoom_y = input$Mass_off_zoom_y,
                        ymin = input$Mass_off_ymin,
                        ymax = input$Mass_off_ymax)
    })
    
    #This creates a reactive data frame for all of the cumulative frequency and puberty dot plots.
    #Don't need same filtering code for all of the plots
    #Call by using Maturation_off_forPlots()
    Maturation_off_forPlots <- reactive({
        Maturation_off_forPlots <- Maturation_off
        #Filter for paradigm type
        if(input$Mat_ParaTypes == 2){
            Maturation_off_forPlots <- Maturation_off_forPlots %>%
                filter(ParaType == 2)
        }else if(input$Mat_ParaTypes == 4){
            Maturation_off_forPlots <- Maturation_off_forPlots %>%
                filter(ParaType == 4)
        }
        
        #Filter for DOB
        Maturation_off_forPlots <- Maturation_off_forPlots %>%
            filter(DOB >= input$Mat_DOBs[1] & DOB <= input$Mat_DOBs[2])
        
        #Filter for Strain - By Dam Strain
        if(input$Mat_whichStrain == "B6"){
            Maturation_off_forPlots <- Maturation_off_forPlots %>%
                filter(Dam_Strain == "B6")
        }else if(input$Mat_whichStrain == "CBA"){
            Maturation_off_forPlots <- Maturation_off_forPlots %>%
                filter(Dam_Strain == "CBA")
        }
        
        return(Maturation_off_forPlots)
    })
    
    #Cumulative Frequency Plots
    output$Mat_VO_cumFreq <- renderPlot({
        my_cumulative_freq_plot(df = Maturation_off_forPlots(),
                                color_var = expr(Treatment),
                                linetype_var = expr(Dam_Strain),
                                var_to_plot = expr(VO_age), #as expr()
                                phenotype_name = "VO", #string
                                title = TRUE,
                                change_xmax = input$Mat_cumFreq_same,
                                xmax = input$Mat_cumFreq_xmax,
                                xmin = 21)
    })
    
    output$Mat_1E_cumFreq <- renderPlot({
        my_cumulative_freq_plot(df = Maturation_off_forPlots(),
                                color_var = expr(Treatment),
                                linetype_var = expr(Dam_Strain),
                                var_to_plot = expr(Estrus_age), #as expr()
                                phenotype_name = "First Estrus", #string
                                title = TRUE,
                                change_xmax = input$Mat_cumFreq_same,
                                xmax = input$Mat_cumFreq_xmax,
                                xmin = 21)
    })
    
    output$Mat_PPS_cumFreq <- renderPlot({
        my_cumulative_freq_plot(df = Maturation_off_forPlots(),
                                color_var = expr(Treatment),
                                linetype_var = expr(Dam_Strain),
                                var_to_plot = expr(PreputialSep_age), #as expr()
                                phenotype_name = "PPS", #string
                                title = TRUE,
                                change_xmax = input$Mat_cumFreq_same,
                                xmax = input$Mat_cumFreq_xmax,
                                xmin = 21)
    })
    
    #Puberty Dot Plots
    output$Mat_VO_dot <- renderPlot({
        my_puberty_dot_plot(
            df = Maturation_off_forPlots(),
            expr(VO_age), #expr()
            phenotype_name = "VO",
            shape = expr(Dam_Strain),
            colour = expr(Dam_Strain),
            width = 0.3,
            change_ymax = input$Mat_dot_same,
            ymax = input$Mat_dot_ymax,
            DaysOrMass = "Days"
        )
    })
    
    output$Mat_1E_dot <- renderPlot({
        my_puberty_dot_plot(
            df = Maturation_off_forPlots(),
            expr(Estrus_age), #expr()
            phenotype_name = "First Estrus",
            shape = expr(Dam_Strain),
            colour = expr(Dam_Strain),
            width = 0.3,
            change_ymax = input$Mat_dot_same,
            ymax = input$Mat_dot_ymax,
            DaysOrMass = "Days"
        )
    })
    
    output$Mat_PPS_dot <- renderPlot({
        my_puberty_dot_plot(
            df = Maturation_off_forPlots(),
            expr(PreputialSep_age), #expr()
            phenotype_name = "PPS",
            shape = expr(Dam_Strain),
            colour = expr(Dam_Strain),
            width = 0.3,
            change_ymax = input$Mat_dot_same,
            ymax = input$Mat_dot_ymax,
            DaysOrMass = "Days"
        )
    })
    
    output$Mat_VO_dot_mass <- renderPlot({
        my_puberty_dot_plot(
            df = Maturation_off_forPlots(),
            expr(VO_mass), #expr()
            phenotype_name = "VO",
            shape = expr(Dam_Strain),
            colour = expr(Dam_Strain),
            width = 0.3,
            change_ymax = input$Mat_dot_mass_same,
            ymax = input$Mat_dot_mass_ymax,
            DaysOrMass = "Mass"
        )
    })
    
    output$Mat_1E_dot_mass <- renderPlot({
        my_puberty_dot_plot(
            df = Maturation_off_forPlots(),
            expr(Estrus_mass), #expr()
            phenotype_name = "First Estrus",
            shape = expr(Dam_Strain),
            colour = expr(Dam_Strain),
            width = 0.3,
            change_ymax = input$Mat_dot_mass_same,
            ymax = input$Mat_dot_mass_ymax,
            DaysOrMass = "Mass"
        )
    })
    
    output$Mat_PPS_dot_mass <- renderPlot({
        my_puberty_dot_plot(
            df = Maturation_off_forPlots(),
            expr(PreputialSep_mass), #expr()
            phenotype_name = "PPS",
            shape = expr(Dam_Strain),
            colour = expr(Dam_Strain),
            width = 0.3,
            change_ymax = input$Mat_dot_mass_same,
            ymax = input$Mat_dot_mass_ymax,
            DaysOrMass = "Mass"
        )
    })
    
    #Puberty t-tests
    output$Mat_VO_tTest <- renderPrint({
        t.test(VO_age ~ Treatment, Maturation_off_forPlots())
    })
    
    output$Mat_1E_tTest <- renderPrint({
        t.test(Estrus_age ~ Treatment, Maturation_off_forPlots()) 
    })
    
    output$Mat_PPS_tTest <- renderPrint({
        t.test(PreputialSep_age ~ Treatment, Maturation_off_forPlots())
    })
    
    #Offspring Cycles Plots
    Cycles_off_long <- reactive({
        #Filter for paradigm type
        if(input$Cycles_off_ParaTypes == 2){
            Cycles_off <- Cycles_off %>%
                filter(ParaType == 2)
        }else if(input$Cycles_off_ParaTypes == 4){
            Cycles_off <- Cycles_off %>%
                filter(ParaType == 4)
        }
        
        #Filter for DOB
        Cycles_off <- Cycles_off %>%
            filter(DOB >= input$Cycles_off_DOBs[1] & DOB <= input$Cycles_off_DOBs[2])
        
        #Filter for Strain - By Dam Strain
        if(input$Cycles_off_whichStrain == "B6"){
            Cycles_off <- Cycles_off %>%
                filter(Dam_Strain == "B6")
        }else if(input$Cycles_off_whichStrain == "CBA"){
            Cycles_off <- Cycles_off %>%
                filter(Dam_Strain == "CBA")
        }
        
        Cycles_off_long <- make_cycles_long(Cycles_off) %>%
            add_Day_col() %>%
            drop_na(Stage)
        return(Cycles_off_long)
    })
    
    output$Cycles_off_control_plot <- renderPlot({
        Cycles_off_long() %>%
            filter(Treatment == "Control") %>%
            cyclesPlotFunc()
    })
    
    output$Cycles_off_LBN_plot <- renderPlot({
        Cycles_off_long() %>%
            filter(Treatment == "LBN") %>%
            cyclesPlotFunc()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
