#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Old ---------------------------------------------------------------------


# library(tidyverse)
# library(readr)
# library(rlang)
# library(purrr)
# library(scales)
# library(knitr)
# #library(flextable) #error with data.table
# library(officer)
# library(GGally)
# library(dplyr)
# library(ggfortify)
# library(openxlsx)
# library(lubridate)
# 
# #Define paths in .Renviron on each computer
# source("./Scripts/LBN_0002_AGG_setUp.R")
# 
# DFs <- load_LBN_data(
#     dataFolder = DataFolder,
#     excelName = LBN_DataName
# )
# 
# Demo_dam <- DFs$Demo_dam
# Demo_dam_P2 <- Demo_dam %>%
#     filter(ParaType == 2)
# Demo_dam_P4 <- Demo_dam %>%
#     filter(ParaType == 4)
# 
# Demo_off <- DFs$Demo_off
# Demo_off_P2 <- Demo_off %>%
#     filter(ParaType == 2)
# Demo_off_P4 <- Demo_off %>%
#     filter(ParaType == 4)
# 
# Mass_off <- DFs$Mass_off
# Mass_off_P2 <- Mass_off %>%
#     filter(ParaType == 2)
# Mass_off_P4 <- Mass_off %>%
#     filter(ParaType == 4)
# 
# Maturation_off <- DFs$Maturation_off
# Maturation_off_P2 <- Maturation_off %>%
#     filter(ParaType == 2)
# Maturation_off_P4 <- Maturation_off %>%
#     filter(ParaType == 4)
# 
# EndPara_off <- DFs$EndPara_off
# EndPara_off_P2 <- EndPara_off %>%
#     filter(ParaType == 2)
# EndPara_off_P4 <- EndPara_off %>%
#     filter(ParaType == 4)
# 
# Cycles_off <- DFs$Cycles_off
# Cycles_off_P2 <- Cycles_off %>%
#     filter(ParaType == 2)
# Cycles_off_P4 <- Cycles_off %>%
#     filter(ParaType == 4)
# 
# AcuteStress_off <- DFs$AcuteStress_off
# AcuteStress_off_P2 <- AcuteStress_off %>%
#     filter(ParaType == 2)
# AcuteStress_off_P4 <- AcuteStress_off %>%
#     filter(ParaType == 4)
# 
# ChronicStress_off <- DFs$ChronicStress_off
# ChronicStress_off_P2 <- ChronicStress_off %>%
#     filter(ParaType ==2)
# ChronicStress_off_P4 <- ChronicStress_off %>%
#     filter(ParaType == 4)
# 
# LBN_all <- DFs$LBN_all
# LBN_all_P2 <- LBN_all %>%
#     filter(ParaType == 2)
# LBN_all_P4 <- LBN_all %>%
#     filter(ParaType == 4)
# 
# LBN_data <- DFs$LBN_data
# LBN_data_P2 <- LBN_data %>%
#     filter(ParaType == 2)
# LBN_data_P4 <- LBN_data %>%
#     filter(ParaType == 4)
# 
# Dam_CRH <- DFs$Dam_CRH
# 
# Dam_dates <<- damDatesFunc(Demo_dam)
# Dam_dates_CRH <- damDatesFunc_CRH(Dam_CRH)
# 
# Off_dates <<- offDatesFunc(LBN_data)
# 
# LBN_varNames <<- LBN_varNames_func(LBN_all)
# 
# sourceModule <- function(scriptName){
#     source(file.path(".", "Scripts", "AppScripts", scriptName))
# }
# 
# #Load Modules
# sourceModule("zoomAxisModule.R")
# 
# sourceModule("filteringDFModule.R")
# sourceModule(file.path("P2-9", "filteringDFModule_P2_9.R"))
# 
# sourceModule("summaryTableModule.R")
# 
# sourceModule("taskTrackingModule.R")
# 
# sourceModule("rawDataModule.R")
# 
# sourceModule("massDamModule.R")
# sourceModule(file.path("P2-9", "massDamModule_P2_9.R"))
# 
# sourceModule("pupLossModule.R")
# 
# sourceModule("damCortModule.R")
# 
# sourceModule("massOffModule.R")
# sourceModule("massRegModule.R")
# sourceModule(file.path("P2-9", "massOffModule_P2_9.R"))
# 
# sourceModule("maturationOffModule.R")
# sourceModule("maturationRegModule.R")
# sourceModule(file.path("P2-9", "maturationOffModule_P2_9.R"))
# 
# sourceModule("acuteStressModule.R")
# 
# sourceModule("cyclesModule.R")


# New section -------------------------------------------------------------

source("./01-scripts/01-set-up.R")
source(file.path(scriptsFolder, "02-get-datasets.R"))

moduleFiles <- list.files(
    appScriptsFolder, 
    full.names = TRUE,
    recursive = TRUE, 
    pattern = "*.R"
)

sapply(moduleFiles, source)


# Define UI for application
ui <- navbarPage(
    "LBN",
    
    tabPanel(
        "Analysis",
        titlePanel("LBN Analysis"),
        tabsetPanel(
            ### Dams --------
            tabPanel(
                "Dam",
                tabsetPanel(
                    
                    #Mass
                    tabPanel(
                        "Dam Mass",
                        # massDamUI("massDam", Demo_dam_P4)
                    ),
                    
                    #Pup Loss
                    tabPanel(
                        "Pup Loss",
                        # pupLossUI("pupLoss", Demo_dam_P4)
                    ),
                    
                    #Corticosterone
                    tabPanel(
                        "Dam Corticosterone",
                        # damCortUI("damCort", Demo_dam_P4)
                    )
                )
            ),
            
            ### Offspring Mass -----------
            tabPanel(
                "Offspring Mass",
                # massOffUI("massOff", Mass_off_P4)
            ),
            ### Offspring Maturation ----
            tabPanel(
                "Offspring Maturation",
                # maturationOffUI("maturationOff", Maturation_off_P4, LBN_data %>% filter(ParaType == 4))
            ),
            
            ### Offspring Corticosterone ----
            tabPanel(
                "Acute Stress Paradigm",
                acuteStressUI("acuteStress", AcuteStress_off)
            ), #End off cort panel
            
            ### Offspring Cycles ----
            tabPanel("Offspring Cycles",
                     cyclesUI("cycles")
            ), #End cycles tabPanel
            tabPanel(
                "Sampling PPTs",
                samplingPPTsUI("samplingPPTs")
            ) #End sampling PPTs tabPanel
        ) #end analysis tabsetPanel
    ),

    ## TASK TRACKING PANEL ----------------------
    tabPanel(
        "Tasks",
        fluidPage(
            titlePanel("Limited Bedding and Nesting Task Tracking"),
            tabsetPanel(
                tabPanel(
                    "Task List",
                    # taskTrackingUI("tasks")
                ),
                tabPanel(
                    "Task Table",
                    # taskTableUI("taskTable")
                )
            )
        )
    ),
    
    ## DATA FRAMES -----------------------
    tabPanel(
        "Data",
        # rawDataUI(
        #     "rawData",
        #     Demo_dam,
        #     LBN_data,
        #     Dam_CRH)
    )

############
)

############# SERVER #########################################################
server <- function(input, output) {
    ### OFFSPRING DOB ----------------------
    # 
    # #### TASK TRACKING HTML TEXT------------------
    # taskTrackingServer("tasks", Dam_dates, Dam_dates_CRH, Off_dates)
    # taskTableServer("taskTable", Dam_dates, Dam_dates_CRH, Off_dates)
    # 
    # #### RENDER DATA FRAMES----------------------
    # rawDataServer(
    #     "rawData",
    #     Demo_dam,
    #     Demo_off,
    #     Mass_off,
    #     Maturation_off,
    #     EndPara_off,
    #     Cycles_off,
    #     AcuteStress_off,
    #     ChronicStress_off,
    #     LBN_data,
    #     Dam_CRH
    # )
    # 
    # #### ANALYSIS MODULES ----------------------
    # #P2-P9
    # massDam_P2_9_Server("massDam_P2_9", Demo_dam_P2)
    # massDam_P2_9_Server("massDam_P2_9", Demo_dam_P2)
    # pupLossServer("pupLoss_P2_9", Demo_dam_P2)
    # damCortServer("damCort_P2_9", Demo_dam_P2)
    # massOff_P2_9_Server("massOff_P2_9", Mass_off_P2, Demo_dam_P2)
    # maturationOff_P2_9_Server("maturationOff_P2_9", Maturation_off_P2)
    # acuteStressServer("acuteStress_P2_9", AcuteStress_off_P2, Demo_dam_P2)
    # cyclesServer("cycles_P2_9", Cycles_off_P2)
    # 
    # #P4-P11
    # massDamServer("massDam", Demo_dam_P4)
    # pupLossServer("pupLoss", Demo_dam_P4)
    # damCortServer("damCort", Demo_dam_P4)
    # massOffServer("massOff", Mass_off_P4, Demo_dam_P4)
    # maturationOffServer("maturationOff", Maturation_off_P4, LBN_data %>% filter(ParaType == 4))
    acuteStressServer("acuteStress", AcuteStress_off, LH_off, Cort_off, Demo_dam, niceNames)
    cyclesServer("cycles", Cycles_off)
    samplingPPTsServer("samplingPPTs", dateToday, AcuteStress_off, LBN_data, Cycles_off_all)

}

# Run the application
shinyApp(ui = ui, server = server)
