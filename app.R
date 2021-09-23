#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("./01-scripts/01-set-up.R")
source(file.path(scriptsFolder, "02-get-datasets.R"))
library(shinyFiles)

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
                     LBNCyclesUI("cycles", Cycles_off_all)
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
    ),
    tabPanel(
        "Cycles",
        uploadCyclesUI(
            "uploadCycles"
        )
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
    LBNCyclesServer("cycles",
                    damInfo = Demo_dam,
                    offspringInfo = Demo_off,
                    Cycles_off = Cycles_off_all,
                    CohortCyclingFolder = CohortCyclingFolder,
                    compType = currentCompType)
    samplingPPTsServer("samplingPPTs", dateToday, AcuteStress_off, LBN_data, Cycles_off_all)
    uploadCyclesServer("uploadCycles", compType = currentCompType)

}

# Run the application
shinyApp(ui = ui, server = server)
