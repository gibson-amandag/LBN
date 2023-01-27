#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# source("./01-scripts/01-set-up.R")
# source(file.path(scriptsFolder, "02-get-datasets.R"))
# source(file.path(scriptsFolder, "04-filter-datasets.R"))
# library(shinyFiles)

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
        tags$body(
            # Note the wrapping of the string in HTML()
            tags$style( # This keeps the nav bar as a single line instead of wasting space with each panel as a line on small screen
                HTML("
                    .navbar-header, .navbar-nav, .navbar-nav>li {
                        float: left;
                    }
                    
                    .navbar-nav{
                        margin: 0px
                    }
                    
                    .navbar-nav>li>a {
                        padding-top: 15px;
                        padding-bottom: 15px;
                    }
                    
                    .container-fluid>.navbar-collapse, .container-fluid>.navbar-header, .container>.navbar-collapse, .container>.navbar-header{
                        margin-right: 0;
                        margin-left: 0;
                    }
                    
                     .navbar>.container .navbar-brand, .navbar>.container-fluid .navbar-brand {
                        margin-left: -15px;
                    }
                     ")
               )
        ),
        titlePanel("LBN Analysis"),
        tabsetPanel(
            ### Dams --------
            tabPanel(
                "Dam",
                tabsetPanel(
                    
                    #Mass
                    tabPanel(
                        "Dam Mass",
                        massDamUI("massDam", damFiltered)
                    ),
                    
                    #Behavior
                    tabPanel(
                        "Dam Behavior",
                        behaviorDamUI(
                          "damBehavior"
                          , damFiltered
                          , damFramesAndBehaviorByDam %>%
                            select(
                              Num_exits:clump8_percLitter
                            )
                          , damFramesAndBehavior_ByPND_ZT
                        )
                    ),
                    
                    #Scatter plots
                    tabPanel(
                        "Scatter plots",
                        damByTrtUI("damByTrt", damFramesAndBehaviorByDam)
                    ),
                    
                )
            ),
            
            ### Offspring Mass -----------
            tabPanel(
                "Offspring Mass",
                massOffUI("massOff", Mass_off %>% filter(ParaType == 4))
            ),
            ### Offspring Maturation ----
            tabPanel(
                "Offspring Maturation",
                maturationUI("maturation", 
                             Maturation_off %>% filter(ParaType == 4), 
                             LBN_data %>% filter(ParaType == 4))
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
            ### GABA PSCs ----
            tabPanel("GABA PSCs",
                     GABApscsUI("GABApscs", GABApscs)
            ), #End GABA PSCs
            tabPanel(
                "Sampling PPTs",
                samplingPPTsUI("samplingPPTs")
            ) #End sampling PPTs tabPanel
        ) #end analysis tabsetPanel
    ),
    tabPanel(
        "Explore Data",
        exploreDataUI("exploreData", LBN_data)
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
    ),
    tabPanel(
        "Cort EIA",
        uploadCortEIAUI(
            "cortEIA"
        )
    )

############
)

############# SERVER #########################################################
server <- function(input, output) {
    exploreDataServer("exploreData", LBN_data, currentCompType)
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
  
    massDamServer("massDam",
                  damFiltered,
                  niceNames = niceNames,
                  compType = currentCompType
                  )
    damByTrtServer("damByTrt",
                   damFramesAndBehaviorByDam,
                   niceNames = niceNames,
                   compType = currentCompType)
    behaviorDamServer("damBehavior",
                   damFramesDF = damFramesFiltered,
                   damBehaviorDF = damBehaviorFiltered_ZTs,
                   niceNames = niceNames,
                   compType = currentCompType,
                   demoDamToAdd = Demo_dam_for_offspring %>% 
                     select(
                       -cyclingFolderPath
                     )
                   )
    massOffServer("massOff", 
                  Mass_off %>% filter(ParaType == 4), 
                  Demo_dam %>% filter(ParaType == 4),
                  Demo_dam_for_offspring %>% filter(ParaType == 4),
                  compType = currentCompType)
    maturationServer("maturation", 
                  Maturation_off %>% filter(ParaType == 4), 
                  LBN_data %>% filter(ParaType == 4),
                  Demo_dam %>% filter(ParaType == 4),
                  Demo_dam_for_offspring %>% filter(ParaType == 4),
                  compType = currentCompType)
    # maturationOffServer("maturationOff", Maturation_off_P4, LBN_data %>% filter(ParaType == 4))
    acuteStressServer("acuteStress", 
                      AcuteStress_off, 
                      LH_off, 
                      Cort_off, 
                      Demo_dam, 
                      niceNames,
                      currentCompType)
    GABApscsServer(
        "GABApscs",
        GABApscs,
        AcuteStress_off,
        LH_off,
        Cort_off,
        Demo_dam,
        niceNames,
        currentCompType
    )
    
    LBNCyclesServer("cycles",
                    damInfo = Demo_dam,
                    offspringInfo = Demo_off,
                    Cycles_off = Cycles_off_all,
                    CohortCyclingFolder = CohortCyclingFolder,
                    compType = currentCompType)
    samplingPPTsServer("samplingPPTs", dateToday, AcuteStress_off, LBN_data, Cycles_off_all, LBN_uterinePicsFolder)
    uploadCyclesServer("uploadCycles", compType = currentCompType)
    uploadCortEIAServer("cortEIA", compType = currentCompType)

}

# Run the application
shinyApp(ui = ui, server = server)
