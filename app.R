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

LBN_varNames <<- LBN_varNames_func(LBN_all)

my_appSource <- function(scriptName){
    source(file.path(".", "Scripts", "AppScripts", scriptName))
}

#Load Modules
my_appSource("zoomAxisModule.R")
my_appSource("filteringDFModule.R")
my_appSource("summaryTableModule.R")
my_appSource("taskTrackingModule.R")
my_appSource("rawDataModule.R")
my_appSource("massDamModule.R")
my_appSource("pupLossModule.R")
my_appSource("damCortModule.R")
my_appSource("massOffModule.R")
my_appSource("maturationOffModule.R")
my_appSource("acuteStressModule.R")
my_appSource("cyclesModule.R")


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
                     checkboxInput("plot_DOB",
                                   "Plot Offspring DOBs?",
                                   value = FALSE),
                     uiOutput("Offspring_DOB_plot"),
                     uiOutput("Offspring_DOB_range"),
                     
                     tabsetPanel(
                     
                     #Dams --------
                     tabPanel("Dam",
                              tabsetPanel(
                                  
                                  #Mass
                                  tabPanel(
                                      "Dam Mass",
                                      massDamUI("massDam", Demo_dam)
                                  ),
                                  
                                  #Pup Loss
                                  tabPanel(
                                      "Pup Loss",
                                      pupLossUI("pupLoss", Demo_dam)
                                  ),
                                  
                                  #Corticosterone
                                  tabPanel(
                                      "Dam Corticosterone",
                                      damCortUI("damCort", Demo_dam)
                                  )
                              )
                              ),
                     
                     #Offspring Mass -----------
                     tabPanel("Offspring Mass",
                              massOffUI("massOff", Mass_off)
                              ),
                     ### Offspring Maturation ----
                     tabPanel("Offspring Maturation",
                              maturationOffUI("maturationOff", Maturation_off)
                              ),
                     
                     ### Offspring Corticosterone ----
                     tabPanel("Acute Stress Paradigm",
                              acuteStressUI("acuteStress", AcuteStress_off)
                              ), #End off cort panel
                     
                     ### Offspring Cycles ----
                     tabPanel("Offspring Cycles",
                              cyclesUI("cycles")
                              ) #End cycles tabPanel
                     ) #end analysis tabsetPanel
                 ### END ANALYSIS ----    
                 ) #end analysis tabPanel
                 
############                 
)

############# SERVER #########################################################
server <- function(input, output) {
    ### OFFSPRING DOB ----------------------
    output$Offspring_DOB_plot <- renderUI({
        if(input$plot_DOB){
            plotOutput("Offspring_DOB_hist",
                       height = "200px")
        }
    })
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
    massDamServer("massDam", Demo_dam)
    
    pupLossServer("pupLoss", Demo_dam)
    
    damCortServer("damCort", Demo_dam)
    
    massOffServer("massOff", Mass_off, Demo_dam)
    
    maturationOffServer("maturationOff", Maturation_off)
    
    acuteStressServer("acuteStress", AcuteStress_off, Demo_dam)
    
    cyclesServer("cycles", Cycles_off)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
