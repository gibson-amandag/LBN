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

blueText <-  function(text){
    paste0("<span style='color:blue'>", text, "</span>")
}

LBN_varNames <<- LBN_varNames_func(LBN_all)

# Define UI for application that draws a histogram
ui <- navbarPage("LBN",
                 
                 ### TASK TRACKING PANEL ----------------------
                 tabPanel(
                     "Tasks",
                     fluidPage(
                         # Page title
                         titlePanel("Limited Bedding and Nesting Task Tracking"),
                         
                         #Fluid Row
                         fluidRow(column(
                             4,
                             dateInput("date",
                                       "Enter starting date:")
                         ),
                         column(
                             4,
                             sliderInput(
                                 "days",
                                 "Select Number of Days:",
                                 min = 0,
                                 max = 100,
                                 value = 5
                             )
                         )),
                         
                         uiOutput("selectedDates"),
                         uiOutput("toDoText")
                         
                     )
                 ),
                 
                 ### DATA FRAMES -----------------------
                 tabPanel(
                     "Data",
                     fluidPage(
                         titlePanel("LBN Data Frames"),
                         
                         #Dam Data
                         h2("Dam Data"),
                         varSelectInput("dam_vars_include",
                                        label = "Select Column Variables",
                                        data = Demo_dam,
                                        selected = c("Dam_ID", 
                                                     "Treatment",
                                                     "Breed_date",
                                                     "Plug_date",
                                                     "DOB"),
                                        multiple = TRUE),
                         dataTableOutput("Demo_dam"),
                         
                         #Offspring Data
                         h2("Offspring Data"),
                         tabsetPanel(
                             tabPanel("Offspring Demographics",
                                      h3("Offspring Demographics"),
                                      dataTableOutput("Demo_off")),
                             tabPanel("Offspring Mass",
                                      h3("Offspring Mass"),
                                      dataTableOutput("Mass_off")),
                             tabPanel("Offspring Maturation",
                                      h3("Offspring Maturation"),
                                      dataTableOutput("Maturation_off")),
                             tabPanel("After Paradigm",
                                      h3("After Paradigm"),
                                      dataTableOutput("EndPara_off")),
                             tabPanel("Cycles",
                                      h3("Offspring Cycles"),
                                      dataTableOutput("Cycles_off")),
                             tabPanel("Acute Stress",
                                      h3("Acute Stress"),
                                      dataTableOutput("AcuteStress_off")),
                             tabPanel("Chronic Stress",
                                      h3("Chronic Stress"),
                                      dataTableOutput("ChronicStress_off"))
                         ),
                         
                         
                         h3("Combined Offspring Data"),
                         varSelectInput("offspring_vars_include",
                                        label = "Select Column Variables",
                                        data = Demo_off,
                                        selected = c("Mouse_ID",
                                                     "Dam_ID",
                                                     "Treatment",
                                                     "DOB"),
                                        multiple = TRUE),
                         dataTableOutput("LBN_data")
                     )
                 ),
                 
                 ### ANALYSIS-------------------------
                 tabPanel(
                     "Analysis",
                     titlePanel("LBN Analysis"),
                     tabsetPanel(
                     tabPanel("Dam Mass",
                              h3("Dam Mass"),
                              fluidRow(
                              column(4,
                                     varSelectInput("Mass_dams_vars_to_sum",
                                             "Select variables to summarize",
                                             data = Demo_dam %>%
                                                 select(Dam_Mass_P2:Dam_Mass_P21),
                                             selected = c("Dam_Mass_P2",
                                                          "Dam_Mass_P4",
                                                          "Dam_Mass_P9",
                                                          "Dam_Mass_P11"),
                                             multiple = TRUE)),
                              column(4,
                                     varSelectInput("Mass_dams_grouping_vars",
                                             "Select variables to group by",
                                             data = Demo_dam %>%
                                                 select(Treatment:Dam_Strain,
                                                        ParaType,
                                                        Sac_or_stop),
                                             selected = c("Treatment",
                                                          "Dam_Strain"),
                                             multiple = TRUE))
                              ),
                              dataTableOutput("Mass_dam_summary")),
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
                                 column(3,
                                        radioButtons("Mass_off_ParaTypes",
                                              "Which paradigm type?",
                                              c("Both", "P2-P9", "P4-P11"),
                                              selected = "Both"),
                                        checkboxInput("Mass_off_by_dam",
                                                      "Plot by dam?",
                                                      value = FALSE),
                                        checkboxInput("Mass_off_by_strain",
                                                      "Plot by strain?",
                                                      value = TRUE)),
                                 column(3,
                                        checkboxInput("Mass_off_individual_lines",
                                                      "Plot individual lines?",
                                                      value = TRUE),
                                        checkboxInput("Mass_off_mean_lines",
                                                      "Plot mean lines?",
                                                      value = TRUE),
                                        textInput("Mass_off_title",
                                                  "Graph Title:")
                                        ),
                                 column(3,
                                        checkboxInput("Mass_off_zoom_x",
                                                      "Zoom x axis?"),
                                        checkboxInput("Mass_off_zoom_y",
                                                      "Zoom y axis?"),
                                        ##Add conditional
                                        numericInput("Mass_off_xmin",
                                                     "Lower Limit x-axis:",
                                                     0),
                                        numericInput("Mass_off_xmax",
                                                     "Upper Limit x-axis:",
                                                     21),
                                        numericInput("Mass_off_ymin",
                                                     "Lower Limit y-axis:",
                                                     0),
                                        numericInput("Mass_off_ymax",
                                                     "Upper Limit y-axis:",
                                                     15))
                                 
                              ),
                              plotOutput("Mass_off_plot"))
                     )
                 )
############                 
)

############# SERVER #########################################################
server <- function(input, output) {
    
    #### TASK TRACKING HTML TEXT------------------
    
    output$selectedDates <-renderUI({
        Day0 <- input$date
        days <- input$days
        
        endDay <- Day0 + days
        
        str <- paste("<h2>", "You have selected a range from", blueText(Day0), "to", blueText(endDay), "</h2>")
        
        HTML(str)
    })

    output$toDoText <- renderUI({
        Day0 <- input$date
        days <- c(0:input$days) #how many days to print
        printCat <- c("")
        for(day in days){
            Day <- Day0 + day
            #Print the Day
            printCat <- list_add(printCat, paste0("<strong>On ", Day, "</strong> <br>"))
            
            Count <<- 0
            
            #Plug check
            for(val in Dam_seq()){
                if(Dam_dates$plug_check[val] == TRUE & 
                   Dam_day_greater(Day, "Breed_date", val)){
                    printCat <- Dam_tasks_app(paste0("Check for ", blueText("plugs"), "from the following mice"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #Check for pregnancy (breed date)
            for(val in Dam_seq()){
                if(Dam_is.na("mass_G12", val) & 
                   sac_stop(val) & 
                   Dam_not.na("mass_check", val) & 
                   Dam_day_equals(Day, "mass_check", val)
                ){
                    printCat <- Dam_tasks_app(paste0("Check for ", blueText("pregnancy and/or separate"), " the following mice (by breed date)"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #Check for pregnancy (plug date)
            for(val in Dam_seq()){
                if(
                    sac_stop(val) &
                    Dam_not.na("mass_G12", val) &
                    Dam_day_equals(Day, "mass_G12", val)
                ){
                    printCat <- Dam_tasks_app(paste0("Check for ", blueText("pregnancy and/or separate"), " the following mice (by plug date)"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            
            #Watch for births
            for(val in Dam_seq()){
                if(Dam_is.na("DOB", val) & #if there is not a DOB yet
                   sac_stop(val) &
                   Dam_not.na("start_birth_check", val) &
                   Dam_day_greater(Day, "start_birth_check", val)
                ){
                    printCat <- Dam_tasks_app(paste0("Watch for ", blueText("births"), " from the following dams"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #Set up LBN
            for(val in Dam_seq()){
                if(Dam_not.na("start_paradigm", val) &
                   Dam_day_equals(Day, "start_paradigm", val)
                ){
                    printCat <- Dam_tasks_app(paste0(blueText("Set up"), " the LBN paradigm (and ", 
                                                     blueText("take masses"), " for the following litter(s) (known births)"),
                                              val, printCat)
                }
            }
            printCat <- printLine_func_app(Count, printCat)
            
            #separated out from is..else because they might not be the same day, and only want this to print if there's no known date
            for(val in Dam_seq()){
                if(
                    sac_stop(val) &
                    Dam_is.na("start_paradigm", val) &
                    Dam_not.na("est_start_paradigm", val) &
                    Dam_day_equals(Day, "est_start_paradigm", val)
                ){
                    printCat <- Dam_tasks_app(paste0(blueText("Set up"), " the LBN paradigm (and ", 
                                                     blueText("take masses"), " for the following litter(s) (predicted births)"),
                                              val, printCat)
                } 
            }
                
            printCat <- printLine_func_app(Count, printCat)
            
            #End LBN
            for(val in Dam_seq()){
                if(Dam_not.na("end_paradigm", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "end_paradigm", val)
                ){
                    printCat <- Dam_tasks_app(paste0(blueText("End"), " the LBN paradigm, ", blueText("tag,"), " and ", 
                                                     blueText("take masses"), " for the following mice"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #Take dam mass
            for(val in Dam_seq()){
                if(Dam_not.na("mass_startPara", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "mass_startPara", val)
                ){
                    printCat <- Dam_tasks_app(paste0("Take the ", blueText("mass"), " of the following dams"), val, printCat)
                }
                
                if(Dam_not.na("mass_endPara", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "mass_endPara", val)
                ){
                    printCat <- Dam_tasks_app(paste0("Take the ", blueText("mass"), " of the following dams"), val, printCat)
                }
                
                if(Dam_not.na("mass_P21", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "mass_P21", val)
                ){
                    printCat <- Dam_tasks_app(paste0("Take the ", blueText("mass"), " of the following dams"), val, printCat)
                }
                
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #Wean cages
            for(val in Dam_seq()){
                if(Dam_not.na("mass_P21", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "mass_P21", val)
                ){
                    printCat <- Dam_tasks_app(paste0(blueText("Wean"), " the following cages"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #Offspring Mass
            #Filter df for only rows that have the Day in any column
            mass_on_date_litter <- Dam_dates %>%
                filter(is.na(Sac_or_stop)) %>%
                select(Dam_ID, mass_P10:mass_P19) %>%
                filter_all(any_vars(. %in% Day))
            if(nrow(mass_on_date_litter) > 0){ #only print if there are values in df
                printCat <- list_add(printCat, paste0("<em>Take the ", blueText("mass"), " of the following litters:<em> <ul>"))
                for(val in seq_along(mass_on_date_litter$Dam_ID)){
                    printCat <- list_add(printCat, paste0("<li>", mass_on_date_litter$Dam_ID[val], "</li>"))
                }
            }
            if(nrow(mass_on_date_litter) > 0){printCat <- list_add(printCat, "</ul>")}
            
            #Filter df for only rows that have the Day in any column
            #https://dplyr.tidyverse.org/articles/colwise.html - new is across, but no replacement for any_vars()
            mass_on_date <- Off_dates %>%
                select(Mouse_ID, mass_P22:mass_P72) %>%
                filter_all(any_vars(. %in% Day))
            if(nrow(mass_on_date) > 0){ #only print if there are values in df
                printCat <- list_add(printCat, paste0("<em>Take the ", blueText("mass"), " of the following offspring:<em> <ul>"))
                for(val in seq_along(mass_on_date$Mouse_ID)){
                    printCat <- list_add(printCat, paste0("<li>", mass_on_date$Mouse_ID[val], "</li>"))
                }
            }
            if(nrow(mass_on_date) > 0){printCat <- list_add(printCat, "</ul>")}
            
            
            #AGD
            for(val in Off_seq()){
                if(Off_not.na("start_AGD", val) &
                   Off_day_greater(Day, "start_AGD", val) &
                   Off_day_less(Day, "end_AGD", val)
                ){
                    printCat <- Off_tasks_app(paste0("Take the ", blueText("ano-genital distance"), " of the following mice"), val, printCat)
                }
                if(Off_not.na("adult_AGD_start", val) &
                   Off_day_greater(Day, "adult_AGD_start", val) &
                   Off_day_less(Day, "adult_AGD_end", val)
                ){
                    printCat <- Off_tasks_app(paste0("Take the ", blueText("ano-genital distance"), " of the following mice"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #VO
            for(val in Off_seq()){
                if(
                    Off_not.na("check_VO", val) &
                    Off_day_greater(Day, "check_VO", val)
                ){
                    printCat <- Off_tasks_app(paste0("Check for ", blueText("vaginal opening"), " of the following mice"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #estrus
            for(val in Off_seq()){  
                if(
                    Off_not.na("check_Estrus", val) &
                    Off_day_greater(Day, "check_Estrus", val)
                ){
                    printCat <- Off_tasks_app(paste0("Check for ", blueText("first estrus"), " for the following mice"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #preputial separation
            for(val in Off_seq()){
                if(
                    Off_not.na("check_PPS", val) &
                    Off_day_greater(Day, "check_PPS", val)
                ){
                    printCat <- Off_tasks_app(paste0("Check for ", blueText("preputial separation"), " for the following mice"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #cycle
            for(val in Off_seq()){
                if(
                    Off_not.na("start_cycle", val) &
                    Off_day_greater(Day, "start_cycle", val) &
                    Off_day_less(Day, "end_cycle", val)
                ){
                    printCat <- Off_tasks_app(paste0(blueText("Cycle"), " the following mice"), val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            printCat <- list_add(printCat, "<br>")
        }
        HTML(printCat)

     })
    
    #### RENDER DATA FRAMES----------------------
    
    output$Demo_dam <- renderDataTable(
        Demo_dam %>%
            select(!!! input$dam_vars_include),
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
            )
    
    output$Demo_off <- renderDataTable(
        Demo_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
    )
    
    output$Mass_off <- renderDataTable(
        Mass_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
    )
    
    output$Maturation_off <- renderDataTable(
        Maturation_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
    )
    
    output$EndPara_off <- renderDataTable(
        EndPara_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
    )
    
    output$Cycles_off <- renderDataTable(
        Cycles_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
    )
    
    output$AcuteStress_off <- renderDataTable(
        AcuteStress_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
    )
    
    output$ChronicStress_off <- renderDataTable(
        ChronicStress_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
    )
    
    
    output$LBN_data <- renderDataTable(
        LBN_data %>%
            select(!!! input$offspring_vars_include),
        options = list(
            scrollX = TRUE,
            scroller = TRUE,
            pageLength = 10)
    )
        
    
    
    
    #### RENDER ANALYSIS -----------------------------
    ### Summary Data Frames ----------------
    output$Mass_dam_summary <- renderDataTable(
        map_dfr(input$Mass_dams_vars_to_sum, LBN_summary_byGroup, Demo_dam, input$Mass_dams_grouping_vars)
    )
    
    output$Mass_off_summary <- renderDataTable(
        map_dfr(input$Mass_vars_to_sum, LBN_summary_byGroup, Mass_off, input$Mass_grouping_vars)
    )
    
    ### Plots ------------------
    output$Mass_off_plot <- renderPlot({
        Mass_off_long <- make_long_form(Mass_off)
        
        Mass_off_long <- make_PND_col(Mass_off_long)
        
        Mass_off_long <- Mass_off_long %>%
            filter(!is.na(Mass))
        
        #Add - filter for paradigm type
        #add - filter for breed date (add to UI, too)
        #add prep for grouping by dam
        
        mass_plot_lines(Mass_off_long,
                        line_group = expr(Mouse_ID),
                            #ifelse(input$Mass_off_by_dam, expr(Dam_ID), expr(Mouse_ID))
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
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
