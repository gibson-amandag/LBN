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
source("./Scripts/LBN_0002_AGG_GetDataReady.R")

#Source Task Functions
source("./Scripts/Functions/LBN_0002_AGG_taskFunctions.R")


### DAM DATES --------
Dam_dates <- Demo_dam %>%
    select(Dam_ID,
           Breed_date,
           Plug_date,
           DOB,
           Sac_or_stop)

Dam_dates <- Dam_dates %>%
    mutate(
        #plug check - true or false, based off of whether this is a plug date, DOB for litter, or sacrifice or stop marked
        #when printing, will want to add a "is Day > breed_date"
        plug_check = ifelse(
            is.na(Plug_date) & is.na(DOB) & is.na(Sac_or_stop),
            TRUE,
            FALSE),

        #Check for pregnancy of dam
        mass_check = Breed_date + 12 - 1, #will use this only if mass_G12 is NA, meaning Plug_date hasn't occured
        mass_G12 = Plug_date + 12 - 1,

        #estimated birth dates
        start_birth_check = Plug_date + 19 - 1,
        est_birth_date = Plug_date + 21 - 1,

        #Mass of dam and pups on these days
        mass_P2 = ifelse( # to-do will want to change to P4 for paradigm change. Determine if this is with DOB = P0 or P1
            is.na(DOB), #if there isn't a DOB of the litter
            Plug_date + 21 - 1 + 2, #estimate based on plug date
            DOB + 2), #if there is a DOB, add 2
        mass_P9 = DOB + 9, # to-do change to P11
        mass_P21 = DOB + 21,

        #Mass of only pups on these days
        mass_P9 = DOB + 9,
        mass_P10 = DOB + 10,
        mass_P11 = DOB + 11,
        mass_P12 =  DOB + 12,
        mass_P13 = DOB + 13,
        mass_P15 = DOB + 15,
        mass_P17 = DOB + 17,
        mass_P19 = DOB + 19,
        mass_P21 = DOB + 21,

        #paradigm dates
        # to-do adjust these dates

        start_paradigm = DOB + 2,
        end_paradigm = DOB + 9,
        est_start_paradigm = Plug_date + 21 - 1 + 2,
        end_recording = DOB + 4
    )


Dam_dates$mass_P2 <- as_date(Dam_dates$mass_P2)

Dam_dates <<- Dam_dates

# ### OFFSPRING DATES -----------
Off_dates <- LBN_data %>%
    select(Mouse_ID,
           Sex,
           DOB,
           VO_day,
           Estrus_day,
           PreputialSep_day) %>%
    mutate(

        #offspring mass dates
        mass_P22 = DOB + 22,
        mass_P23 = DOB + 23,
        mass_P24 = DOB + 24,
        mass_P28 = DOB + 28,
        mass_P35 = DOB + 35,
        mass_P42 = DOB + 42,
        mass_P49 = DOB + 49,
        mass_P56 = DOB + 56,
        mass_P63 = DOB + 63,
        mass_P70 = DOB + 70,
        mass_P71 = DOB + 71,
        mass_P72 = DOB + 72,

        #AGD Dates
        start_AGD = DOB + 22,
        end_AGD = DOB + 24,
        adult_AGD_start = DOB + 70,
        adult_AGD_end = DOB + 72,

        #Females
        check_VO = ifelse(Sex == "F" & is.na(VO_day), DOB + 21, NA),
        check_Estrus = ifelse(Sex == "F" & !is.na(VO_day) & is.na(Estrus_day), DOB + 21, NA),
        start_cycle = ifelse(Sex == "F", DOB + 70, NA),
        end_cycle = ifelse(Sex == "F", DOB + 90, NA),

        #Males
        check_PPS = ifelse(Sex == "M" & is.na(PreputialSep_day), DOB + 21, NA)

    )

#Because of the check of sex, it forces these into numerical rep of dates
Off_dates$check_VO = as_date(Off_dates$check_VO)
Off_dates$check_Estrus = as_date(Off_dates$check_Estrus)
Off_dates$start_cycle = as_date(Off_dates$start_cycle)
Off_dates$end_cycle = as_date(Off_dates$end_cycle)
Off_dates$check_PPS = as_date(Off_dates$check_PPS)

Off_dates <<- Off_dates


blueText <-  function(
    text
){
    paste0("<span style='color:blue'>", text, "</span>")
}

# Define UI for application that draws a histogram
ui <- navbarPage("LBN",
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
                 
                 ##Data frames -----------------------
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
                             tabPanel("End after Paradigm",
                                      h3("End after Paradigm"),
                                      dataTableOutput("EndPara_off")),
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
                 
                 ##Analysis----------------------
                 tabPanel(
                     "Analysis",
                     titlePanel("LBN Analysis")
                     )
                 
)

# Define server logic
server <- function(input, output) {
    
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
                else{
                    if(
                        sac_stop(val) &
                        Dam_not.na("est_start_paradigm", val) &
                        Dam_day_equals(Day, "est_start_paradigm", val)
                    ){
                        printCat <- Dam_tasks_app(paste0(blueText("Set up"), " the LBN paradigm (and ", 
                                                         blueText("take masses"), " for the following litter(s) (predicted births)"),
                                                  val, printCat)
                    }
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
                if(Dam_not.na("mass_P2", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "mass_P2", val)
                ){
                    printCat <- Dam_tasks_app("Take the mass of the following dams", val, printCat)
                }
                
                if(Dam_not.na("mass_P9", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "mass_P9", val)
                ){
                    printCat <- Dam_tasks_app("Take the mass of the following dams", val, printCat)
                }
                
                if(Dam_not.na("mass_P21", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "mass_P21", val)
                ){
                    printCat <- Dam_tasks_app("Take the mass of the following dams", val, printCat)
                }
                
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #Wean cages
            for(val in Dam_seq()){
                if(Dam_not.na("mass_P21", val) &
                   sac_stop(val) &
                   Dam_day_equals(Day, "mass_P21", val)
                ){
                    printCat <- Dam_tasks_app("Wean the following cages", val, printCat)
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
                printCat <- list_add(printCat, "<em>Take the mass of the following litters:<em> <br><br><ul>")
                for(val in seq_along(mass_on_date_litter$Mouse_ID)){
                    printCat <- list_add(printCat, paste0("<li>", mass_on_date_litter$Mouse_ID[val], "</li>"))
                }
            }
            if(nrow(mass_on_date_litter) > 0){printCat <- list_add(printCat, "</ul>")}
            
            #Filter df for only rows that have the Day in any column
            #https://dplyr.tidyverse.org/articles/colwise.html - new is across, but no replacement for any_vars()
            mass_on_date <- Off_dates %>%
                select(Mouse_ID, mass_P22:mass_P72) %>%
                filter_all(any_vars(. %in% Day))
            if(nrow(mass_on_date) > 0){ #only print if there are values in df
                printCat <- list_add(printCat, "<em>Take the mass of the following offspring:</em> <br><br><ul>")
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
                    printCat <- Off_tasks_app("Take the ano-genital distance of the following mice", val, printCat)
                }
                if(Off_not.na("adult_AGD_start", val) &
                   Off_day_greater(Day, "adult_AGD_start", val) &
                   Off_day_less(Day, "adult_AGD_end", val)
                ){
                    printCat <- Off_tasks_app("Take the ano-genital distance of the following mice", val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #VO
            for(val in Off_seq()){
                if(
                    Off_not.na("check_VO", val) &
                    Off_day_greater(Day, "check_VO", val)
                ){
                    printCat <- Off_tasks_app("Check for vaginal opening of the following mice", val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #estrus
            for(val in Off_seq()){  
                if(
                    Off_not.na("check_Estrus", val) &
                    Off_day_greater(Day, "check_Estrus", val)
                ){
                    printCat <- Off_tasks_app("Check for estrus for the following mice", val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            
            #preputial separation
            for(val in Off_seq()){
                if(
                    Off_not.na("check_PPS", val) &
                    Off_day_greater(Day, "check_PPS", val)
                ){
                    printCat <- Off_tasks_app("Check for preputial separation for the following mice", val, printCat)
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
                    printCat <- Off_tasks_app("Cycle the following mice", val, printCat)
                }
            }
            
            printCat <- printLine_func_app(Count, printCat)
            printCat <- list_add(printCat, "<br>")
        }
        HTML(printCat)

     })
    
    output$Demo_dam <- renderDataTable(
        Demo_dam %>%
            select(!!! input$dam_vars_include),
        options = list(
            scrollX = TRUE,
            scroller = TRUE)
            )
    
    output$Demo_off <- renderDataTable(
        Demo_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE)
    )
    
    output$Mass_off <- renderDataTable(
        Mass_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE)
    )
    
    output$Maturation_off <- renderDataTable(
        Maturation_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE)
    )
    
    output$EndPara_off <- renderDataTable(
        EndPara_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE)
    )
    
    output$AcuteStress_off <- renderDataTable(
        AcuteStress_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE)
    )
    
    output$ChronicStress_off <- renderDataTable(
        ChronicStress_off,
        options = list(
            scrollX = TRUE,
            scroller = TRUE)
    )
    
    
    output$LBN_data <- renderDataTable(
        LBN_data %>%
            select(!!! input$offspring_vars_include),
        options = list(
            scrollX = TRUE,
            scroller = TRUE)
    )
        
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
