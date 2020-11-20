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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("LBN TRACKING"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("date",
                      "Enter starting date:"
                      ),
            sliderInput("days",
                        "Select Date Range:",
                        min = 0,
                        max = 100,
                        value = 15)
        ),

        # Show text with description of tasks
        mainPanel(
           uiOutput("toDoText")
        )
    ),
    
    h1("Take Mass of Dams"),
    fluidRow(
        #varSelectInput()
    ),
    dataTableOutput("damMassDates"),
    
    h1("Offspring Dates"),
    dataTableOutput("offDates")
    
)

# Define server logic
server <- function(input, output) {
    
    #list("test", "test2", "test3")

    output$toDoText <- renderText({
        Day0 <- input$date
        days <- input$days
        
        endDay <- Day0 + days
        
        paste("You have selected a range starting on", Day0, "and ending on", endDay)
        

     })
    
    output$damMassDates <- renderDataTable(
        df <- Dam_dates %>%
            filter(is.na(Sac_or_stop)) %>%
            select(Dam_ID, mass_P2:mass_P21) %>%
            filter_all(any_vars(. %in% input$date)) %>%
            select(Dam_ID)
    )
    
    output$offDates <- renderDataTable(
        Off_dates
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
