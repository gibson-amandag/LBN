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

#Data File Name
LBN_DataName = "LBN_0002_data_AGG.xlsx"

#Sheet Names
DemoName_dam = "Demo_dam"
DemoName_off = "Demo_off"
MassName_off = "Mass_off"
MaturationName_off = "Maturation_off"
EndParaName_off = "EndParadigm_off"
CyclesName_off = "Cycles_off"
ChronicStressName_off = "ChronicStress_off"
AcuteStressName_off = "AcuteStress_off"

#Functions Script (ends in .R)
FunctionsFileName = "LBN_0002_AGG_functions_Fall2020.R"


#Info about working directory and R Markdown: https://martinctc.github.io/blog/rstudio-projects-and-working-directories-a-beginner's-guide/#fn1
#More info: https://bookdown.org/yihui/rmarkdown-cookbook/working-directory.html

#Where R Notebook files are saved
ScriptsFolder = file.path("Scripts")

#Where Function files are saved
FunctionsFolder = file.path(ScriptsFolder, "Functions")

#Where data files are saved
DataFolder = file.path("Data")

#Where output should be saved
OutputFolder = file.path("Output")
#Where plot output should be saved
PlotFolder = file.path(OutputFolder, "Plots")
#Where data output should be saved
DataOutFolder = file.path(OutputFolder, "Data")


#Load the KNDy Functions
source(file.path(FunctionsFolder, FunctionsFileName))

# #Load datasets
NameDemo_dam = "Demo_dam"
NameDemo_off = "Demo_off"
NameMass_off = "Mass_off"
NameMaturation_off = "Maturation_off"
NameEndPara_off = "EndParadigm_off"
NameCycles_off = "Cycles_off"
NameChronicStress_off = "ChronicStress_off"
NameAcuteStress_off = "AcuteStress_off"

Demo_off = myXLSX_func(DataFolder, LBN_DataName, NameDemo_off) #replaced Offspring_demo
Mass_off = myXLSX_func(DataFolder, LBN_DataName, NameMass_off) #replaced Offspring_mass
Demo_dam = myXLSX_func(DataFolder, LBN_DataName, NameDemo_dam) #replaced Dam_info
Maturation_off = myXLSX_func(DataFolder, LBN_DataName, NameMaturation_off) #replaced Offspring_maturation
EndPara_off = myXLSX_func(DataFolder, LBN_DataName, NameEndPara_off) #replaced Offspring_post_para
AcuteStress_off = myXLSX_func(DataFolder, LBN_DataName, NameAcuteStress_off) #replaced Offspring_acute_stress
ChronicStress_off = myXLSX_func(DataFolder, LBN_DataName, NameChronicStress_off) #replaced Offspring_chronic_stress

#Cleaning up dfs
Demo_dam$Dam_ID = as.character(Demo_dam$Dam_ID)
Demo_off$Dam_ID = as.character(Demo_off$Dam_ID)

AcuteStress_off$Mouse_ID = as.character(AcuteStress_off$Mouse_ID)
ChronicStress_off$Mouse_ID = as.character(ChronicStress_off$Mouse_ID)
Demo_off$Mouse_ID = as.character(Demo_off$Mouse_ID)
Mass_off$Mouse_ID = as.character(Mass_off$Mouse_ID)
Maturation_off$Mouse_ID = as.character(Maturation_off$Mouse_ID)
EndPara_off$Mouse_ID = as.character(EndPara_off$Mouse_ID)

#Add demographics
Demo_dam_for_offspring <- Demo_dam %>%
    select(Dam_ID, Dam_cage, Treatment, Dam_Strain, Sire, DOB, Avg_litter_mass_P2, Litter_size_P2, Litter_size_P9)
print(head(Demo_dam_for_offspring))

Demo_off <- Demo_off %>%
    left_join(Demo_dam_for_offspring) %>%
    select(Mouse_ID:Dam_ID, DOB, Wean_Cage_Number:Sire, Avg_litter_mass_P2:Litter_size_P9)

Demo_off <<- Demo_off

LBN_data <- Demo_off %>%
    full_join(AcuteStress_off) %>%
    full_join(ChronicStress_off) %>%
    full_join(Mass_off) %>%
    full_join(Maturation_off) %>%
    full_join(EndPara_off)

LBN_data <<- LBN_data


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
           textOutput("toDoText")
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

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$toDoText <- renderText({
        Day0 <- input$date
        days <- input$days
        
        endDay <- Day0 + days
        
        #paste("You have selected a range starting on", Day0, "and ending on", endDay)
        
        mass_on_date_litter = Dam_dates %>%
            filter(is.na(Sac_or_stop)) %>%
            select(Dam_ID, mass_P10:mass_P19) %>%
            filter_all(any_vars(. %in% Day0))
        # print(mass_on_date_litter$Dam_ID)
        if(nrow(mass_on_date_litter) > 0){ #only print if there are values in df
            paste("Take the mass of the following litters:
                  ",
                  "+ ", mass_on_date_litter$Dam_ID)
            #paste0("+ ", mass_on_date_litter$Dam_ID, "\n\n")
        }

    #     for(day in days){
    #         Day = Day0 + day
    #         #Print the Day
    #         p(strong("On", Day))
    #         br()
    #         cat(paste0("**On ", Day, "** \n\n"))
    # 
    #         Count <- 0
    # 
    #         #Plug check
    #         for(val in Dam_seq()){
    #             if(Dam_dates$plug_check[val] == TRUE &
    #                Dam_day_greater(Day, "Breed_date", val)){
    #                 Dam_tasks("Check for plugs from the following mice", val)
    #             }
    #         }
    #     }
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
