### Tasking Tracking App Module

# https://shiny.rstudio.com/articles/modules.html

taskTrackingUI <- function(id){
  ns <- NS(id)
  tagList(
    
    #Fluid Row
    fluidRow(column(
      4,
      dateInput(
        ns("date"),
        "Enter starting date:"
      )
    ),
    column(
      4,
      sliderInput(
        ns("days"),
        "Select Number of Days:",
        min = 0,
        max = 100,
        value = 5
      )
    )),
    
    uiOutput(ns("selectedDates")),
    uiOutput(ns("toDoText"))
  )
}

taskTrackingServer <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
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
          
          #Set up breeding
          for(val in Dam_seq()){
            if(Dam_day_equals(Day, "Breed_date", val)){
              printCat <- Dam_tasks_app(
                paste0(
                  "Set up ", blueText("breeding cages"), " for the following dams"
                ), 
                val, printCat
              )
            }
          }
          
          printCat <- printLine_func_app(Count, printCat)
          
          #Plug check
          for(val in Dam_seq()){
            if(Dam_dates$plug_check[val] == TRUE & 
               Dam_day_greater(Day, "Breed_date", val)){
              printCat <- Dam_tasks_app(
                paste0(
                  "Check for ", blueText("plugs"), " from the following mice"
                ), 
                val, printCat
              )
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
              printCat <- Dam_tasks_app(
                paste0(
                  "Check for ", blueText("pregnancy and/or separate"), " the following mice (by breed date)"
                ), 
                val, printCat
              )
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
              printCat <- Dam_tasks_app(
                paste0(
                  "Check for ", blueText("pregnancy and/or separate"), " the following mice (by plug date)"
                ), 
                val, printCat
              )
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
              printCat <- Dam_tasks_app(
                paste0(
                  "Watch for ", blueText("births"), " from the following dams"
                ), 
                val, printCat
              )
            }
          }
          
          printCat <- printLine_func_app(Count, printCat)
          
          #Set up LBN
          for(val in Dam_seq()){
            if(Dam_not.na("start_paradigm", val) &
               sac_stop(val) &
               Dam_day_equals(Day, "start_paradigm", val)
            ){
              printCat <- Dam_tasks_app(
                paste0(
                  blueText("Set up"), 
                  " the LBN paradigm (and ", 
                  blueText("take masses)"), 
                  " for the following litter(s) (known births)"
                ),
                val, printCat
              )
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
              printCat <- Dam_tasks_app(
                paste0(
                  blueText("Set up"), 
                  " the LBN paradigm (and ", 
                  blueText("take masses)"), 
                  " for the following litter(s) (predicted births)"
                ),
                val, printCat
              )
            } 
          }
          
          printCat <- printLine_func_app(Count, printCat)
          
          #End LBN
          for(val in Dam_seq()){
            if(Dam_not.na("end_paradigm", val) &
               sac_stop(val) &
               Dam_day_equals(Day, "end_paradigm", val)
            ){
              printCat <- Dam_tasks_app(
                paste0(
                  blueText("End"), 
                  " the LBN paradigm, ", 
                  blueText("tag,"), " and ", 
                  blueText("take masses"), 
                  " for the following mice"
                ), 
                val, printCat
              )
            }
          }
          
          printCat <- printLine_func_app(Count, printCat)
          
          #Take dam mass
          for(val in Dam_seq()){
            if(Dam_not.na("mass_startPara", val) &
               sac_stop(val) &
               Dam_day_equals(Day, "mass_startPara", val)
            ){
              printCat <- Dam_tasks_app(
                paste0(
                  "Take the ", blueText("mass"), " of the following dams"
                ), 
                val, printCat
              )
            }
            
            if(Dam_not.na("mass_endPara", val) &
               sac_stop(val) &
               Dam_day_equals(Day, "mass_endPara", val)
            ){
              printCat <- Dam_tasks_app(
                paste0(
                  "Take the ", blueText("mass"), " of the following dams"
                ), 
                val,printCat
              )
            }
            
            if(Dam_not.na("mass_P21", val) &
               sac_stop(val) &
               Dam_day_equals(Day, "mass_P21", val)
            ){
              printCat <- Dam_tasks_app(
                paste0(
                  "Take the ", blueText("mass"), " of the following dams"
                ), 
                val, printCat
              )
            }
            
          }
          
          printCat <- printLine_func_app(Count, printCat)
          
          #Wean cages
          for(val in Dam_seq()){
            if(Dam_not.na("mass_P21", val) &
               sac_stop(val) &
               Dam_day_equals(Day, "mass_P21", val)
            ){
              printCat <- Dam_tasks_app(
                paste0(
                  blueText("Wean"), " the following cages"
                ), 
                val, printCat
              )
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
            printCat <- list_add(
              printCat, 
              paste0(
                "<em>Take the ", blueText("mass"), " of the following litters:</em> <ul style=\"list-style-type:circle;\">"
              )
            )
            for(val in seq_along(mass_on_date_litter$Dam_ID)){
              printCat <- list_add(
                printCat, 
                paste0(
                  "<li>", mass_on_date_litter$Dam_ID[val], "</li>"
                )
              )
            }
          }
          if(nrow(mass_on_date_litter) > 0){printCat <- list_add(printCat, "</ul>")}
          
          #Filter df for only rows that have the Day in any column
          #https://dplyr.tidyverse.org/articles/colwise.html - new is across, but no replacement for any_vars()
          mass_on_date <- Off_dates %>%
            select(Mouse_ID, mass_P22:mass_P72) %>%
            filter_all(any_vars(. %in% Day))
          if(nrow(mass_on_date) > 0){ #only print if there are values in df
            printCat <- list_add(
              printCat, 
              paste0(
                "<em>Take the ", blueText("mass"), " of the following offspring:</em> <ul style=\"list-style-type:circle;\">"
              )
            )
            for(val in seq_along(mass_on_date$Mouse_ID)){
              printCat <- list_add(
                printCat, 
                paste0(
                  "<li>", mass_on_date$Mouse_ID[val], "</li>"
                )
              )
            }
          }
          if(nrow(mass_on_date) > 0){printCat <- list_add(printCat, "</ul>")}
          
          
          #AGD
          for(val in Off_seq()){
            if(Off_not.na("start_AGD", val) &
               Off_day_greater(Day, "start_AGD", val) &
               Off_day_less(Day, "end_AGD", val)
            ){
              printCat <- Off_tasks_app(
                paste0(
                  "Take the ", blueText("ano-genital distance"), " of the following mice"
                ), 
                val, printCat
              )
            }
            if(Off_not.na("adult_AGD_start", val) &
               Off_day_greater(Day, "adult_AGD_start", val) &
               Off_day_less(Day, "adult_AGD_end", val)
            ){
              printCat <- Off_tasks_app(
                paste0(
                  "Take the ", blueText("ano-genital distance"), " of the following mice"
                ), 
                val, printCat
              )
            }
          }
          
          printCat <- printLine_func_app(Count, printCat)
          
          #VO
          for(val in Off_seq()){
            if(
              Off_not.na("check_VO", val) &
              Off_day_greater(Day, "check_VO", val)
            ){
              printCat <- Off_tasks_app(
                paste0(
                  "Check for ", blueText("vaginal opening"), " of the following mice"
                ), 
                val, printCat
              )
            }
          }
          
          printCat <- printLine_func_app(Count, printCat)
          
          #estrus
          for(val in Off_seq()){  
            if(
              Off_not.na("check_Estrus", val) &
              Off_day_greater(Day, "check_Estrus", val)
            ){
              printCat <- Off_tasks_app(
                paste0(
                  "Check for ", blueText("first estrus"), " for the following mice"
                ), 
                val, printCat
              )
            }
          }
          
          printCat <- printLine_func_app(Count, printCat)
          
          #preputial separation
          for(val in Off_seq()){
            if(
              Off_not.na("check_PPS", val) &
              Off_day_greater(Day, "check_PPS", val)
            ){
              printCat <- Off_tasks_app(
                paste0(
                  "Check for ", blueText("preputial separation"), " for the following mice"
                ), 
                val, printCat
              )
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
              printCat <- Off_tasks_app(
                paste0(
                  blueText("Cycle"), " the following mice"
                ), 
                val, printCat
              )
            }
          }
          
          printCat <- printLine_func_app(Count, printCat)
          printCat <- list_add(printCat, "<br>")
        }
        HTML(printCat)
        
      })
    }
  )
}

taskTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Dam Tasks"),
    #date input
    dataTableOutput(ns("damTable")),
    h3("Offspring Tasks"),
    dataTableOutput(ns("offspringTable"))
    
  )
}

taskTableServer <- function(
  id,
  Dam_dates,
  Off_dates
){
  moduleServer(
    id,
    function(input, output, session) {
      output$damTable <- renderDataTable(
        Dam_dates %>%
          filter(is.na(Sac_or_stop)) %>%
          filter(Breed_date > as.Date("2020-12-01")) %>% 
          arrange(DOB, Plug_date),
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 16)
      )
      
      output$offspringTable <- renderDataTable(
        Off_dates %>% filter(DOB > as.Date("2020-12-01")) %>% arrange(DOB),
        options = list(
          scrollX = TRUE,
          scroller = TRUE,
          pageLength = 16)
      )
      
      
    }
  )
}


