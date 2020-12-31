#### FUNCTIONS FOR DATES DFs AND PRINTING TASKS #####################################################################################

#### DAM DATES FUNCTIONS -----------------------------------------
damDatesFunc <- function(
  Demo_dam
){
  Dam_dates <- Demo_dam %>%
    select(
      Dam_ID,
      Breed_date,
      Plug_date,
      DOB,
      Sac_or_stop,
      ParaType
    )
  
  Dam_dates <- Dam_dates %>%
    mutate(
      #plug check - true or false, based off of whether this is a plug date, DOB for litter, or sacrifice or stop marked
      #when printing, will want to add a "is Day > breed_date"
      plug_check = ifelse(
        is.na(Plug_date) & is.na(DOB) & is.na(Sac_or_stop), 
        TRUE, 
        FALSE
      ),
      
      #Check for pregnancy of dam
      mass_check = Breed_date + 12 - 1, #will use this only if mass_G12 is NA, meaning Plug_date hasn't occured
      mass_G12 = Plug_date + 12 - 1,
      
      #estimated birth dates
      start_birth_check = Plug_date + 19 - 1,
      est_birth_date = Plug_date + 21 - 1,
      
      #Mass of dam and pups on these days
      mass_startPara = ifelse(
        is.na(DOB), #if there isn't a DOB of the litter
        case_when(
          ParaType == 2 ~ Plug_date + 21 - 1 + 2, #estimate based on plug date
          ParaType == 4 ~ Plug_date + 21 - 1 + 4),
        case_when(
          ParaType == 2 ~ DOB + 2, #if there is a DOB
          ParaType == 4 ~ DOB + 4
        )),
      mass_endPara = 
        case_when(
          ParaType == 2 ~ DOB + 9,
          ParaType == 4 ~ DOB + 11
        ),
      mass_P21 = DOB + 21,
      
      #Mass of only pups on these days
      mass_P10 = ifelse(
        ParaType == 2,
        DOB + 10,
        NA
      ),
      mass_P11 = ifelse(
        ParaType == 2, #P4 start is incorporated above for end of paradigm
        DOB + 11,
        NA
      ),
      mass_P12 = DOB + 12, #both
      mass_P13 = DOB + 13, #both
      mass_P14 = ifelse(
        ParaType == 4, #only P4 start
        DOB + 14,
        NA
      ),
      mass_P15 = DOB + 15, #both
      mass_P16 = ifelse(
        ParaType == 4, #only P4 start
        DOB + 16,
        NA
      ),
      mass_P17 = ifelse(
        ParaType == 2, #only P2 start
        DOB + 10,
        NA
      ),
      mass_P19 = DOB + 19, #both
      mass_P21 = DOB + 21, #both
      
      #paradigm dates
      # to-do adjust these dates
      
      start_paradigm = case_when(
        ParaType == 2 ~ DOB + 2,
        ParaType == 4 ~ DOB + 4
      ),
      end_paradigm = case_when(
        ParaType == 2 ~ DOB + 9,
        ParaType == 4 ~ DOB + 11
      ),
      est_start_paradigm = case_when(
        ParaType == 2 ~ Plug_date + 21 - 1 + 2,
        ParaType == 4 ~ Plug_date + 21 - 1 + 4
      ),
      end_recording = case_when(
        ParaType == 2 ~ DOB + 4,
        ParaType == 4 ~ DOB + 6
      ),
    )
  
  #needed because of logical check
  Dam_dates$mass_startPara <- as_date(Dam_dates$mass_startPara)
  Dam_dates$mass_endPara <- as_date(Dam_dates$mass_endPara)
  Dam_dates$mass_P10 <- as_date(Dam_dates$mass_P10)
  Dam_dates$mass_P11 <- as_date(Dam_dates$mass_P11)
  Dam_dates$mass_P14 <- as_date(Dam_dates$mass_P14)
  Dam_dates$mass_P16 <- as_date(Dam_dates$mass_P16)
  Dam_dates$mass_P17 <- as_date(Dam_dates$mass_P17)
  
  return(Dam_dates)
}

#### OFFSPRING DATES FUNCTIONS ----------------------------------------
offDatesFunc <- function(
  LBN_data
)
{
  Off_dates <- LBN_data %>%
    select(
      Mouse_ID,
      Sex,
      DOB,
      VO_day,
      Estrus_day,
      PreputialSep_day
    )%>%
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
  Off_dates$check_VO <- as_date(Off_dates$check_VO)
  Off_dates$check_Estrus <- as_date(Off_dates$check_Estrus)
  Off_dates$start_cycle <- as_date(Off_dates$start_cycle)
  Off_dates$end_cycle <- as_date(Off_dates$end_cycle)
  Off_dates$check_PPS <- as_date(Off_dates$check_PPS)
  
  return(Off_dates)
}

#### RMARKDOWN FORMATTING ------------------------------------------

### Dam tasks ----------------------------

#Provide the text for the task as a string, 
#provide the value which tells the function which row to reference (val from loop)
#because this is the dams, default to the Dam_dates dataframe
#default to the Dam_ID column

Dam_tasks <- function(
  task_text,
  val,
  df = Dam_dates,
  id_var = "Dam_ID"
){
  if(Count == 0){
    cat(paste0("*", task_text, ":* \n\n"))
    Count <<- 1
  }
  cat(paste0("+ ", df[[id_var]][val], "\n\n"))
}

### Offspring tasks ----------------------------

#Prining output for offspring tasks
#provide the text to print for the task
#provide the value (from val in the script)
#defaults to the Off_dates df
#defaults to the Mouse_ID column
Off_tasks <- function(
  task_text,
  val,
  df = Off_dates,
  id_var = "Mouse_ID"
){
  if(Count == 0){
    #Only print this the first time that the task occurs.
    #Then change the counter to 1
    cat(paste0("*", task_text, ":* \n\n"))
    Count <<- 1
  }
  cat(paste0("+ ", df[[id_var]][val], "\n\n"))
}

### Add a new line ----------------------------

#If Count equals 1, add a new life
#Reset Count to 0
printLine_func <- function(Count){
  if(Count == 1) cat("\n")
  Count <<- 0
}

# printLine_func(Count)

#### HTML FORMATTING ------------------------------------------

#Function for making text blue
blueText <-  function(text){
  paste0("<span style='color:blue'>", text, "</span>")
}

#take a one-dimensional vector (c()) and adds text as the next index value in that vector
list_add <- function(
  list_name,
  text_to_add
){
  list_name[length(list_name)+1] <- text_to_add
  return(list_name)
}

### Dam tasks HTML ----------------------------
Dam_tasks_app <- function(
  task_text,
  val,
  list_name = printCat, #despite the default, this really needs to be explicit in the app
  df = Dam_dates,
  id_var = "Dam_ID"
){
  if(Count == 0){
    #add the text to print to the list
    list_name <- list_add(
      list_name, 
      paste0(
        "<em>", #make this text emphasized
        task_text, ": </em>",#end emphasis
        "<ul style=\"list-style-type:circle;\">" #start a list
      )
    )
    Count <<- 1
  }
  #add the dam id to the list and wrap it in instructions to make it a bullet point
  #the end of the list instruction is in the printLine_func_app function
  #add a checkbox for each item
  list_name <- list_add(list_name, paste0("<li>", df[[id_var]][val], "</li>"))
  return(list_name)
}


### Offspring tasks HTML ----------------------------
Off_tasks_app <- function(
  task_text,
  val,
  list_name = printCat,
  df = Off_dates,
  id_var = "Mouse_ID"
){
  if(Count == 0){
    #Only add to the list the first time that the task occurs.
    #Then change the counter to 1
    list_name <- list_add(
      list_name, 
      paste0(
        "<em>", #make this text strong
        task_text, ": </em>",#end strong
        "<ul style=\"list-style-type:circle;\">"#start a list
      )
    )
    Count <<- 1
  }
  #Add the Mouse_ID to the list wrapped in html instructions to make it a bullet point
  list_name <- list_add(
    list_name, 
    paste0(
      "<li>", df[[id_var]][val], "</li>"
    )
  )
  return(list_name)
}

### Add a new line HTML ----------------------------

#if the counter is 1, that means that a new list has been made.
#Need instructions to end that list and reset count to 0

printLine_func_app <- function(
  Count,
  list_name = printCat
){
  if(Count == 1){
    #end the list
    list_name <- list_add(list_name, "</ul>")
  }
  Count <<- 0
  return(list_name)
}


#### TRUE/FALSE CHECKS ------------------------------------------

### Dams T/F ----------------------------

#Sequence along all of the values in the Dam_ID column
Dam_seq <- function(){
  seq_along(Dam_dates$Dam_ID)
}

#only check mouse if there is not a value (is.na) in the Sac_or_stop column
sac_stop <- function(val){
  is.na(Dam_dates$Sac_or_stop[val])
}

#Check if the date for a given variable is equal to Day
#uses Dam_dates
Dam_day_equals <- function(Day, var, val){
  Day == Dam_dates[[var]][val]
}

# Dam_day_equals("2020-11-30", "Breed_date", 1)

#Check if Day is greater than or equal to the date of a variable
Dam_day_greater <- function(Day, var, val){
  Day >= Dam_dates[[var]][val]
}

# Dam_day_greater("2020-12-13", "mass_check", 1)
# Dam_day_greater("2020-11-11", "mass_check", 1)

#Check that the variable for this mouse is not na
Dam_not.na <- function(var, val){
  !is.na(Dam_dates[[var]][val])
}

# Dam_not.na("Breed_date", 1)
# Dam_not.na("Sac_or_stop", 1)

#Check that the variable for this mouse is na
Dam_is.na <- function(var, val){
  is.na(Dam_dates[[var]][val])
}

# Dam_is.na("Sac_or_stop", 2)
# Dam_is.na("mass_G12", 1)

### Offspring T/F ----------------------------

#Sequence along the Mouse_ID column
Off_seq <- function(){
  seq_along(Off_dates$Mouse_ID)
}

#Equals Day
Off_day_equals <- function(Day, var, val){
  Day == Off_dates[[var]][val]
}

#Day is greater than or equal to
Off_day_greater <- function(Day, var, val){
  Day >= Off_dates[[var]][val]
}

#Day is less than or equal to
Off_day_less <- function(Day, var, val){
  Day <= Off_dates[[var]][val]
}

#Not NA
Off_not.na <- function(var, val){
  !is.na(Off_dates[[var]][val])
}

#Is NA
Off_is.na <- function(var, val){
  is.na(Off_dates[[var]][val])
}