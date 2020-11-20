#### FUNCTIONS FOR PRINTING TASKS #####################################################################################


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
    list_name <- list_add(list_name, paste0("<em>", #make this text emphasized
                                            task_text, ": </em>",#end emphasis
                                            "<ul>"#start a list
    ))
    Count <<- 1
  }
  #add the dam id to the list and wrap it in instructions to make it a bullet point
  #the end of the list instruction is in the printLine_func_app function
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
    list_name <- list_add(list_name, paste0("<em>", #make this text strong
                                            task_text, ": </em>",#end strong
                                            "<ul>"#start a list
    ))
    Count <<- 1
  }
  #Add the Mouse_ID to the list wrapped in html instructions to make it a bullet point
  list_name <- list_add(list_name, paste0("<li>", df[[id_var]][val], "</li>"))
  return(list_name)
}

### Add a new line HTML ----------------------------

#if the counter is 1, that means that a new list has been made.
#Need instructions to end that list and reset count to 0

printLine_func_app <- function(Count,
                               list_name = printCat){
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