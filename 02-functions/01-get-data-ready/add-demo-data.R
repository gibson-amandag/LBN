#' addDamDemoData
#' 
#' add the demographic information from a dam dataframe to the offspring dataframe
#' matches the dam ID column in the two dataframes
#'
#' @param offDF a dataframe with the offspring data
#' @param damDemo_forOff a dataframe with the dam demographic data to be added to offDF
#' @param damIDName the name of the dam ID column from the damDemo_forOff dataframe
#'
#' @return a dataframe with the dam demographic info added to the offspring dataframe
#' @export
#'
#' @examples
addDamDemoData <- function(
  offDF,
  damDemo_forOff,
  damIDName = "Dam_ID"
){
  df <- offDF %>%
    left_join(damDemo_forOff, by = damIDName) %>%
    relocate(
      Dam,
      Cohort,
      DOB,
      earlyLifeTrt:ParaType,
      .after = Dam_ID
    )
  return(df)
}

#' addOffspringDemoData
#' 
#' Add demographic data to an exisiting dataframe
#' Relocates mouseID, num_ID, sex,and earlyLifeTrt to the beginning of the df
#'
#' @param df a dataframe to which you want to add demographic data
#' @param offDemo_toAdd a dataframe that contains the demographic data that you wish to add
#'
#' @return a dataframe with the demographic data added to df
#' @export
#'
#' @examples
addOffspringDemoData <- function(
  df,
  offDemo_toAdd = Demo_off
){
  df <- df %>%
    left_join(offDemo_toAdd, by = "mouseID") %>%
    relocate( # move to start
      mouseID,
      num_ID,
      sex,
      earlyLifeTrt
    )
  
  return(df)
}
