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
  damIDName = "damID"
){
  df <- offDF %>%
    left_join(damDemo_forOff, by = damIDName) %>%
    relocate(
      dam,
      DOB,
      .after = damID
    )
  
  if ("cohort" %in% colnames(df)) {
    df <- df %>%
      relocate(
        cohort
        , .after = dam
      )
  }
  
  if ("earlyLifeTrt" %in% colnames(df)) {
    df <- df %>%
      relocate(
        earlyLifeTrt:ParaType
        , .after = DOB
      )
  }
  
  
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
  offDemo_toAdd = Demo_off,
  addBy = "mouseID"
){
  df <- df %>%
    left_join(offDemo_toAdd, by = addBy) %>%
    relocate( # move to start
      mouseID,
      sex
    )
  
  if ("mouseID_spec" %in% colnames(df)) {
    df <- df %>%
      relocate(
        mouseID_spec
      )
  }
  if ("num_ID" %in% colnames(df)) {
    df <- df %>%
      relocate(
        num_ID
        , .after = mouseID
      )
  }
  if ("earlyLifeTrt" %in% colnames(df)) {
    df <- df %>%
      relocate(
        earlyLifeTrt
        , .after = sex
      )
  }
  
  return(df)
}
