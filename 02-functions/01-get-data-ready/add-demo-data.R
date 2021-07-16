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

addOffspringDemoData <- function(
  df,
  offDemo_toAdd = Demo_off
){
  df <- df %>%
    left_join(offDemo_toAdd, by = "Mouse_ID") %>%
    relocate( # move to start
      Mouse_ID,
      sex,
      earlyLifeTrt
    )
  
  return(df)
}