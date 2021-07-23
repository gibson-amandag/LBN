getStressTrtNumbers <- function(df){
  countTable <- table(df$adultTrt, df$earlyLifeTrt)
  return(countTable)
}