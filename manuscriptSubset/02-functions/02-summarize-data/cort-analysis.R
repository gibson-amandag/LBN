#' Get a table with numbers of animals in stress treatment groups
#' 
#' A count table for early-life and adult stress treatment groups
#'
#' @param df a dataframe with columns adultTrt and earlyLifeTrt
#'
#' @return a count table
#' @export
#'
#' @examples
getStressTrtNumbers <- function(df){
  countTable <- table(df$adultTrt, df$earlyLifeTrt)
  return(countTable)
}

#' Analyze two-way anova for early-life and adult treatment effects on corticosterone
#'
#' Runs the two-way anova with rstatix::anova_test(). 
#' The dependent variable is cort
#' Within animal id is mouseID
#' Between animal variables are early-life and adult treatment (earlyLifeTrt, adultTrt)
#' The within animal variable is time
#' 
#' Formats the output as a flextable using formatAnova()
#' 
#' @param df a long-form data frame with the columns cort, mouseID, earlyLifeTrt, adultTrt, and time
#'
#' @return a flextable with the formatted anova results
#' @export
#'
#' @examples
cortAnova <- function(
  df,
  byCycle = FALSE
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
  ){
  anovaRes <- df %>%
    anova_test(
      dv = cort,
      wid = mouseID,
      between = c(earlyLifeTrt, adultTrt),
      within = time
    )
  if(byCycle){
    anovaRes <- df %>%
      anova_test(
        dv = cort,
        wid = mouseID,
        between = c(earlyLifeTrt, adultTrt, Sac_cycle),
        within = time
      )
  }
  flxTbl <- formatAnova(anovaRes, fontSize = fontSize, addWVal = addWVal, addHVal = addHVal)
  return(flxTbl)
  # return(anovaRes)
}
cortAnova_returnBoth <- function(
  df,
  byCycle = FALSE
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
  ){
  anovaRes <- df %>%
    anova_test(
      dv = cort,
      wid = mouseID,
      between = c(earlyLifeTrt, adultTrt),
      within = time
    )
  if(byCycle){
    anovaRes <- df %>%
      anova_test(
        dv = cort,
        wid = mouseID,
        between = c(earlyLifeTrt, adultTrt, Sac_cycle),
        within = time
      )
  }
  flxTbl <- formatAnova(anovaRes, fontSize = fontSize, addWVal = addWVal, addHVal = addHVal)
  return(list(
      anova = anovaRes %>% get_anova_table() %>% # added 2021-10-23
        as_tibble() # replaced as_data_frame()
      , flxTbl = flxTbl
    )
  )
}

# Post-hoc ANOVA functions ---------------------------------------------------------

groupForAnovaPostHoc <- function(df, groupVar){
  groupedDF <- df %>%
    group_by({{ groupVar }})
  
  groups <- groupedDF %>% group_keys()
  numGroups <- length(groups[[1]])
  return(list(
    df = groupedDF,
    groups = groups,
    num = numGroups
  ))
}

cortAnova_2wayPost_timeAdult <- function(df){
  groupedByTime <- df %>%
    groupForAnovaPostHoc(time)
  
  anovaByTime <- groupedByTime$df %>%
    anova_test(
      dv = cort,
      wid = mouseID,
      between = c(adultTrt)
    )
  
  anovaByTime %>%
    get_anova_table() %>%
    as_tibble()%>%
    mutate(
      p.adj = ifelse(p*groupedByTime$num < 1, p*groupedByTime$num, 1)
    ) %>%
    formatAdjAnova()
}

cortAnova_2wayPost_earlyLifeAdult <- function(df){
  groupedByAdult <- df %>%
    groupForAnovaPostHoc(adultTrt)

  anovaByAdult <- groupedByAdult$df %>%
    anova_test(
      dv = cort,
      wid = mouseID,
      between = c(earlyLifeTrt)
    )

  anovaByAdult %>%
    get_anova_table() %>%
    as_tibble()%>%
    mutate(
      p.adj = ifelse(p*groupedByAdult$num < 1, p*groupedByAdult$num, 1)
    ) %>%
    formatAdjAnova()
}

cortAnova_3wayPost <- function(df){
  groupedByTime <- df %>%
    groupForAnovaPostHoc(time)
  
  anovaByTime <- groupedByTime$df %>%
    anova_test(
      dv = cort,
      wid = mouseID,
      between = c(earlyLifeTrt, adultTrt)
    )
  
  anovaByTime %>%
    get_anova_table() %>%
    as_tibble()%>%
    mutate(
      p.adj = ifelse(p*groupedByTime$num < 1, p*groupedByTime$num, 1)
    ) %>%
    formatAdjAnova()
}




