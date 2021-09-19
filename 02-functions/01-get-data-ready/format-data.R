#' make factors
#'
#' @param df a dataframe
#' @param cols the columns that you wish to make factors; if using multiple use c(...)
#'
#' @return a dataframe with the indicated columns made factors
#' @export
#'
#' @examples
#' Demo_dam %>%
#' makeFactors(c(
#'  Dam_ID,
#'  Dam,
#'  ParaType,
#'  Litter_num,
#'  Cohort
#' ))
#' 
#' makeFactors(Off_ID, c(mouseID))
#' makeFactors(EndPara_off, mouseID)
makeFactors <- function(df, cols){
  df <- df %>%
    mutate(
      across(
        {{ cols }},
        as.factor
      )
    )
  return(df)
}

#' order early life treatment
#' 
#' Order the factors for early life treatment
#'
#' @param df a dataframe with the column earlyLifeTrt
#'
#' @return a dataframe with the earlyLifeTrt column made a factor with the levels STD and LBN
#' @export
#'
#' @examples
orderEarlyLifeTrt <- function(df){
  df <- df %>%
    mutate(
      earlyLifeTrt = factor(earlyLifeTrt, levels = c("STD", "LBN"))
    )
  return(df)
}

#' combine stress treatment variables
#' 
#' Create a comboStress variable to combine early-life and adult treatments into one variable.
#' This is easier to work with for some plots
#' Factor levels are STD-CON, STD-ALPS, LBN-CON, LBN-ALPS
#'
#' @param df a dataframe with columns earlyLifeTrt and adultTrt
#'
#' @return a dataframe with the column comboTrt added to make four groups
#' @export
#'
#' @examples
combineStress <- function(df){
  df <- df %>%
    unite(
      comboTrt,
      earlyLifeTrt,
      adultTrt,
      sep = "-",
      remove = FALSE
    ) %>%
    mutate(
      comboTrt = factor(comboTrt, levels = c("STD-CON", "STD-ALPS", "LBN-CON", "LBN-ALPS"))
    )
  return(df)
}

#' order adult treatment
#' 
#' order adult treatment variable as CON then ALPS
#' 
#'
#' @param df a dataframe containing the column adultTrt
#'
#' @return a dataframe with adultTrt made a factor with CON and ALPS as the order
#' @export
#'
#' @examples
orderAdultTrt <- function(df){
  df <- df %>%
    mutate(
      adultTrt = factor(adultTrt, levels = c("CON", "ALPS"))
    )
  return(df)
}

#' Use paradigm dates to fill mass at P2 or P4 (start of paradigm) 
#'
#' Takes the average litter mass at the start of the paradigm and the paradigm dates (ParaType)
#' Fills mass at P2 or P4
#' 
#' @param df a dataframe containing the columns Avg_litter_mass_startPara and ParaType
#'
#' @return a dataframe with mass at P2 + P4, corresponding to the start of the paradigm, filled in
#' @export
#'
#' @examples
convertStartPara <- function(df){
  df <- df %>%
    mutate(
      Mass_P2 = case_when(
        ParaType == 2 ~ Avg_litter_mass_startPara,
        ParaType == 4 ~ NA_real_
      ),
      Mass_P4 = case_when(
        ParaType == 4 ~ Avg_litter_mass_startPara,
        ParaType == 2 ~ NA_real_
      ),
      .after = Avg_litter_mass_startPara
    )
  return(df)
}
