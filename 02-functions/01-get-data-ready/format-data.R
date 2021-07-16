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

orderEarlyLifeTrt <- function(df){
  df <- df %>%
    mutate(
      earlyLifeTrt = factor(earlyLifeTrt, levels = c("STD", "LBN"))
    )
  return(df)
}

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