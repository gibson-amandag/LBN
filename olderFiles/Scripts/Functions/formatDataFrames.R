renameStressGroups <- function(
  df,
  earlyLifeNonStress = "STD",
  earlyLifeStress = "LBN",
  adultNonStress = "CON",
  adultStress = "ALPS"
  ){
  df <- df %>%
    mutate(
      Treatment = factor(Treatment, levels = c("Control", "LBN"), labels = c("STD", "LBN")),
      Stress_treatment = factor(Stress_treatment, levels = c("Control", "Stress"), labels = c("CON", "ALPS"))
    )
  return(df)
}

combineStressGroups <- function (
  df
) {
  df <- df %>%
    unite(
    comboTrt,
    Treatment,
    Stress_treatment,
    sep = "-",
    remove = FALSE
  ) %>%
    mutate(
      comboTrt = factor(comboTrt, levels = c("STD-CON", "STD-ALPS", "LBN-CON", "LBN-ALPS"))
    )
  return(df)
}

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