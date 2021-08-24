# uses rstatix
meanSummary <- function(df, col){ # colName, col1:col5, c(col1, col3) etc 
  sumDF <- df %>%
    get_summary_stats(
      {{ col }},
      type = "full",
      show = c("n", "mean", "se", "sd")
    ) %>%
    rename(
      "sem" = "se"
    )
  
  return(sumDF)
}

quartilesSummary <- function(df, col){
  sumDF <- df %>%
    get_summary_stats(
      {{ col }},
      type = "five_number"
    )
  return(sumDF)
}


# SUMMARIZE BY DAM ------------------------------------------------------------

groupByDam <- function(df){
  df_byDam <- df %>%
    group_by(Dam_ID)
  return(df_byDam)
}

getAvgByDam <- function(df, damDemo_forOff = Demo_dam_for_offspring){
  avgDF <- df %>%
    groupByDam() %>%
    summarise(
      across(
        where(is.numeric),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    addDamDemoData(
      damDemo_forOff = Demo_dam_for_offspring %>%
        select(!where(is.numeric))
    )
  return(avgDF)
}


# Get maximum -------------------------------------------------------------

getMaxFromRepMeasures <- function(df, col, maxColName, groupingVar){
  df_max <- df %>%
    group_by( {{ groupingVar }} ) %>%
    summarize(
      {{ maxColName }} := max({{ col }}, na.rm = TRUE),
      .groups = "drop"
    )
  return(df_max)
}

# AgeInDays -------------------------------------------------------------

calcAgeInDays <- function(
  df,
  DOBVar = DOB,
  ageAtDateVar = Sac_date,
  DOBisDay = 0
){
  df <- df %>%
    mutate(
      AgeInDays = ifelse(
        !is.na({{ageAtDateVar}}) & !is.na({{DOBVar}}),
        as.numeric({{ageAtDateVar}} - {{DOBVar}}) + DOBisDay,
        NA
      ),
      .after = {{ageAtDateVar}}
    )
  return(df)
}


# Organ mass by body mass -------------------------------------------------

calcOrganMassByBodyMass <- function(
  df,
  organMassVar,
  bodyMassVar = Body_mass_sac
){
  df <- df %>%
    mutate(
      "{{ organMassVar }}_perBody_g" := {{ organMassVar }} / {{ bodyMassVar }},
      .after = {{ organMassVar }}
    )
  return(df)
}
