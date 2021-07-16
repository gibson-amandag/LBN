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
