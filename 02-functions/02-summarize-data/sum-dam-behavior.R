summarizeDamFrames <- function(
    df # grouped,
    , demoDF = Demo_dam_for_offspring %>% select(-cyclingFolderPath)
){
  sum_df <- df %>%
    summarize(
      across(c(damOnNest, pupsTogether), ~mean(.x, na.rm = TRUE) * 100),
      across(c(distPxl, dist, numClumps, starts_with("clump")), ~ mean(.x, na.rm = TRUE))
      , .groups = "drop"
    ) %>%
    left_join(
      demoDF,
      by = "damID"
    )
}

summarizeDamBehavior <- function(
    df # grouped,
    , demoDF = Demo_dam_for_offspring %>% select(-cyclingFolderPath)
){
  sum_df <- df %>%
    summarize(
      across(c(Duration:Avg_dur_on_nest), ~ mean(.x, na.rm = TRUE))
      , .groups = "drop"
    ) %>%
    left_join(
      demoDF,
      by = "damID"
    )
}