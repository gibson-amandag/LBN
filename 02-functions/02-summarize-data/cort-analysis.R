getStressTrtNumbers <- function(df){
  countTable <- table(df$adultTrt, df$earlyLifeTrt)
  return(countTable)
}

cortAnova <- function(df){
  anovaRes <- df %>%
  anova_test(
    dv = cort,
    wid = Mouse_ID,
    between = c(earlyLifeTrt, adultTrt),
    within = time
  )
  flxTbl <- formatAnova(anovaRes)
  return(flxTbl)
  # return(anovaRes)
}

formatAnova <- function(anovaDF){
  flxTbl <- anovaDF %>%
    as_data_frame() %>%
    mutate(
      p = case_when(
        p < 0.001 ~ as.character("<0.001"),
        TRUE ~ as.character(p)
      # ),
      # Effect = case_when(
      #   Effect == "earlyLifeTrt" ~ "early life trt",
      #   Effect == "adultTrt" ~ "adult trt",
      #   Effect == "earlyLifeTrt:adultTrt" ~ "early life x adult trt",
      #   Effect == "earlyLifeTrt:time" ~ "early life trt x time",
      #   Effect == "adultTrt:time" ~ "adult trt x time",
      #   Effect == "earlyLifeTrt:adultTrt:time" ~ "early life x adult trt x time",
      #   TRUE ~ Effect
      )
    ) %>%
    flextable() %>%
    bold(
      i = ~ `p<.05` == "*"
    ) %>%
    fontsize(
      size = 11
    )
  return(flxTbl)
}