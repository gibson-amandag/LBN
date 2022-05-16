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
  flxTbl <- formatAnova(anovaRes)
  return(flxTbl)
  # return(anovaRes)
}

#' Format LBN x ALPS stress anova output
#' 
#' takes the rstatix::anova_test() output and makes it a dataframe, then mutates to make the following changes:
#' If p is less than 0.001, it is reported as "<0.001"
#' Makes it a flextable
#' Bolds rows where p is < 0.05
#' Font size is 11
#'
#' @param anovaDF 
#'
#' @return
#' @export
#'
#' @examples
formatAnova <- function(anovaDF){
  flxTbl <- anovaDF %>%
    get_anova_table() %>% # added 2021-10-23
    as_tibble() %>% # replaced as_data_frame()
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

formatAdjAnova <- function(anovaDF){
  flxTbl <- anovaDF %>%
    get_anova_table() %>%
    as_tibble() %>% # replaced as_data_frame()
    mutate(
      p = case_when(
        p < 0.001 ~ as.character("<0.001"),
        TRUE ~ as.character(p)
      ),
      'p.adj<.05' = case_when(
        p.adj < 0.05 ~ "*",
        TRUE ~ ""
      ),
      p.adj = case_when(
        p.adj < 0.001 ~ as.character("<0.001"),
        p.adj > 0.999 ~ as.character(">0.999"),
        TRUE ~ as.character(round(p.adj, 3))
      )
    ) %>%
    select(-`p<.05`) %>%
    flextable() %>%
    bold(
      i = ~ `p.adj<.05` == "*"
    ) %>%
    fontsize(
      size = 11
    )
  return(flxTbl)
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




