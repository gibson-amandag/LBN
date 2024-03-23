library(tidyverse)
library(lqmm)
library(rstatix)
library(flextable)
library(afex)

pscProps <- read.csv(file = "./AG_LBN_pscProps.csv", header = TRUE, stringsAsFactors = FALSE)

pscProps$cellID <- factor(pscProps$cellID)
pscProps$mouseID <- factor(pscProps$mouseID)
pscProps$damID <- factor(pscProps$damID)
pscProps$earlyLifeTrt <- factor(pscProps$earlyLifeTrt, levels = c("STD", "LBN"))
pscProps$adultTrt <- factor(pscProps$adultTrt, levels = c("CON", "ALPS"))

pscProps <- pscProps %>%
  mutate(
    amplitude = -relPeak
    , riseTime = ifelse(
      riseTime < 1/10000 * 1000
      , NA
      , riseTime
    )
  )


# effects contrasts to match the rest of the paper, rather than treatment contrasts
# focus on main effects and difference from global mean, versus differences from STD-CON group
set_sum_contrasts() 

# Functions ---------------------------


quartilesSummary <- function(df, col){
  sumDF <- df %>%
    get_summary_stats(
      {{ col }},
      type = "five_number"
    )
  return(sumDF)
}

getLQMMpredictions <- function(
    predRes
    , df = pscProps
    , responseVals = TRUE # transforms from log10
){
  intPredict <- predRes %>%
    as.data.frame() %>%
    bind_cols(
      df %>%
        select(
          cellID
          , mouseID
          , earlyLifeTrt
          , adultTrt
        )
    ) %>%
    group_by(
      earlyLifeTrt
      , adultTrt
    ) %>%
    summarize(
      "mean_0.3" = mean(X0.3.yhat, na.rm = TRUE)
      , "lower_0.3" = mean(X0.3.lower, na.rm = TRUE)
      , "upper_0.3" = mean(X0.3.upper, na.rm = TRUE)
      , "SEM_0.3" = mean(X0.3.SE, na.rm = TRUE)
      , "mean_0.5" = mean(X0.5.yhat, na.rm = TRUE)
      , "lower_0.5" = mean(X0.5.lower, na.rm = TRUE)
      , "upper_0.5" = mean(X0.5.upper, na.rm = TRUE)
      , "SEM_0.5" = mean(X0.5.SE, na.rm = TRUE)
      , "mean_0.7" = mean(X0.7.yhat, na.rm = TRUE)
      , "lower_0.7" = mean(X0.7.lower, na.rm = TRUE)
      , "upper_0.7" = mean(X0.7.upper, na.rm = TRUE)
      , "SEM_0.7" = mean(X0.7.SE, na.rm = TRUE)
      # "mean_0.25" = mean(X0.25.yhat, na.rm = TRUE)
      # , "lower_0.25" = mean(X0.25.lower, na.rm = TRUE)
      # , "upper_0.25" = mean(X0.25.upper, na.rm = TRUE)
      # , "SEM_0.25" = mean(X0.25.SE, na.rm = TRUE)
      # , "mean_0.50" = mean(X0.50.yhat, na.rm = TRUE)
      # , "lower_0.50" = mean(X0.50.lower, na.rm = TRUE)
      # , "upper_0.50" = mean(X0.50.upper, na.rm = TRUE)
      # , "SEM_0.50" = mean(X0.50.SE, na.rm = TRUE)
      # , "mean_0.75" = mean(X0.75.yhat, na.rm = TRUE)
      # , "lower_0.75" = mean(X0.75.lower, na.rm = TRUE)
      # , "upper_0.75" = mean(X0.75.upper, na.rm = TRUE)
      # , "SEM_0.75" = mean(X0.75.SE, na.rm = TRUE)
      , .groups = "drop"
    )
  
  if(responseVals){
    intPredict <- intPredict %>%
      mutate(
        mean_0.3 = 10^mean_0.3
        , lower_0.3 = 10^lower_0.3
        , upper_0.3 = 10^upper_0.3
        , SEM_0.3 = 10^SEM_0.3
        , mean_0.5 = 10^mean_0.5
        , lower_0.5 = 10^lower_0.5
        , upper_0.5 = 10^upper_0.5
        , SEM_0.5 = 10^SEM_0.5
        , mean_0.7 = 10^mean_0.7
        , lower_0.7 = 10^lower_0.7
        , upper_0.7 = 10^upper_0.7
        , SEM_0.7 = 10^SEM_0.7
        # mean_0.25 = 10^mean_0.25
        # , lower_0.25 = 10^lower_0.25
        # , upper_0.25 = 10^upper_0.25
        # , SEM_0.25 = 10^SEM_0.25
        # , mean_0.50 = 10^mean_0.50
        # , lower_0.50 = 10^lower_0.50
        # , upper_0.50 = 10^upper_0.50
        # , SEM_0.50 = 10^SEM_0.50
        # , mean_0.75 = 10^mean_0.75
        # , lower_0.75 = 10^lower_0.75
        # , upper_0.75 = 10^upper_0.75
        # , SEM_0.75 = 10^SEM_0.75
      )
  }
  return(intPredict)
}

## Flextable printing ------------------

simplifyQuartileOutput <- function(df # just one quartile
){
  df %>%
    subEarlyLifeTrtInRowNames_quartile() %>%
    subAdultTrtInRowNames_quartile() %>%
    subColonInRowNames() %>%
    as.data.frame() %>%
    rownames_to_column(var = "fixed effect") %>%
    rename(
      SEM = `Std. Error`
      , p = `Pr(>|t|)`
    ) %>%
    mutate(
      Value = 10^Value
      , SEM = 10^SEM
      , `95% CI` = paste0(
        "[", format(
          round(
            10^`lower bound`
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), ", ", format(
          round(
            10^`upper bound`
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), "]")
      , .after = SEM
    ) %>% 
    select(
      -c(`lower bound`, `upper bound`)
    ) %>%
    formatPCol()
}

simplifyAllQuartilesOutput <- function(allDFs){
  
  resultDF <- NULL
  
  for(name in names(allDFs)){
    df <- allDFs[[name]]
    
    simpDF <- simplifyQuartileOutput(df) %>%
      mutate(
        quartile = name
        , .before = `fixed effect`
      )
    
    resultDF <- bind_rows(resultDF, simpDF)
  }
  
  return(resultDF)
}

simplifyLQMMPredictions <- function(df # just one quartile
){
  df %>%
    mutate(
      `95% CI_0.3` = paste0(
        "[", format(
          round(
            lower_0.3
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), ", ", format(
          round(
            upper_0.3
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), "]")
      , `95% CI_0.5` = paste0(
        "[", format(
          round(
            lower_0.5
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), ", ", format(
          round(
            upper_0.5
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), "]")
      , `95% CI_0.7` = paste0(
        "[", format(
          round(
            lower_0.7
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), ", ", format(
          round(
            upper_0.7
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), "]")
      # `95% CI_0.25` = paste0(
      #   "[", format(
      #     round(
      #       lower_0.25
      #       , 2
      #     )
      #     , nsmall = 2, trim = TRUE
      #   ), ", ", format(
      #     round(
      #       upper_0.25
      #       , 2
      #     )
      #     , nsmall = 2, trim = TRUE
      #   ), "]")
      # , `95% CI_0.50` = paste0(
      #   "[", format(
      #     round(
      #       lower_0.50
      #       , 2
      #     )
      #     , nsmall = 2, trim = TRUE
      #   ), ", ", format(
      #     round(
      #       upper_0.50
      #       , 2
      #     )
      #     , nsmall = 2, trim = TRUE
      #   ), "]")
      # , `95% CI_0.75` = paste0(
      #   "[", format(
      #     round(
      #       lower_0.75
      #       , 2
      #     )
      #     , nsmall = 2, trim = TRUE
      #   ), ", ", format(
      #     round(
      #       upper_0.75
      #       , 2
      #     )
      #     , nsmall = 2, trim = TRUE
      #   ), "]")
    ) %>% 
    select(
      -c(starts_with("lower_"), starts_with("upper_"))
    )
}


subEarlyLifeTrtInRowNames_quartile <- function(tbl, sub = "early-life treatment") {
  row.names(tbl) <- gsub("earlyLifeTrt1", sub, row.names(tbl))
  return(tbl)
}

subAdultTrtInRowNames_quartile <- function(tbl) {
  row.names(tbl) <- gsub("adultTrt1", "adult treatment", row.names(tbl))
  return(tbl)
}

subColonInRowNames <- function(tbl) {
  row.names(tbl) <- gsub("\\:", " * ", row.names(tbl))
  return(tbl)
}


makeManuscriptFlexTable <- function(
    df
    , headerDF = NULL
    , vertLines = c()
    , horzLines = c()
    , fullWidth = TRUE
    , vertMergeCols = c()
    , round1Cols = c()
    , round2Cols = c()
    , round3Cols = c()
){
  if(!is.null(headerDF)){
    tbl <- df %>%
      flextable(
        col_keys = headerDF$col_keys
      ) %>%
      set_header_df(
        mapping = headerDF, key = "col_keys"
      )
  } else {
    tbl <- df %>%
      flextable(
        
      )
  }
  
  if(length(vertMergeCols)>0){
    for (col in vertMergeCols) {
      tbl <- tbl %>%
        merge_v(j = col)
    }
  }
  
  tbl <- tbl %>%
    merge_h(part = "header") %>%
    colformat_num(
      na_str = ""
    )
  
  if(fullWidth){
    tbl <- tbl %>%
      set_table_properties(
        layout = "autofit"
        , width = 1
      )
  } else {
    tbl <- tbl %>%
      set_table_properties(
        layout = "autofit"
      )
  }
  
  if(length(round1Cols)>0){
    tbl <- tbl %>%
      colformat_double(j = round1Cols, digits = 1)
  }
  
  if(length(round2Cols)>0){
    tbl <- tbl %>%
      colformat_double(j = round2Cols, digits = 2)
  }
  
  if(length(round3Cols)>0){
    tbl <- tbl %>%
      colformat_double(j = round3Cols, digits = 3)
  }
  
  tbl <- tbl %>%
    theme_booktabs() %>%
    align(
      align = "center"
      , part = "all"
    )
  
  if(length(vertLines)>0){
    for (col in vertLines) {
      tbl <- tbl %>%
        vline(j = col)
    }
  }
  
  if(length(horzLines)>0){
    for (col in horzLines) {
      tbl <- tbl %>%
        hline(i = col)
    }
  }
  
  tbl <- tbl %>%
    fix_border_issues(
      
    )
  
  return(tbl)
}

formatPCol <- function(df){
  df <- df %>%
    mutate(
      p = ifelse(
        p < 0.001, "<0.001"
        , ifelse(
          p > 0.999, ">0.999"
          , format(round(p, 3), nsmall = 3, trim = TRUE)
        )
      )
    )
  return(df)
}

# Grouped quartile summary ----------------

# Doesn't take into account the cell, but to give a general idea of the range
# for each of the variables

sumQuartiles <- pscProps %>%
  mutate(
    logAmp = log10(amplitude)
    , logRiseTime = log10(riseTime)
    , logDecayTime = log10(decay9010)
    , logFWHM = log10(fwhm)
  ) %>%
  group_by(
    earlyLifeTrt
    , adultTrt
  ) %>%
  quartilesSummary()

View(sumQuartiles)

# Amplitude -------------------------

quantiles <- c(
  # 0.25
  0.3
  , 0.5
  , 0.7
  # , 0.75
)

logAmplitude_models <- lqmm(
  log10(amplitude) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2500
    , LP_tol_ll = 1e-6
    , LP_tol_theta = 1e-6
    , startQR = TRUE # This seems to make it worse for amplitude
  )
)

logAmplitude_models_sum <- summary(
  logAmplitude_models
  , seed = 231
  # , R = 200
)

logAmplitude_models_pred <- predint(
  logAmplitude_models
  , seed = 231
  # , R = 200
)

logAmplitude_models_predictions <- logAmplitude_models_pred %>%
  getLQMMpredictions()

# Rise time ----------------------------


logRiseTime_models <- lqmm(
  log10(riseTime) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps %>%
    filter(
      !is.na(riseTime)
    )
  , control = lqmmControl(
    LP_max_iter = 2500
    , LP_tol_ll = 1e-6
    , LP_tol_theta = 1e-6
    , startQR = TRUE
  )
)

logRiseTime_models_sum <- summary(
  logRiseTime_models
  , seed = 231
  # , R = 200
)

logRiseTime_models_pred <- predint(
  logRiseTime_models
  , seed = 231
  # , R = 200
)

logRiseTime_models_predictions <- logRiseTime_models_pred %>%
  getLQMMpredictions(
    pscProps %>%
      filter(
        !is.na(riseTime)
      )
  ) %>%
  flextable()

# Decay time ------------------------------------

logDecayTime_models <- lqmm(
  log10(decay9010) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2500
    , LP_tol_ll = 1e-6
    , LP_tol_theta = 1e-6
    , startQR = TRUE
  )
)

logDecayTime_models_sum <- summary(
  logDecayTime_models
  , seed = 231
  # , R = 200
)

logDecayTime_models_pred <- predint(
  logDecayTime_models
  , seed = 231
  # , R = 200
)

logDecayTime_models_predictions <- logDecayTime_models_pred %>%
  getLQMMpredictions()

# FWHM ------------------------------------

logFWHM_models <- lqmm(
  log10(fwhm) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2500
    , LP_tol_ll = 1e-6
    , LP_tol_theta = 1e-6
    , startQR = TRUE
  )
)

logFWHM_models_sum <- summary(
  logFWHM_models
  , seed = 231
  # , R = 200
)

logFWHM_models_pred <- predint(
  logFWHM_models
  , seed = 231
  # , R = 200
) 

logFWHM_models_predictions <- logFWHM_models_pred %>%
  getLQMMpredictions()


# Flextables -------------------------

## Summary ------------------------

logAmplitude_models_sum_tbl <- logAmplitude_models_sum$tTable %>%
  simplifyAllQuartilesOutput()

logRiseTime_models_sum_tbl <- logRiseTime_models_sum$tTable %>%
  simplifyAllQuartilesOutput()

logDecayTime_models_sum_tbl <- logDecayTime_models_sum$tTable %>%
  simplifyAllQuartilesOutput()

logFWHM_models_sum_tbl <- logFWHM_models_sum$tTable %>%
  simplifyAllQuartilesOutput()

logModels_tbl <- bind_rows(
  list(
    "amplitude (pA)" = logAmplitude_models_sum_tbl
    , "rise time (ms)" = logRiseTime_models_sum_tbl
    , "decay time (ms)" = logDecayTime_models_sum_tbl
    , "FWHM (ms)" = logFWHM_models_sum_tbl
  )
  , .id = "feature"
) %>%
  pivot_wider(
    id_cols = c(feature, `fixed effect`), names_from = quartile, values_from = c("Value", "SEM", "95% CI", "p")
  )

logModels_header <- data.frame(
  col_keys = c("feature", "fixed effect"
               , "Value_0.3", "SEM_0.3", "95% CI_0.3", "p_0.3"
               , "Value_0.5", "SEM_0.5", "95% CI_0.5", "p_0.5"
               , "Value_0.7", "SEM_0.7", "95% CI_0.7", "p_0.7"
               # , "Value_0.25", "SEM_0.25", "95% CI_0.25", "p_0.25"
               # , "Value_0.5", "SEM_0.5", "95% CI_0.5", "p_0.5"
               # , "Value_0.75", "SEM_0.75", "95% CI_0.75", "p_0.75"
  )
  , line2 = c("", ""
              , rep("30th percentile", 4)
              , rep("50th percentile", 4)
              , rep("70th percentile", 4)
              # , rep("25th percentile", 4)
              # , rep("50th percentile", 4)
              # , rep("75th percentile", 4)
  )
  , line3 = c("feature", "fixed effect"
              , "Value", "SEM", "95% CI", "p"
              , "Value", "SEM", "95% CI", "p"
              , "Value", "SEM", "95% CI", "p"
  )
)

logModels_flexTable <- logModels_tbl %>%
  makeManuscriptFlexTable(
    headerDF = logModels_header
    , vertLines = c(2, 6, 10)
    , horzLines = c(4, 8, 12, 16)
    , round2Cols = c("Value_0.3", "Value_0.5", "Value_0.7")
    , round3Cols = c("SEM_0.3", "SEM_0.5", "SEM_0.7")
    # , round2Cols = c("Value_0.25", "Value_0.5", "Value_0.75")
    # , round3Cols = c("SEM_0.25", "SEM_0.5", "SEM_0.75")
    , vertMergeCols = c("feature")
  )

print(logModels_flexTable)

## Predictions -------------------


logAmplitude_models_predictions_tbl <- logAmplitude_models_predictions %>%
  simplifyLQMMPredictions()
logRiseTime_models_predictions_tbl <- logRiseTime_models_predictions %>%
  simplifyLQMMPredictions()
logDecayTime_models_predictions_tbl <- logDecayTime_models_predictions %>%
  simplifyLQMMPredictions()
logFWHM_models_predictions_tbl <- logFWHM_models_predictions %>%
  simplifyLQMMPredictions()

logModels_predictions_tbl <- bind_rows(
  list(
    "amplitude (pA)" = logAmplitude_models_predictions_tbl
    , "rise time (ms)" = logRiseTime_models_predictions_tbl
    , "decay time (ms)" = logDecayTime_models_predictions_tbl
    , "FWHM (ms)" = logFWHM_models_predictions_tbl
  )
  , .id = "feature"
)

logModels_predictions_header <- data.frame(
  col_keys = c("feature", "earlyLifeTrt", "adultTrt"
               , "mean_0.3", "SEM_0.3", "95% CI_0.3"
               , "mean_0.5", "SEM_0.5", "95% CI_0.5"
               , "mean_0.7", "SEM_0.7", "95% CI_0.7"
               # , "mean_0.25", "SEM_0.25", "95% CI_0.25"
               # , "mean_0.50", "SEM_0.50", "95% CI_0.50"
               # , "mean_0.75", "SEM_0.75", "95% CI_0.75"
  )
  , line2 = c("", "", ""
              , rep("30th percentile", 3)
              , rep("50th percentile", 3)
              , rep("70th percentile", 3)
              # , rep("25th percentile", 3)
              # , rep("50th percentile", 3)
              # , rep("75th percentile", 3)
  )
  , line3 = c("feature", "early-life treatment", "adult treatment"
              , "mean", "SEM", "95% CI"
              , "mean", "SEM", "95% CI"
              , "mean", "SEM", "95% CI"
  )
)

logModels_predictions_flexTable <- logModels_predictions_tbl %>%
  makeManuscriptFlexTable(
    headerDF = logModels_predictions_header
    , vertLines = c(3, 6, 9)
    , horzLines = c(2, 4, 6, 8, 10, 12, 14, 16)
    , round2Cols = c("mean_0.3", "mean_0.5", "mean_0.7")
    , round3Cols = c("SEM_0.3", "SEM_0.5", "SEM_0.7")
    # , round2Cols = c("mean_0.25", "mean_0.50", "mean_0.75")
    # , round3Cols = c("SEM_0.25", "SEM_0.50", "SEM_0.75")
    , vertMergeCols = c("feature", "earlyLifeTrt")
  )

print(logModels_predictions_flexTable)
