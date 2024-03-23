# effects contrasts to match the rest of the paper, rather than treatment contrasts
# focus on main effects and difference from global mean, versus differences from STD-CON group
# set_sum_contrasts() 

# Functions ---------------------------

getLQMMpredictions <- function(
    predRes
    , df = pscProps
    , responseVals = TRUE # transforms from log10
    , forPlot = FALSE
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
      "estimate_0.5" = mean(yhat, na.rm = TRUE)
      , "lower_0.5" = mean(lower, na.rm = TRUE)
      , "upper_0.5" = mean(upper, na.rm = TRUE)
      , "SEM_0.5" = mean(SE, na.rm = TRUE)
      , .groups = "drop"
    )
  
  if(responseVals){
    intPredict <- intPredict %>%
      mutate(
        estimate_0.5 = 10^estimate_0.5
        , lower_0.5 = 10^lower_0.5
        , upper_0.5 = 10^upper_0.5
        , SEM_0.5 = 10^SEM_0.5
      )
  }
  
  if(forPlot){
    intPredict <- intPredict %>%
      rename(
        y = estimate_0.5
        , SE = SEM_0.5
        , lower = lower_0.5
        , upper = upper_0.5
      )
  }
  return(intPredict)
}

## Flextable printing ------------------

simplifyQuartileOutput <- function(df # just one quartile
){
  df %>%
    # subEarlyLifeTrtInRowNames_quartile() %>%
    # subAdultTrtInRowNames_quartile() %>%
    # subColonInRowNames() %>%
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
      `95% CI_0.5` = paste0(
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
    ) %>% 
    select(
      -c(starts_with("lower_"), starts_with("upper_"))
    )
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

# View(sumQuartiles)

# Amplitude -------------------------

quantiles <- c(
  0.5
)

logAmplitude_models <- lqmm(
  log10(amplitude) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    # LP_max_iter = 2500
    # , LP_tol_ll = 1e-6
    # , LP_tol_theta = 1e-6
    # , startQR = TRUE # This seems to make it worse for amplitude
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

amplitudeMedian_errors <- logAmplitude_models_pred %>%
  getLQMMpredictions(forPlot = TRUE)

# Rise time ----------------------------


logRiseTime_models <- lqmm(
  log10(riseTime) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    # LP_max_iter = 2500
    # , LP_tol_ll = 1e-6
    # , LP_tol_theta = 1e-6
    # , startQR = TRUE
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
  getLQMMpredictions()

riseTimeMedian_errors <- logRiseTime_models_pred %>%
  getLQMMpredictions(forPlot = TRUE)

# Decay time ------------------------------------

logDecayTime_models <- lqmm(
  log10(decay9010) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    # LP_max_iter = 2500
    # , LP_tol_ll = 1e-6
    # , LP_tol_theta = 1e-6
    # , startQR = TRUE
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

decayTimeMedian_errors <- logDecayTime_models_pred %>%
  getLQMMpredictions(forPlot = TRUE)

# FWHM ------------------------------------

logFWHM_models <- lqmm(
  log10(fwhm) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    # LP_max_iter = 2500
    # , LP_tol_ll = 1e-6
    # , LP_tol_theta = 1e-6
    # , startQR = TRUE
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

FWHMMedian_errors <- logFWHM_models_pred %>%
  getLQMMpredictions(forPlot = TRUE)


# Flextables -------------------------

## Summary ------------------------

logAmplitude_models_sum_tbl <- logAmplitude_models_sum$tTable %>%
  simplifyQuartileOutput()
  # simplifyAllQuartilesOutput()

logRiseTime_models_sum_tbl <- logRiseTime_models_sum$tTable %>%
  simplifyQuartileOutput()
  # simplifyAllQuartilesOutput()

logDecayTime_models_sum_tbl <- logDecayTime_models_sum$tTable %>%
  simplifyQuartileOutput()
  # simplifyAllQuartilesOutput()

logFWHM_models_sum_tbl <- logFWHM_models_sum$tTable %>%
  simplifyQuartileOutput()
  # simplifyAllQuartilesOutput()

logModels_tbl <- bind_rows(
  list(
    "amplitude (pA)" = logAmplitude_models_sum_tbl
    , "rise time (ms)" = logRiseTime_models_sum_tbl
    , "decay time (ms)" = logDecayTime_models_sum_tbl
    , "FWHM (ms)" = logFWHM_models_sum_tbl
  )
  , .id = "feature"
# ) %>%
#   pivot_wider(
#     id_cols = c(feature, `fixed effect`), names_from = quartile, values_from = c("Value", "SEM", "95% CI", "p")
  )

logModels_flexTable <- logModels_tbl %>%
  makeManuscriptFlexTable(
    horzLines = c(4, 8, 12, 16)
    , round2Cols = c("Value")
    , round3Cols = c("SEM")
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
  col_keys = colnames(logModels_predictions_tbl)
  , line1 = c("feature", "early-life treatment", "adult treatment"
              , "estimate", "SEM", "95% CI"
  )
)

logModels_predictions_flexTable <- logModels_predictions_tbl %>%
  makeManuscriptFlexTable(
    headerDF = logModels_predictions_header
    , vertLines = c(3)
    , horzLines = c(2, 4, 6, 8, 10, 12, 14, 16)
    , round2Cols = c("estimate_0.5")
    , round3Cols = c("SEM_0.5")
    , vertMergeCols = c("feature", "earlyLifeTrt")
  )

print(logModels_predictions_flexTable)
