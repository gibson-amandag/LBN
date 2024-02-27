# effects contrasts to match the rest of the paper, rather than treatment contrasts
# focus on main effects and difference from global mean, versus differences from STD-CON group
# set_sum_contrasts() 

# Functions ---------------------------

getLQMM_MedianCI <- function(
    emm
){
  df <- emm %>%
    as.data.frame() %>%
    rename(
      y = response
      , lower = lower.CL
      , upper = upper.CL
    ) %>%
    combineStress()
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
)

logAmplitude_emm <- emmeans(
  logAmplitude_models
  , ~ earlyLifeTrt * adultTrt
  , type = "response"
)

logAmplitude_emm_jt <- joint_tests(
  logAmplitude_emm
)

amplitudeMedian_errors <- logAmplitude_emm %>%
  getLQMM_MedianCI()

# Rise time ----------------------------


logRiseTime_models <- lqmm(
  log10(riseTime) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
)


logRiseTime_emm <- emmeans(
  logRiseTime_models
  , ~ earlyLifeTrt * adultTrt
  , type = "response"
)

logRiseTime_emm_jt <- joint_tests(
  logRiseTime_emm
)

riseTimeMedian_errors <- logRiseTime_emm %>%
  getLQMM_MedianCI()

# Decay time ------------------------------------

logDecayTime_models <- lqmm(
  log10(decay9010) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
)


logDecayTime_emm <- emmeans(
  logDecayTime_models
  , ~ earlyLifeTrt * adultTrt
  , type = "response"
)

logDecayTime_emm_jt <- joint_tests(
  logDecayTime_emm
)

decayTimeMedian_errors <- logDecayTime_emm %>%
  getLQMM_MedianCI()

# FWHM ------------------------------------

logFWHM_models <- lqmm(
  log10(fwhm) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
)


logFWHM_emm <- emmeans(
  logFWHM_models
  , ~ earlyLifeTrt * adultTrt
  , type = "response"
)

logFWHM_emm_jt <- joint_tests(
  logFWHM_emm
)

FWHMMedian_errors <- logFWHM_emm %>%
  getLQMM_MedianCI()

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
