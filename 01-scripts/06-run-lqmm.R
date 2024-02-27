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
  , control = lqmmControl(
    LP_max_iter = 1000
  )
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
  , control = lqmmControl(
    LP_max_iter = 1000
  )
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
  , control = lqmmControl(
    LP_max_iter = 1000
  )
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
  , control = lqmmControl(
    LP_max_iter = 1000
  )
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