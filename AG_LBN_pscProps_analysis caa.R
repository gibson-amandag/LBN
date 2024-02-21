# pscProps <- read.csv(file = "./AG_LBN_pscProps.csv", header = TRUE, stringsAsFactors = FALSE)
pscProps <- read.csv(file = "./AG_LBN_pscPropsOld.csv", header = TRUE, stringsAsFactors = FALSE)

pscProps$cellID <- factor(pscProps$cellID)
pscProps$mouseID <- factor(pscProps$mouseID)
pscProps$damID <- factor(pscProps$damID)
pscProps$earlyLifeTrt <- factor(pscProps$earlyLifeTrt, levels = c("STD", "LBN"))
pscProps$adultTrt <- factor(pscProps$adultTrt, levels = c("CON", "ALPS"))

summary(pscProps)

library(tidyverse)
library(lqmm)

quantiles <- c(
  0.25
  , 0.5
  , 0.75
)

# Full width half maximum - works -------------------------

FWHM_models <- lqmm(
  fwhm ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2000
  ) 
)

FWHM_models
summary(FWHM_models)

# Rise time - works -------------------------

riseTime_models <- lqmm(
  riseTime ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2000
  ) 
)

riseTime_models
summary(riseTime_models)


# Decay time - NA/inf error ----------------

decay9010_models <- lqmm(
  decay9010 ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2000
  ) 
)

decay9010_models
summary(decay9010_models)

# Amplitude - NA/inf error ----------------

relPeak_models <- lqmm(
  relPeak ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2000
  ) 
)

relPeak_models
summary(relPeak_models)

# Troubleshooting -----------------------
## Summary statistics -----------------

summary(pscProps$relPeak)
summary(pscProps$decay9010)

## Checking for NAs/Inf ---------------

sum(is.na(pscProps$relPeak))
sum(is.nan(pscProps$relPeak))
sum(is.infinite(pscProps$relPeak))

sum(is.na(pscProps$decay9010))
sum(is.nan(pscProps$decay9010))
sum(is.infinite(pscProps$decay9010))

any(is.na(pscProps$mouseID))
any(is.na(pscProps$earlyLifeTrt))
any(is.na(pscProps$adultTrt))

## QQ plots ------------------
library(ggplot2)

ggplot(pscProps, aes(sample = relPeak)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot of relPeak") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

ggplot(pscProps, aes(sample = decay9010)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot of decay9010") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

ggplot(pscProps, aes(sample = fwhm)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot of fwhm") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

ggplot(pscProps, aes(sample = riseTime)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Q-Q Plot of riseTime") +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  theme_minimal()

# Scaled relPeak, this seems to work --------------------

pscProps$relPeak_scaled <- scale(pscProps$relPeak)
summary(pscProps$relPeak_scaled)
attributes(pscProps$relPeak_scaled)

relPeak_scaled_models <- lqmm(
  relPeak_scaled ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2000
  ) 
)

relPeak_scaled_models
summary(relPeak_scaled_models)

# Scaled decay9010, this seems to work --------------------

pscProps$decay9010_scaled <- scale(pscProps$decay9010)

decay9010_scaled_models <- lqmm(
  decay9010_scaled ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2000
  ) 
)

decay9010_scaled_models
summary(decay9010_scaled_models)


# log10 transformations --------------------------------

# ultimately need to set a seed before running these, as it changes with each run
logrelPeak_models <- lqmm(
  -log10(-relPeak) ~ earlyLifeTrt * adultTrt
  # log10(absAmp) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2500
  ) 
)

# logrelPeak_models
# summary(logrelPeak_models)

logdecay9010_models <- lqmm(
  log10(decay9010) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2500
  ) 
)

# logdecay9010_models
# summary(logdecay9010_models)

logFWHM_models <- lqmm(
  log10(fwhm) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2500
  ) 
)

# logFWHM_models
# summary(logFWHM_models)

logriseTime_models <- lqmm(
  log10(riseTime) ~ earlyLifeTrt * adultTrt
  , random = ~ 1
  , group = cellID
  , tau = quantiles
  , data = pscProps
  , control = lqmmControl(
    LP_max_iter = 2500
  ) 
)

# logriseTime_models
summary(logriseTime_models)
