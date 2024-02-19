pscProps <- read.csv(file = "./AG_LBN_pscProps.csv", header = TRUE, stringsAsFactors = FALSE)

pscProps$cellID <- factor(pscProps$cellID)
pscProps$mouseID <- factor(pscProps$mouseID)
pscProps$damID <- factor(pscProps$damID)
pscProps$earlyLifeTrt <- factor(pscProps$earlyLifeTrt, levels = c("STD", "LBN"))
pscProps$adultTrt <- factor(pscProps$adultTrt, levels = c("CON", "ALPS"))

library(tidyverse)
library(lqmm)

# Full width half maximum - works -------------------------

quantiles <- c(
  0.25
  , 0.5
  , 0.75
)
FWHM_models <- list()

for (tau in quantiles) {
  FWHM_models[[as.character(tau)]] <- lqmm(
    fwhm ~ earlyLifeTrt * adultTrt
    , random = ~ 1
    , group = cellID
    , tau = tau
    , data = pscProps
    , control = lqmmControl(
      LP_max_iter = 2000
    ) 
  )
  
}

# Summaries for each quantile
for (tau in names(FWHM_models)) {
  cat("Summary for tau =", tau, "quantile:\n")
  print(summary(FWHM_models[[tau]]))
  cat("\n\n")
}

# Rise time - works -------------------------

riseTime_models <- list()

for (tau in quantiles) {
  riseTime_models[[as.character(tau)]] <- lqmm(
    riseTime ~ earlyLifeTrt * adultTrt
    , random = ~ 1
    , group = cellID
    , tau = tau
    , data = pscProps
    , control = lqmmControl(
      LP_max_iter = 2000
    ) 
  )
  
}

# Summaries for each quantile
for (tau in names(riseTime_models)) {
  cat("Summary for tau =", tau, "quantile:\n")
  print(summary(riseTime_models[[tau]]))
  cat("\n\n")
}

# Decay time - NA/inf error ----------------

decay9010_models <- list()

for (tau in quantiles) {
  decay9010_models[[as.character(tau)]] <- lqmm(
    decay9010 ~ earlyLifeTrt * adultTrt
    , random = ~ 1
    , group = cellID
    , tau = tau
    , data = pscProps
    , control = lqmmControl(
      LP_max_iter = 2000
    ) 
  )
  
}

# Summaries for each quantile
for (tau in names(decay9010_models)) {
  cat("Summary for tau =", tau, "quantile:\n")
  print(summary(decay9010_models[[tau]]))
  cat("\n\n")
}

# Amplitude - NA/inf error ----------------

relPeak_models <- list()

for (tau in quantiles) {
  relPeak_models[[as.character(tau)]] <- lqmm(
    relPeak ~ earlyLifeTrt * adultTrt
    , random = ~ 1
    , group = cellID
    , tau = tau
    , data = pscProps
    , control = lqmmControl(
      LP_max_iter = 2000
    ) 
  )
  
}

# Summaries for each quantile
for (tau in names(relPeak_models)) {
  cat("Summary for tau =", tau, "quantile:\n")
  print(summary(relPeak_models[[tau]]))
  cat("\n\n")
}

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

relPeak_scaled_models <- list()

for (tau in quantiles) {
  relPeak_scaled_models[[as.character(tau)]] <- lqmm(
    relPeak_scaled ~ earlyLifeTrt * adultTrt
    , random = ~ 1
    , group = cellID
    , tau = tau
    , data = pscProps
    , control = lqmmControl(
      LP_max_iter = 2000
    ) 
  )
  
}

# Summaries for each quantile
for (tau in names(relPeak_scaled_models)) {
  cat("Summary for tau =", tau, "quantile:\n")
  print(summary(relPeak_scaled_models[[tau]]))
  cat("\n\n")
}

# Scaled decay9010, this seems to work --------------------

pscProps$decay9010_scaled <- scale(pscProps$decay9010)

decay9010_scaled_models <- list()

for (tau in quantiles) {
  decay9010_scaled_models[[as.character(tau)]] <- lqmm(
    decay9010_scaled ~ earlyLifeTrt * adultTrt
    , random = ~ 1
    , group = cellID
    , tau = tau
    , data = pscProps
    , control = lqmmControl(
      LP_max_iter = 2000
    ) 
  )
  
}

# Summaries for each quantile
for (tau in names(decay9010_scaled_models)) {
  cat("Summary for tau =", tau, "quantile:\n")
  print(summary(decay9010_scaled_models[[tau]]))
  cat("\n\n")
}
