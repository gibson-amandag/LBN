library('lmerTest') # for p-values
library('lme4')
#library('nlme')

#read data
# long format

# subract 11 from day
mod <- lmer(mass ~ Tx + Day11 + Tx:Day11 )

(1 + Day11 | mouseID) # slope random effects for each mouse

(1|litterID) # may make it harder to id effect of tx

summary(mod)



# generalized additive mixed model

#gamm

library(mgcv)

te(Tx, Day11) -> each treatment can get its own curve
But harder to get comparison of slopes, just graph and curve