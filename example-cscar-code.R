library('lmerTest')
library('lme4')
# library('nlme')

# read data 
# long format
dat <- data.frame(litterID = ..., mouseID = ..., PND = ..., mass =  ..., Tx = ..., sex = ...)
dat$Day11 <- dat$PND - 11

# lmer in lme4
mod <- lmer(mass ~ Tx + Day11 + Tx:Day11 + Sex + nsiblings + (1 | litterID) + (1 + Day11 | mouseID), data = dat, subset = PND < 30)

# (lme in nlme has slightly different syntax)

summary(mod)
plot(mod)

# gamm
# library(mgcv)
library(gamm4)
mod <- gamm4(mass ~ Tx + Day11 + te(Tx, Day11) + Sex + nsiblings + (1 | litterID) + (1 + Day11 | mouseID), data = dat)


##############

mod <- lmer(log10(cort_post) ~ Tx*Stress + Sex*Stress + Tx*Sex + nsiblings + log10(cort_pre) + (1 | litterID), data = dat2)
mod <- lmer(log10(cort_post) ~ Tx*Stress*Sex + nsiblings + log10(cort_pre) + (1 | litterID), data = dat2)

library(effects)



