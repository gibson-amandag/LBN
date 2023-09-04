set_sum_contrasts()
set_null_device("png")


# Dam behavior - exits ------------------------------------------------------

#Chose a negative binomial distritution because this is count data, but it is
#overdispersed, meaning that the variation is more than the means (which
#violates assumptions for a Poisson distribution). 

#This would suggest that there's an effect of both treatment and experimental
#day on the number of exits that the dam makes from the nest. There's not a lot
#of evidence here for an interaction between day and treatment, though it's
#harder to tell with each of the individual effects reported.

## Model -------------------------------------------------------------------

numExits_nb.GLMM <- glmer.nb(
  Num_exits ~ earlyLifeTrt * PND + (1|damID)
  , data = damBehavior_byPND_ZT %>%
    makeFactors(c(PND, ZT))
)

# Closest I've gotten to an ANOVA-like analysis. Not sure if this is best
numExits_nb.GLMM_jointTest <- joint_tests(numExits_nb.GLMM)


### Post-hoc ----------------------------------------------------------------

numExits_nb.GLMM.earlyLifeEMM <- emmeans(
  numExits_nb.GLMM
  , "earlyLifeTrt"
  , type = "response"
)

numExits_nb.GLMM.earlyLifeEMM.pairs <- pairs(numExits_nb.GLMM.earlyLifeEMM)



### Errors for graph --------------------------------------------------------

numExits_nb.GLMM_errors <- numExits_nb.GLMM %>%
  getErrorDF_LMM("PND", panel = "earlyLifeTrt")

numExits_nb.GLMM_errors.earlyLifeEMM <- numExits_nb.GLMM %>%
  getErrorDF_LMM("earlyLifeTrt")



# Dam behavior - perc time off nest ---------------------------------------


## Model -------------------------------------------------------------------

percOffNest_lmm <- mixed(
  Perc_off_nest ~ earlyLifeTrt * PND + (1|damID)
  , data = damBehavior_byPND_ZT %>%
    makeFactors(c(PND, ZT))
  , method = "KR"
)

# No interactions or main effects -> no post-hocs


### Errors for graph --------------------------------------------------------

percOffNest_lmm_errors <- percOffNest_lmm %>%
  getErrorDF_LMM("PND", panel = "earlyLifeTrt")

numExits_nb.GLMM_errors.earlyLifeEMM <- percOffNest_lmm %>%
  getErrorDF_LMM("earlyLifeTrt")


# Dam Mass ----------------------------------------------------------------


## Model -------------------------------------------------------------------

damMass_lmm <- mixed(
  mass ~ earlyLifeTrt * PND + (1|damID)
  , data = damMassFiltered %>%
    mutate(
      PND = as.factor(day)
    )
  , method = "KR"
)


### Post-hoc ----------------------------------------------------------------

# Main effect of PND and main effect of LBN. Trending, but not sig interaction
# However, when the interaction is run, it's showing that the difference
# really seems to be emerging after the treatment, and it's not just
# that they're bigger before hand

damMass_lmm_EMM.PND <- emmeans(
  damMass_lmm$full_model
  , "PND"
)

# Each day is different - PND 4 is less than both PND 11 and PND 21
# PND 11 is more than PND 21
damMass_lmm_EMM.PND.pairs <- pairs(damMass_lmm_EMM.PND, adjust = "holm")


damMass_lmm_EMM.earlyLifeTrt <- emmeans(
  damMass_lmm$full_model
  , "earlyLifeTrt"
)

# LBN more than STD
damMass_lmm_EMM.earlyLifeTrt.pairs <- pairs(damMass_lmm_EMM.earlyLifeTrt, adjust = "holm")


### Errors for graph --------------------------------------------------------

damMass_lmm_errors <- damMass_lmm %>%
  getErrorDF_LMM("PND", panel = "earlyLifeTrt")


# Dam cort ----------------------------------------------------------------

damCort_t.Test <- t.test(
  Cort_dam_P11 ~ earlyLifeTrt, data = damFiltered
)
