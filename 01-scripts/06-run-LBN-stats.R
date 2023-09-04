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



