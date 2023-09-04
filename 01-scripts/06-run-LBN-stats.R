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

numExits_nb.GLMM <- glmer.nb(
  Num_exits ~ earlyLifeTrt * PND + (1|damID)
  , data = damBehavior_byPND_ZT %>%
    makeFactors(c(PND, ZT))
)

# Closest I've gotten to an ANOVA-like analysis. Not sure if this is best
numExits_nb.GLMM_jointTest <- joint_tests(numExits_nb.GLMM)


## Post-hoc ----------------------------------------------------------------

numExits_nb.GLMM.earlyLifeEMM <- emmeans(
  numExits_nb.GLMM
  , "earlyLifeTrt"
  , type = "response"
)

numExits_nb.GLMM.earlyLifeEMM.pairs <- pairs(numExits_nb.GLMM.earlyLifeEMM)



## Errors for graph --------------------------------------------------------

numExits_nb.GLMM_errors <- numExits_nb.GLMM %>%
  getErrorDF_LMM("PND", panel = "earlyLifeTrt")

numExits_nb.GLMM_errors.earlyLifeEMM <- numExits_nb.GLMM %>%
  getErrorDF_LMM("earlyLifeTrt")



# Dam behavior - perc time off nest ---------------------------------------


percOffNest_lmm <- mixed(
  Perc_off_nest ~ earlyLifeTrt * PND + (1|damID)
  , data = damBehavior_byPND_ZT %>%
    makeFactors(c(PND, ZT))
  , method = "KR"
)

# No interactions or main effects -> no post-hocs


## Errors for graph --------------------------------------------------------

percOffNest_lmm_errors <- percOffNest_lmm %>%
  getErrorDF_LMM("PND", panel = "earlyLifeTrt")

numExits_nb.GLMM_errors.earlyLifeEMM <- percOffNest_lmm %>%
  getErrorDF_LMM("earlyLifeTrt")


# Dam Mass ----------------------------------------------------------------


damMass_lmm <- mixed(
  mass ~ earlyLifeTrt * PND + (1|damID)
  , data = damMassFiltered %>%
    mutate(
      PND = as.factor(day)
    )
  , method = "KR"
)


## Post-hoc ----------------------------------------------------------------

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


## Errors for graph --------------------------------------------------------

damMass_lmm_errors <- damMass_lmm %>%
  getErrorDF_LMM("PND", panel = "earlyLifeTrt")


# Dam cort ----------------------------------------------------------------

damCort_t.Test <- t.test(
  Cort_dam_P11 ~ earlyLifeTrt, data = damFiltered
)


# Offspring mass ----------------------------------------------------------



# Offspring maturation ----------------------------------------------------


VO_age_lmm <- mixed(
  VO_age ~ earlyLifeTrt + (1|damID)
  , data = maturationFiltered
  , method = "KR"
)

Estrus_age_lmm <- mixed(
  Estrus_age ~ earlyLifeTrt + (1|damID)
  , data = maturationFiltered
  , method = "KR"
)

PreputialSep_age_lmm <- mixed(
  PreputialSep_age ~ earlyLifeTrt + (1|damID)
  , data = maturationFiltered
  , method = "KR"
)

VO_mass_lmm <- mixed(
  VO_mass ~ earlyLifeTrt + (1|damID)
  , data = maturationFiltered
  , method = "KR"
)

Estrus_mass_lmm <- mixed(
  Estrus_mass ~ earlyLifeTrt + (1|damID)
  , data = maturationFiltered
  , method = "KR"
)

PreputialSep_mass_lmm <- mixed(
  PreputialSep_mass ~ earlyLifeTrt + (1|damID)
  , data = maturationFiltered
  , method = "KR"
)


## Errors for graphs -------------------------------------------------------

VO_age_lmm_errors <- VO_age_lmm %>%
  getErrorDF_LMM(
    xVarAsChar = "earlyLifeTrt"
  ) %>%
  mutate(
    matType = "vaginal opening"
  )

Estrus_age_lmm_errors <- Estrus_age_lmm %>%
  getErrorDF_LMM(
    xVarAsChar = "earlyLifeTrt"
  ) %>%
  mutate(
    matType = "first estrus"
  )

PreputialSep_age_lmm_errors <- PreputialSep_age_lmm %>%
  getErrorDF_LMM(
    xVarAsChar = "earlyLifeTrt"
  ) %>%
  mutate(
    matType = "preputial separation"
  )

age_lmm_errors <- rbind(
  VO_age_lmm_errors
  , Estrus_age_lmm_errors
  , PreputialSep_age_lmm_errors
) %>%
  mutate(
    matType = factor(matType, levels = c("vaginal opening", "first estrus", "preputial separation"))
  )

VO_mass_lmm_errors <- VO_mass_lmm %>%
  getErrorDF_LMM(
    xVarAsChar = "earlyLifeTrt"
  ) %>%
  mutate(
    matType = "vaginal opening"
  )

Estrus_mass_lmm_errors <- Estrus_mass_lmm %>%
  getErrorDF_LMM(
    xVarAsChar = "earlyLifeTrt"
  ) %>%
  mutate(
    matType = "first estrus"
  )

PreputialSep_mass_lmm_errors <- PreputialSep_mass_lmm %>%
  getErrorDF_LMM(
    xVarAsChar = "earlyLifeTrt"
  ) %>%
  mutate(
    matType = "preputial separation"
  )

mass_lmm_errors <- rbind(
  VO_mass_lmm_errors
  , Estrus_mass_lmm_errors
  , PreputialSep_mass_lmm_errors
)%>%
  mutate(
    matType = factor(matType, levels = c("vaginal opening", "first estrus", "preputial separation"))
  )


# AGD ---------------------------------------------------------------------

AGD_lmm <- mixed(
  AGD_adult ~ earlyLifeTrt * sex + (1|damID)
  , data = maturationFiltered
  , method = "KR"
)


## Post-hoc ----------------------------------------------------------------

AGD_lmm_emm_sex <- emmeans(
  AGD_lmm
  , "sex"
)

AGD_lmm_emm_sex.pairs <- pairs(AGD_lmm_emm_sex)


## Errors for graph --------------------------------------------------------

AGD_lmm_errors <- AGD_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "sex"
  )


# Estrous cycles - number -------------------------------------------------

# Warning about singular fit
# damID doesn't really add anything to model
numCycles_lmm <- mixed(
  numCycles ~ earlyLifeTrt + (1|damID)
  , data = cyclesFiltered
  , method = "KR"
)



