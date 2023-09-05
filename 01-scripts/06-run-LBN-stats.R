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

percOffNest_lmm_errors.earlyLifeEMM <- percOffNest_lmm %>%
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


## Errors for graph --------------------------------------------------------

numCycles_lmm_error <- numCycles_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
  )


# Estrous cycles - length -------------------------------------------------

lengthCycles_log_lmm <- mixed(
  log(cycleLength) ~ earlyLifeTrt + (1|damID)
  , data = cyclesFiltered
  , method = "KR"
)


## Error for graph ---------------------------------------------------------

# Can't get the afex_plot data output to work with the 
# log scale, so manually pulled out from emmeans

lengthCycles_log_lmm_emm <- emmeans(lengthCycles_log_lmm, "earlyLifeTrt", type = "response")

lengthCycles_log_lmm_error <- lengthCycles_log_lmm_emm %>%
  as_data_frame() %>%
  rename(
    y = response
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  )


# Estrous cycles - stage distribution -------------------------------------

cycles_contTable <- table(
  cyclesLong$earlyLifeTrt,
  cyclesLong$stageName
)

cycles_ChiSq <- chisq_test(cycles_contTable)



# Corticosterone ----------------------------------------------------------

cort_lmm <- mixed(
  log(cort) ~ hormoneStatus * earlyLifeTrt * adultTrt * time + (1|mouseID) + (1|damID)
  , data = cortFiltered_M_DiPro
  , method = "KR"
)

# 2023-09-03
# Interaction between adultTrt and time
# Interaction between hormoneStatus and time


## Post-hoc ----------------------------------------------------------------

cort_lmm_emm_adultTrtTime <-  emmeans(
  cort_lmm
  , "adultTrt"
  , by = "time"
  , type = "response"
)

# ALPS at 5 is more than CON at 5
# No difference initially
cort_lmm_emm_adultTrtTime.pairs <- test(
  pairs(cort_lmm_emm_adultTrtTime)
  , by = NULL
  , adjust = "holm"
)

cort_lmm_emm_hormoneStatusTime <-  emmeans(
  cort_lmm
  , "hormoneStatus"
  , by = "time"
  , type = "response"
)

# At time 0 (baseline)
# Trending (p = 0.052) di diff than male (less)
# Di diff than pro (less)
# Pro not diff than male

# At time 5
# Male diff than pro (less)
# (No interaction with adult treatment b/c this is true 
# for both the CON and ALPS groups at hour 5)
cort_lmm_emm_hormoneStatusTime.pairs <- test(
  pairs(cort_lmm_emm_hormoneStatusTime)
  , by = NULL
  , adjust = "holm"
)


## Errors for graph --------------------------------------------------------

cort_lmm_emm <- emmeans(
  cort_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt", "hormoneStatus", "time")
  , type = "response"
  )

cort_lmm_error <- cort_lmm_emm %>%
  as_data_frame() %>%
  rename(
    y = response
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()


# Body mass - AM ----------------------------------------------------------

bodyMassAM_lmm <- mixed(
  Body_mass_AM ~ hormoneStatus * earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFiltered_M_DiPro
  , method = "KR"
)


## Post-hoc ----------------------------------------------------------------

# 2023-09-04 
# Interaction hormone status and adult treamtment
# Interaction hormone status and early-life treatment


bodyMassAM_lmm_emm_adultTrt <- emmeans(
  bodyMassAM_lmm
  , "adultTrt"
  , by = "hormoneStatus"
)

# males that went on to receive ALPS were larger
# than those that received CON treatment
bodyMassAM_lmm_emm_adultTrt.pairs <- test(
  pairs(bodyMassAM_lmm_emm_adultTrt)
  , by = NULL
  , adjust = "holm"
)

bodyMassAM_lmm_emm_earlyLifeTrt <- emmeans(
  bodyMassAM_lmm
  , "earlyLifeTrt"
  , by = "hormoneStatus"
)


# Individual comparisons aren't different, but the only
# one that seems to be trending after adjusting for
# multiple comparisons is that LBN males were trending toward smaller
bodyMassAM_lmm_emm_earlyLifeTrt.pairs <- test(
  pairs(bodyMassAM_lmm_emm_earlyLifeTrt)
  , by = NULL
  , adjust = "holm"
)


## Errors for graph --------------------------------------------------------

bodyMassAM_lmm_error <- emmeans(
  bodyMassAM_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt", "hormoneStatus")
  ) %>%
  as_data_frame() %>%
  rename(
    y = emmean
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()


# % change body mass ------------------------------------------------------


percChangeBodyMass_lmm <- mixed(
  percChangeBodyMass ~ hormoneStatus * earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFiltered_M_DiPro
  , method = "KR"
)


## Post-hoc ----------------------------------------------------------------

# Effect of adultTrt

percChangeBodyMass_lmm_emm_adultTrt <- emmeans(
  percChangeBodyMass_lmm
  , "adultTrt"
)

percChangeBodyMass_lmm_emm_adultTrt.pairs <- test(pairs(percChangeBodyMass_lmm_emm_adultTrt), by = NULL, adjust = "holm")


## Errors for graph --------------------------------------------------------

percChangeBodyMass_lmm_error <- emmeans(
  percChangeBodyMass_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt", "hormoneStatus")
  ) %>%
  as_data_frame() %>%
  rename(
    y = emmean
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()


# Adrenal mass ------------------------------------------------------------

adrenalMass_lmm <- mixed(
  Adrenal_mass_perBodyAM_g ~ hormoneStatus * earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFiltered_M_DiPro
  , method = "KR"
)


## Post-hoc ----------------------------------------------------------------

adrenalMass_lmm_emm_hormoneStatus <- emmeans(
  adrenalMass_lmm
  , "hormoneStatus"
)

# Males are smaller than both female, no difference between di/pro
adrenalMass_lmm_emm_hormoneStatus.pairs <- test(
  pairs(adrenalMass_lmm_emm_hormoneStatus)
  , by = NULL
  , adjust = "holm"
)


## Errors for graph --------------------------------------------------------

adrenalMass_lmm_error <- emmeans(
  adrenalMass_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt", "hormoneStatus")
  ) %>%
  as_data_frame() %>%
  rename(
    y = emmean
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()


# Seminal vesicle mass ----------------------------------------------------

seminalVesicle_lmm <- mixed(
  ReproTract_mass_perBodyAM_g ~ earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFiltered_M_DiPro %>%
    filter(
      sex == "M"
    )
  , method = "KR"
)


## Post-hoc ----------------------------------------------------------------

seminalVesicle_lmm_emm_earlyLifeTrt <- emmeans(
  seminalVesicle_lmm
  , "earlyLifeTrt"
)

# LBN seminal vesicles adjusted for body mass in the AM are larger
# than STD seminal vesicles
# Note, that LBN males are also trending towards being smaller overall in AM
# No absolute difference in seminal vesicle mass
seminalVesicle_lmm_emm_earlyLifeTrt.pairs <- test(
  pairs(seminalVesicle_lmm_emm_earlyLifeTrt)
  , by = NULL
  , adjust = "holm"
)


# Errors for graph --------------------------------------------------------

seminalVesicle_lmm_error <- emmeans(
  seminalVesicle_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt")
  ) %>%
  as_data_frame() %>%
  rename(
    y = emmean
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()



# Uterine mass ------------------------------------------------------------

uterineMass_lmm <- mixed(
  ReproTract_mass_perBodyAM_g ~ earlyLifeTrt * adultTrt * hormoneStatus + (1|damID)
  , data = acuteStressFiltered_M_DiPro %>%
    filter(
      sex == "F"
    )
  , method = "KR"
)


## Post-hoc ----------------------------------------------------------------

# Effect of hormone status
# Trend (p=0.064) for interaction between adultTrt and hormone status

uterineMass_lmm_emm_stage <- emmeans(
  uterineMass_lmm
  , "hormoneStatus"
  
)

# Diestrus is smaller than pro, as it should be
uterineMass_lmm_emm_stage.pairs <- test(
  pairs(uterineMass_lmm_emm_stage)
  , by = NULL
  , adjust = "holm"
)


uterineMass_lmm_emm_adultTrtStage <- emmeans(
  uterineMass_lmm
  , "adultTrt"
  , by = "hormoneStatus"
)

# For diestrus, no difference
# For proestrus, trend that ALPS have a larger uterus relative
# relative to AM body mass

# When normalized to PM body mass (where ALPS are smaller),
# the interaction is significantly different, which makes sense, and would suggest
# that the uterus is not losing mass proportionally to the rest of the body

# I don't have an explaination for why the uterine mass should be relatively
# larger compared to the AM body mass, based on the fact that they're losing
# body mass, and if the uterus was losing mass, I'd expect this to be in the 
# opposite direction

# In absolute mass, similar trend towards larger for pro ALPS, not significant
uterineMass_lmm_emm_adultTrtStage.pairs <- test(
  pairs(uterineMass_lmm_emm_adultTrtStage)
  , by = NULL
  , adjust = "holm"
)

## Errors for graph --------------------------------------------------------


uterineMass_lmm_error <- emmeans(
  uterineMass_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt", "hormoneStatus")
  ) %>%
  as_data_frame() %>%
  rename(
    y = emmean
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()


# Testicular mass ---------------------------------------------------------

testicularMass_lmm <- mixed(
  Gonad_mass_perBodyAM_g ~ earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFiltered_M_DiPro %>%
    filter(
      sex == "M"
    )
  , method = "KR"
)


## Post-hoc ----------------------------------------------------------------

# Effect of adult treatment

testicularMass_lmm_emm_adultTrt <- emmeans(
  testicularMass_lmm
  , "adultTrt"
)

# ALPS testicular mass is smaller relative to AM body mass

# Holds with relative to PM mass, and absolute (with absolute, also early-life diff)
testicularMass_lmm_emm_adultTrt.pairs <- test(
  pairs(testicularMass_lmm_emm_adultTrt)
  , by = NULL
  , adjust = "holm"
)


## Errors for graph --------------------------------------------------------

testicularMass_lmm_error <- emmeans(
  testicularMass_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt")
  ) %>%
  as_data_frame() %>%
  rename(
    y = emmean
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()


# GABA - capacitance ------------------------------------------------------

capacitance_lmm <- mixed(
  capacitance ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240FilteredFiring
  , method = "KR"
)


## Errors for graph --------------------------------------------------------

capacitance_lmm_errors <- capacitance_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()


# GABA - input resistance -------------------------------------------------


inputResistance_lmm <- mixed(
  Rinput ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240FilteredFiring
  , method = "KR"
)


## Errors for graph --------------------------------------------------------

inputResistance_lmm_errors <- inputResistance_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()


# GABA - series resistance ------------------------------------------------

seriesResistance_lmm <- mixed(
  Rseries ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240FilteredFiring
  , method = "KR"
)


## Errors for graph --------------------------------------------------------

seriesResistance_lmm_errors <- seriesResistance_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()


# GABA - holding current --------------------------------------------------

holdingCurrent_lmm <- mixed(
  holdingCurrent ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240FilteredFiring
  , method = "KR"
)


## Errors for graph --------------------------------------------------------

holdingCurrent_lmm_errors <- holdingCurrent_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()


# GABA - PSC frequency ----------------------------------------------------

# There's a lot of skew to the data, but there's a true zero
# So can't just do a log transformation

# Negative binomial of counts of PSCs in 2 min

GABApscs_240_count <- GABApscs_240FilteredFiring %>%
  mutate(
    numEvents = frequency * duration
    , .after = frequency
  )

numEvents_nb.GLMM <- glmer.nb(
  numEvents ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240_count
)


## Errors for graph --------------------------------------------------------

numEvents_nb.GLMM_errors <- numEvents_nb.GLMM %>%
  getErrorDF_LMM("earlyLifeTrt", panel = "adultTrt", errorType = "model") %>%
  combineStress() %>%
  mutate(
    y = y / 240.02
    , SE = SE / 240.02
    , error = error / 240.02
    , lower = lower / 240.02
    , upper = upper / 240.02
  )


# GABA - PSC amplitude ----------------------------------------------------

relAmplitude_lmm <- mixed(
  relPeak ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240FilteredFiring
  , method = "KR"
)


# Errors for graph --------------------------------------------------------

relAmplitude_lmm_errors <- relAmplitude_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()
