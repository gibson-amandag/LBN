set_sum_contrasts()
set_null_device("png")


# Dam behavior - exits ------------------------------------------------------
# 
# #Chose a negative binomial distritution because this is count data, but it is
# #overdispersed, meaning that the variation is more than the means (which
# #violates assumptions for a Poisson distribution). 
# 
# #This would suggest that there's an effect of both treatment and experimental
# #day on the number of exits that the dam makes from the nest. There's not a lot
# #of evidence here for an interaction between day and treatment, though it's
# #harder to tell with each of the individual effects reported.
# 
# numExits_nb.GLMM <- glmer.nb(
#   Num_exits ~ earlyLifeTrt * PND + (1|damID)
#   , data = damBehavior_byPND_ZT %>%
#     makeFactors(c(PND, ZT))
# )
# 
# # Closest I've gotten to an ANOVA-like analysis. Not sure if this is best
# numExits_nb.GLMM_jointTest <- joint_tests(numExits_nb.GLMM)
# 
# 
# ## Post-hoc ----------------------------------------------------------------
# 
# numExits_nb.GLMM.earlyLifeEMM <- emmeans(
#   numExits_nb.GLMM
#   , "earlyLifeTrt"
#   , type = "response"
# )
# 
# numExits_nb.GLMM.earlyLifeEMM.pairs <- pairs(numExits_nb.GLMM.earlyLifeEMM)
# 
# 
# 
# ## Errors for graph --------------------------------------------------------
# 
# numExits_nb.GLMM_errors <- numExits_nb.GLMM %>%
#   getErrorDF_LMM("PND", panel = "earlyLifeTrt")
# 
# numExits_nb.GLMM_errors.earlyLifeEMM <- numExits_nb.GLMM %>%
#   getErrorDF_LMM("earlyLifeTrt")
# 
# 
# 
# # Dam behavior - perc time off nest ---------------------------------------
# 
# 
# percOffNest_lmm <- mixed(
#   Perc_off_nest ~ earlyLifeTrt * PND + (1|damID)
#   , data = damBehavior_byPND_ZT %>%
#     makeFactors(c(PND, ZT))
#   , method = "KR"
# )
# 
# # No interactions or main effects -> no post-hocs
# 
# 
# ## Errors for graph --------------------------------------------------------
# 
# percOffNest_lmm_errors <- percOffNest_lmm %>%
#   getErrorDF_LMM("PND", panel = "earlyLifeTrt")
# 
# percOffNest_lmm_errors.earlyLifeEMM <- percOffNest_lmm %>%
#   getErrorDF_LMM("earlyLifeTrt")
# 
# 

# Dam behavior ------------------------------------------------------------
numExits_nparLD <- f1.ld.f1(
  y = damBehavior_byPND$Num_exits
  , time = damBehavior_byPND$PND
  , group = damBehavior_byPND$earlyLifeTrt
  , subject = damBehavior_byPND$damID
  , time.name = "PND"
  , group.name = "early-life trt"
)

percOffNest_nparLD <- f1.ld.f1(
  y = damBehavior_byPND$Perc_off_nest
  , time = damBehavior_byPND$PND
  , group = damBehavior_byPND$earlyLifeTrt
  , subject = damBehavior_byPND$damID
  , time.name = "PND"
  , group.name = "early-life trt"
)


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
  , var.equal = TRUE
)


# Offspring mass ----------------------------------------------------------

## Females -------------------
female_mass_lmm <- mixed(
  mass ~ earlyLifeTrt * lspline(day, c(21, 35)) + (1|damID) + (1|mouseID)
  , data =  mass_long %>%
    filter(
      day >= 11
      , sex == "F"
    )
  , method = "KR"
)

### Post-hoc -------------------
female_mass_lmm_emm <- emmeans(
  female_mass_lmm
  , ~ earlyLifeTrt * lspline(day, c(21, 35))
  , data = mass_long %>%
    filter(
      day >= 11
    )
  , at = list(day = c(11, 21, 35, 70))
  , pbkrtest.limit = 3500
)

female_mass_lmm_emm.pairs <- contrast(
  female_mass_lmm_emm
  , "pairwise"
  , simple = list("earlyLifeTrt")
  , combine = TRUE
  , adjust = "holm"
)

### Errors for graph ------------------
# Generate a data frame with the specific days and all combinations of other variables
newdata <- expand.grid(day = unique(mass_long$day), earlyLifeTrt = unique(mass_long$earlyLifeTrt))

newdata <- newdata %>% 
  filter(
    day >= 11
  )

PNDs <- unique(mass_long$day)
PNDs <- PNDs[PNDs != 4]

# Compute the emmeans for your model at these specific points
female_mass_lmm_means <- emmeans(
  female_mass_lmm,  ~ earlyLifeTrt * lspline(day, c(21, 35))
  , at = list(day = PNDs, earlyLifeTrt = unique(mass_long$earlyLifeTrt))
  , data = newdata
  , pbkrtest.limit = 4000
)

female_mass_lmm_means_df <- as_tibble(female_mass_lmm_means)

female_mass_lmm_errors <- female_mass_lmm_means_df %>%
  rename(
    y = emmean
  ) %>%
  mutate(
    sex = "F"
    , lower = y - SE
    , upper = y + SE
  )

## Males -----------------------

male_mass_lmm <- mixed(
  mass ~ earlyLifeTrt * lspline(day, c(21, 35)) + (1|damID) + (1|mouseID)
  , data =  mass_long %>%
    filter(
      day >= 11
      , sex == "M"
    )
  , method = "KR"
)


### Post-hoc ----------------------------

male_mass_lmm_emm <- emmeans(
  male_mass_lmm
  , ~ earlyLifeTrt * lspline(day, c(21, 35))
  , data = mass_long %>%
    filter(
      day >= 11
    )
  , at = list(day = c(11, 21, 35, 56, 72))
)

male_mass_lmm_emm.pairs <- contrast(
  male_mass_lmm_emm
  , "pairwise"
  , simple = list("earlyLifeTrt")
  , combine = TRUE
  , adjust = "holm"
)

### Errors for graphs -------------------
male_mass_lmm_means <- emmeans(male_mass_lmm,  ~ earlyLifeTrt * lspline(day, c(21, 35)), at = list(day = PNDs, earlyLifeTrt = unique(mass_long$earlyLifeTrt)), data = newdata)

male_mass_lmm_means_df <- as_tibble(male_mass_lmm_means)

male_mass_lmm_errors <- male_mass_lmm_means_df %>%
  rename(
    y = emmean
  ) %>%
  mutate(
    sex = "M"
    , lower = y - SE
    , upper = y + SE
  )

# combined male and female mass errors
mass_lmm_errors <- female_mass_lmm_errors %>%
  rbind(
    male_mass_lmm_errors
  )

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

matMass_lmm_errors <- rbind(
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
  log10(cycleLength) ~ earlyLifeTrt + (1|damID)
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


# Male cort ---------------------------------------------------------------

male_cort_lmm <- mixed(
  log10(cort) ~ earlyLifeTrt * adultTrt * time + (1|mouseID) + (1|damID)
  , data = cortFilteredMales
  , method = "KR"
)

# adult x time

## post-hoc -------------

male_cort_lmm_ALPSTime_emm <- emmeans(
  male_cort_lmm
  , "adultTrt"
  , by = "time"
  , type = "response"
)

male_cort_lmm_ALPSTime_emm.pairs <- contrast(
  male_cort_lmm_ALPSTime_emm
  , "pairwise"
  # , simple = list("adultTrt")
  , simple = "each"
  , combine = TRUE
  , adjust = "holm"
)


## errors for graph ---------------
male_cort_lmm_emm <- emmeans(
  male_cort_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt", "time")
  , type = "response"
)

male_cort_lmm_error <- male_cort_lmm_emm %>%
  as_data_frame() %>%
  rename(
    y = response
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()

# male cort admin ------------

maleCortAdmin_cort_lmm <- mixed(
  log10(cort) ~ dosage * time + (1|mouseID) + (1|damID)
  , data = maleCortAdmin_cort %>%
    mutate(
      time = as.factor(time) # it's not actually linear
    )
  , method = "KR"
)

# dosage x time

## post-hoc ---------

maleCortAdmin_cort_lmm_emm <- emmeans(
  maleCortAdmin_cort_lmm
  , "dosage"
  , by = "time"
  , type = "response"
)

maleCortAdmin_cort_lmm_emm.pairs <- contrast(
  maleCortAdmin_cort_lmm_emm
  , "pairwise"
  , simple = list("dosage")
  , combine = TRUE
  , adjust = "holm"
)

## errors for graph ---------------
maleCortAdmin_cort_lmm_error <- maleCortAdmin_cort_lmm_emm %>%
  as_data_frame() %>%
  rename(
    y = response
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  )

# Corticosterone - only females ----------------------------------------------------------

female_cort_lmm <- mixed(
  log10(cort) ~ Sac_cycle * earlyLifeTrt * adultTrt * time + (1|mouseID) + (1|damID)
  , data = cortFilteredFemales
  , method = "KR"
)

# 2024-02-26
# Interaction between adultTrt, Sac_cycle, and time

## Post-hoc ---------------------------------------------------

female_cort_lmm_emm_3way <- emmeans(
  female_cort_lmm
  , ~ adultTrt * time * Sac_cycle
  , type = "response"
)

female_cort_lmm_emm_3way.pairs <- contrast(
  female_cort_lmm_emm_3way
  , "pairwise"
  , simple = "each"
  , combine = TRUE
  , adjust = "holm"
)

## Errors for graph --------------------------------------------------------

female_cort_lmm_emm <- emmeans(
  female_cort_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt", "Sac_cycle", "time")
  , type = "response"
  )

female_cort_lmm_error <- female_cort_lmm_emm %>%
  as_data_frame() %>%
  rename(
    y = response
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()

# Male masses -------------------------------------------------------------


# LH ----------------------------------------------------------------------

## Diestrus ---------------------------------------------------------------

LH_diAfternoon_lmm <- mixed(
  avgLH ~ earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFilteredDi
  , method = "KR"
)

### Errors for graph ----------------------

LH_diAfternoon_lmm_errors <- LH_diAfternoon_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()

## Pro ephys --------------------------------------------------------------

# 2024-03-10, because there is censoring of the data at 40ng/mL and not normally distributed, this is not the most appropriate test.
LH_proEphys_lmm <- mixed(
  maxLH ~ earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFilteredPro_ephys
  , method = "KR"
)

### Post-hoc ---------------

LH_proEphys_lmm_emm_earlyLifeTrt <- emmeans(
    LH_proEphys_lmm
    , "earlyLifeTrt"
  )

LH_proEphys_lmm_emm_earlyLifeTrt.pairs <- contrast(
  LH_proEphys_lmm_emm_earlyLifeTrt
  , "pairwise"
)

LH_proEphys_lmm_emm_adultTrt <- emmeans(
    LH_proEphys_lmm
    , "adultTrt"
  )

LH_proEphys_lmm_emm_adultTrt.pairs <- contrast(
  LH_proEphys_lmm_emm_adultTrt
  , "pairwise"
)

LH_proEphys_lmm_emm <- emmeans(
  LH_proEphys_lmm
  , "earlyLifeTrt"
  , by = "adultTrt"
)

LH_proEphys_lmm_emm.pairs <- contrast(
  LH_proEphys_lmm_emm
  , "pairwise"
  , simple = list("earlyLifeTrt")
  # , simple = "each" # if want to compare adult treat at levels of early-life, too
  , combine = TRUE
  , adjust = "holm"
)

emmip(LH_proEphys_lmm_emm, adultTrt ~ earlyLifeTrt)


### Errors for graph ------------------------

LH_proEphys_lmm_errors <- LH_proEphys_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()


## Pro sampling --------------------------------------------------------------

# 2024-03-10, because there is censoring of the data at 40ng/mL and not normally distributed, this is not the most appropriate test.
LH_proSampling_lmm <- mixed(
  maxLH ~ earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFilteredPro_sampling
  , method = "KR"
)

### Post-hoc ------------
LH_proSampling_lmm_emm_adultTrt <- emmeans(
  LH_proSampling_lmm
  , "adultTrt"
)

LH_proSampling_lmm_emm_adultTrt.pairs <- contrast(
  LH_proSampling_lmm_emm_adultTrt
  , "pairwise"
)

### Errors for graph ------------------------

LH_proSampling_lmm_errors <- LH_proSampling_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()


## LH peak time in full sampling ----------------

LH_proSampling_peakTime_lmm <- mixed(
  timeAtMax ~ earlyLifeTrt * adultTrt + (1|damID)
  , data = acuteStressFilteredPro_sampling
  , method = "KR"
)

## Chi-squared pro ---------

pro_ALPS_only <- acuteStressFilteredPro_sampling %>%
  filter(
    adultTrt == "ALPS"
  )

ALPS_contTable <- table(
  pro_ALPS_only$earlyLifeTrt
  , pro_ALPS_only$surged
)

propSurged.Chi.Sq.res <- chisq_test(ALPS_contTable) 

propSurged.Chi.Sq.descriptives <-  chisq_descriptives(propSurged.Chi.Sq.res)

## Binomial logistic regression -----------
propSurged_extendedSampling_glmm <- mixed(
  # surged ~ earlyLifeTrt * adultTrt + (1|damID)
  surged ~ earlyLifeTrt + adultTrt + (1|damID) # the interaction term leads to problems with the model fit, probably do not have enough data
  , data = acuteStressFilteredPro_sampling
  , family = binomial(link = "logit")
  , method = "LRT"
)

propSurged_extendedSampling_glmm_emm <- emmeans(
  propSurged_extendedSampling_glmm
  , "adultTrt"
  , type = "response"
)

propSurged_extendedSampling_glmm_emm.pairs <- contrast(
  propSurged_extendedSampling_glmm_emm
  , "pairwise"
)


# GABA - capacitance ------------------------------------------------------

capacitance_lmm <- mixed(
  capacitance ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240FilteredPropsFreq
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
  , data = GABApscs_240FilteredPropsFreq
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
  , data = GABApscs_240FilteredPropsFreq
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
  , data = GABApscs_240FilteredPropsFreq
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

GABApscs_240_count <- GABApscs_240FilteredPropsFreq %>% # this includes the cell that has no PSCs
  mutate(
    numEvents = frequency * duration
    , .after = frequency
  )

numEvents_nb.GLMM <- glmer.nb(
  numEvents ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240_count
)

numEvents_nb.GLMM_sum <- summary(numEvents_nb.GLMM)

# this gives the same results as below, so I think this is probably a Type III Wald Chi-Square test, too
numEvents_nb.GLMM_Anova <- joint_tests(
  emmeans(
    numEvents_nb.GLMM, ~earlyLifeTrt * adultTrt
  )
)

# Alternative:
# car::Anova(
#   # Type III Wald chi-square tests
#   numEvents_nb.GLMM
#   , type = "III"
# )


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

# GABA - PSC interval --------------------------

interval_lmm <- mixed(
  log10(interval) ~ earlyLifeTrt * adultTrt + (1|damID) + (1|mouseID)
  , data = GABApscs_240FilteredPropsFreq
  , method = "KR"
)

## errors for graph ------------
interval_lmm_emm <- emmeans(
  interval_lmm
  , "earlyLifeTrt"
  , by = c("adultTrt")
  , type = "response"
)

interval_lmm_error <- interval_lmm_emm %>%
  as_data_frame() %>%
  rename(
    y = response
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  ) %>%
  combineStress()

# GABA - PSC amplitude ----------------------------------------------------

relAmplitude_lmm <- mixed(
  amplitude ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240FilteredPropsFreq
  , method = "KR"
)


## Errors for graph --------------------------------------------------------

relAmplitude_lmm_errors <- relAmplitude_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()

# GABA - Decay time ----------------------------------------------------

decayTime_lmm <- mixed(
  decayTimes_8020 ~ earlyLifeTrt * adultTrt + (1|mouseID) + (1|damID)
  , data = GABApscs_240FilteredPropsFreq
  , method = "KR"
)


## Errors for graph --------------------------------------------------------

decayTime_lmm_errors <- decayTime_lmm %>%
  getErrorDF_LMM(
    xVar = "earlyLifeTrt"
    , panel = "adultTrt"
  ) %>%
  combineStress()