
# Dam behavior ------------------------------------------------------------

damBehaviorANOVA <- damBehaviorFiltered_P5_P6 %>%
  filter(
    time == 1 | time == 15
  ) %>%
  anova_test(
    dv = Num_exits,
    between = c(earlyLifeTrt, litterNum),
    wid = damID,
    within = time
  )


# Dam cort ----------------------------------------------------------------

damCortANOVA <- damFiltered %>%
  filter(
    !is.na(Cort_dam_P11)
  ) %>%
  anova_test(
    dv = Cort_dam_P11,
    between = c(litterNum, earlyLifeTrt)
  )

# Maturation --------------------------------------------------------------


VO_age_ANOVA <- maturation_byDam_f %>%
  anova_test(
    dv = VO_age,
    between = c(litterNum, earlyLifeTrt)
  )

Estrus_age_ANOVA <- maturation_byDam_f %>%
  anova_test(
    dv = Estrus_age,
    between = c(litterNum, earlyLifeTrt)
  )


PreputialSep_age_ANOVA <- maturation_byDam_m %>%
  anova_test(
    dv = PreputialSep_age,
    between = c(litterNum, earlyLifeTrt)
  )


VO_mass_ANOVA <- maturation_byDam_f %>%
  anova_test(
    dv = VO_mass,
    between = c(litterNum, earlyLifeTrt)
  )


Estrus_mass_ANOVA <- maturation_byDam_f %>%
  anova_test(
    dv = Estrus_mass,
    between = c(litterNum, earlyLifeTrt)
  )


PreputialSep_mass_ANOVA <- maturation_byDam_m %>%
  anova_test(
    dv = PreputialSep_mass,
    between = c(litterNum, earlyLifeTrt)
  )


# ALPS --------------------------------------------------------------------


## Cort --------------------------------------------------------------------


### Females -----------------------------------------------------------------

cort4wayANOVA <- cortFilteredFemales %>%
  cortAnova(
    byCycle = TRUE
    , fontSize = textSize
  )

#'There is a significant 4-way interaction, so do the 3-way interaction of 
#'LBN x ALPS x Time for each cycle stage. Correct for two comparisons

cort3way_byCycle <- cortFilteredFemales %>%
  group_by(
    Sac_cycle
  ) %>%
  anova_test(
    dv = cort,
    wid = mouseID,
    between = c(earlyLifeTrt, adultTrt),
    within = time
  ) %>%
  get_anova_table() %>%
  as_tibble() %>%
  mutate(
    p.adj = p * 2 # doing two cycle stage comparisons
  ) %>%
  # adjust_pvalue(method = "bonferroni") %>%
  formatAdjAnova(fontSize = textSize)

#' For diestrous mice, there is a 3-way interaction of LBN x ALPS x time
#' For proestrous mice, there is a 2-way interaction of ALPS x time
#' 
#' Follow-up the diestrous mice with a 2-way interaction between 
#' LBN x ALPS at each time. Adjust the p-values for two levels of time

diCort2way_byTime <- cortFilteredDi %>%
  group_by(
    time
  ) %>%
  anova_test(
    dv = cort,
    wid = mouseID,
    between = c(earlyLifeTrt, adultTrt)
  ) %>%
  get_anova_table() %>%
  as_tibble() %>%
  mutate(
    p.adj = p * 2 # doing two time comparisons
  ) %>%
  formatAdjAnova(fontSize = textSize)

#' At the end of the paradigm, there is an interaction between LBN and ALPS for
#' the diestrous mice. This means that LBN changes the way that the mice
#' respond to the ALPS paradigm in diestrous mice.



#' For the diestrous mice at the end of the paradigm, group by adult treatment
#' and see if the early-life mice are significantly different.
#' Adjust for making two treatment comparisons

diCortPost_byALPS <- cortFilteredDi %>%
  filter(
    time == 5
  ) %>%
  group_by(
    adultTrt
  ) %>%
  anova_test(
    dv = cort,
    wid = mouseID,
    between = c(earlyLifeTrt)
  ) %>%
  adjust_pvalue(method = "bonferroni") %>%
  formatAdjAnova(fontSize = textSize)

#' For the proestrous mice, follow up the ALPS x time interaction.
#' Group by time, see if there is an effect of ALPS

proCort_byTime <- cortFilteredPro %>%
  group_by(
    time
  ) %>%
  anova_test(
    dv = cort,
    wid = mouseID,
    between = c(adultTrt)
  ) %>%
  adjust_pvalue(method = "bonferroni") %>%
  formatAdjAnova(fontSize = textSize)


### Males -------------------------------------------------------------------

maleCort_3wayANOVA <- cortFilteredMales %>%
  cortAnova(fontSize = textSize)


# GABA PSCs ---------------------------------------------------------------

doCapacitanceANOVA <- anovaComboTrtFunc(
  expr(capacitance)
  , fontSize = textSize
)

doRseriesANOVA <- anovaComboTrtFunc(
  expr(Rseries)
  , fontSize = textSize
)

doRinputANOVA <- anovaComboTrtFunc(
  expr(Rinput)
  , fontSize = textSize
)

doHoldingCurrANOVA <- anovaComboTrtFunc(
  expr(holdingCurrent)
  , fontSize = textSize
)

doFreqANOVA <- anovaComboTrtFunc(
  expr(frequency)
  , fontSize = textSize
)

doAmpANOVA <- anovaComboTrtFunc(
  expr(relPeak)
  , fontSize = textSize
)

capacitanceANOVA <- GABApscsFilteredFiring %>%
  doCapacitanceANOVA()

RseriesANOVA <- GABApscsFilteredFiring %>%
  doRseriesANOVA()

RinputANOVA <- GABApscsFilteredFiring %>%
  doRinputANOVA()

holdingCurrANOVA <- GABApscsFilteredFiring %>%
  doHoldingCurrANOVA()

GABAfreqANOVA <- GABApscsFilteredFiring %>%
  doFreqANOVA()

GABAampANOVA <- GABApscsFilteredFiring %>%
  doAmpANOVA()
