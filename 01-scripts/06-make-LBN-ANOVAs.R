
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
anovaTextSize <- 14

doDamCortANOVA <- anovaTrtLitterFunc(
  expr(Cort_dam_P11),
  idVar = damID, 
  fontSize = anovaTextSize
)

damCortANOVAs <- damFiltered %>%
  doDamCortANOVA()

damCortANOVA <- damCortANOVAs$flxTbl
damCortANOVA_df <- damCortANOVAs$anova
damCortANOVA_text <- getTrtLitterFResults(damCortANOVA_df, sepText = "; ")
damCortANOVA_par <- damCortANOVA_text$paragraph

# Dam mass ----------------------------------------------------------------
anovaTextSize <- 14

doDamMassANOVA <- anovaDayTrtLitterFunc(
  expr(mass),
  idVar = damID, 
  fontSize = anovaTextSize
)

damMassANOVAs <- damFiltered %>%
  makeDamMassLong() %>%
  doDamMassANOVA()

damMassANOVA <- damMassANOVAs$flxTbl
damMassANOVA_df <- damMassANOVAs$anova
damMassANOVA_text <- getTrtLitterFResults(damMassANOVA_df, sepText = "; ")
damMassANOVA_par <- damMassANOVA_text$paragraph

# Maturation --------------------------------------------------------------

doVO_ageANOVA <- anovaTrtLitterFunc(
  expr(VO_age)
  , idVar = damID
  , fontSize = anovaTextSize
)
doVO_massANOVA <- anovaTrtLitterFunc(
  expr(VO_mass)
  , idVar = damID
  , fontSize = anovaTextSize
)
doEstrus_ageANOVA <- anovaTrtLitterFunc(
  expr(Estrus_age)
  , idVar = damID
  , fontSize = anovaTextSize
)
doEstrus_massANOVA <- anovaTrtLitterFunc(
  expr(Estrus_mass)
  , idVar = damID
  , fontSize = anovaTextSize
)
doPreputialSep_ageANOVA <- anovaTrtLitterFunc(
  expr(PreputialSep_age)
  , idVar = damID
  , fontSize = anovaTextSize
)
doPreputialSep_massANOVA <- anovaTrtLitterFunc(
  expr(PreputialSep_mass)
  , idVar = damID
  , fontSize = anovaTextSize
)


## VO_age ------------------------------------------------------------------


VO_age_ANOVAs <- maturation_byDam_f %>%
  doVO_ageANOVA()

VO_age_ANOVA <- VO_age_ANOVAs$flxTbl
VO_age_ANOVA_df <- VO_age_ANOVAs$anova
VO_age_ANOVA_text <- getTrtLitterFResults(VO_age_ANOVA_df)
VO_age_ANOVA_par <- VO_age_ANOVA_text$paragraph

## Estrus_age ------------------------------------------------------------------

Estrus_age_ANOVAs <- maturation_byDam_f %>%
  doEstrus_ageANOVA()

Estrus_age_ANOVA <- Estrus_age_ANOVAs$flxTbl
Estrus_age_ANOVA_df <- Estrus_age_ANOVAs$anova
Estrus_age_ANOVA_text <- getTrtLitterFResults(Estrus_age_ANOVA_df)
Estrus_age_ANOVA_par <- Estrus_age_ANOVA_text$paragraph

## Preputial_age ------------------------------------------------------------------

PreputialSep_age_ANOVAs <- maturation_byDam_m %>%
  doPreputialSep_ageANOVA()

PreputialSep_age_ANOVA <- PreputialSep_age_ANOVAs$flxTbl
PreputialSep_age_ANOVA_df <- PreputialSep_age_ANOVAs$anova
PreputialSep_age_ANOVA_text <- getTrtLitterFResults(PreputialSep_age_ANOVA_df)
PreputialSep_age_ANOVA_par <- PreputialSep_age_ANOVA_text$paragraph

## VO_mass ------------------------------------------------------------------
VO_mass_ANOVAs <- maturation_byDam_f %>%
  doVO_massANOVA()

VO_mass_ANOVA <- VO_mass_ANOVAs$flxTbl
VO_mass_ANOVA_df <- VO_mass_ANOVAs$anova
VO_mass_ANOVA_text <- getTrtLitterFResults(VO_mass_ANOVA_df)
VO_mass_ANOVA_par <- VO_mass_ANOVA_text$paragraph

## Estrus_mass ------------------------------------------------------------------

Estrus_mass_ANOVAs <- maturation_byDam_f %>%
  doEstrus_massANOVA()

Estrus_mass_ANOVA <- Estrus_mass_ANOVAs$flxTbl
Estrus_mass_ANOVA_df <- Estrus_mass_ANOVAs$anova
Estrus_mass_ANOVA_text <- getTrtLitterFResults(Estrus_mass_ANOVA_df)
Estrus_mass_ANOVA_par <- Estrus_mass_ANOVA_text$paragraph

## PreputialSep_mass ------------------------------------------------------------------

PreputialSep_mass_ANOVAs <- maturation_byDam_m %>%
  doPreputialSep_massANOVA()

PreputialSep_mass_ANOVA <- PreputialSep_mass_ANOVAs$flxTbl
PreputialSep_mass_ANOVA_df <- PreputialSep_mass_ANOVAs$anova
PreputialSep_mass_ANOVA_text <- getTrtLitterFResults(PreputialSep_mass_ANOVA_df)
PreputialSep_mass_ANOVA_par <- PreputialSep_mass_ANOVA_text$paragraph

# ALPS --------------------------------------------------------------------


## Cort --------------------------------------------------------------------


### Females -----------------------------------------------------------------
hValTable <- 0

cort4wayANOVAs <- cortFilteredFemales %>%
  cortAnova_returnBoth(
    byCycle = TRUE
    , fontSize = textSize
    , addHVal = hValTable
  )

cort4wayANOVA <- cort4wayANOVAs$flxTbl
cort4wayANOVA_df <- cort4wayANOVAs$anova

#'There is a significant 4-way interaction, so do the 3-way interaction of 
#'LBN x ALPS x Time for each cycle stage. Correct for two comparisons

cort3way_byCycle_df <- cortFilteredFemales %>%
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
  )

cort3way_byCycle <- cort3way_byCycle_df %>%
  # adjust_pvalue(method = "bonferroni") %>%
  formatAdjAnova(fontSize = textSize, addHVal = hValTable)

#' For diestrous mice, there is a 3-way interaction of LBN x ALPS x time
#' For proestrous mice, there is a 2-way interaction of ALPS x time
#' 
#' Follow-up the diestrous mice with a 2-way interaction between 
#' LBN x ALPS at each time. Adjust the p-values for two levels of time

diCort2way_byTime_df <- cortFilteredDi %>%
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
  )

diCort2way_byTime <- diCort2way_byTime_df %>%
  formatAdjAnova(fontSize = textSize, addHVal = hValTable)

#' At the end of the paradigm, there is an interaction between LBN and ALPS for
#' the diestrous mice. This means that LBN changes the way that the mice
#' respond to the ALPS paradigm in diestrous mice.



#' For the diestrous mice at the end of the paradigm, group by adult treatment
#' and see if the early-life mice are significantly different.
#' Adjust for making two treatment comparisons

diCortPost_byALPS_df <- cortFilteredDi %>%
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
  as_tibble()

diCortPost_byALPS <- diCortPost_byALPS_df %>%
  formatAdjAnova(fontSize = textSize, addHVal = hValTable)

#' For the proestrous mice, follow up the ALPS x time interaction.
#' Group by time, see if there is an effect of ALPS

proCort_byTime_df <- cortFilteredPro %>%
  group_by(
    time
  ) %>%
  anova_test(
    dv = cort,
    wid = mouseID,
    between = c(adultTrt)
  ) %>%
  adjust_pvalue(method = "bonferroni") %>%
  as_tibble()

proCort_byTime <- proCort_byTime_df %>%
  formatAdjAnova(fontSize = textSize, addHVal = hValTable)


### Males -------------------------------------------------------------------

maleCort_3wayANOVAs <- cortFilteredMales %>%
  cortAnova_returnBoth(fontSize = textSize, addHVal = hValTable)

maleCort_3wayANOVA <- maleCort_3wayANOVAs$flxTbl
maleCort_3wayANOVA_df <- maleCort_3wayANOVAs$anova
maleCort_3wayANOVA_text <- getMale3WayFResults(maleCort_3wayANOVA_df, sepText = "; ")
maleCort_3wayANOVA_par <- maleCort_3wayANOVA_text$paragraph


# GABA PSCs ---------------------------------------------------------------


tableWVal <- 0.02

doCapacitanceANOVA <- anovaComboTrtFunc(
  expr(capacitance)
  , fontSize = anovaTextSize
  , addWVal = tableWVal
)

doRseriesANOVA <- anovaComboTrtFunc(
  expr(Rseries)
  , fontSize = anovaTextSize
  , addWVal = tableWVal
)

doRinputANOVA <- anovaComboTrtFunc(
  expr(Rinput)
  , fontSize = anovaTextSize
  , addWVal = tableWVal
)

doHoldingCurrANOVA <- anovaComboTrtFunc(
  expr(holdingCurrent)
  , fontSize = anovaTextSize
  , addWVal = tableWVal
)

doFreqANOVA <- anovaComboTrtFunc(
  expr(frequency)
  , fontSize = anovaTextSize
  , addWVal = tableWVal
)

doAmpANOVA <- anovaComboTrtFunc(
  expr(relPeak)
  , fontSize = anovaTextSize
  , addWVal = tableWVal
)

capacitanceANOVAs <- GABApscsFilteredFiring %>%
  doCapacitanceANOVA()

capacitanceANOVA <- capacitanceANOVAs$flxTbl
capacitanceANOVA_df <- capacitanceANOVAs$anova
capacitanceANOVA_text <- getComboTrtFResults(capacitanceANOVA_df)
capacitanceANOVA_par <- capacitanceANOVA_text$paragraph

RseriesANOVAs <- GABApscsFilteredFiring %>%
  doRseriesANOVA()

RseriesANOVA <- RseriesANOVAs$flxTbl
RseriesANOVA_df <- RseriesANOVAs$anova
RseriesANOVA_text <- getComboTrtFResults(RseriesANOVA_df)
RseriesANOVA_par <- RseriesANOVA_text$paragraph

RinputANOVAs <- GABApscsFilteredFiring %>%
  doRinputANOVA()

RinputANOVA <- RinputANOVAs$flxTbl
RinputANOVA_df <- RinputANOVAs$anova
RinputANOVA_text <- getComboTrtFResults(RinputANOVA_df)
RinputANOVA_par <- RinputANOVA_text$paragraph

holdingCurrANOVAs <- GABApscsFilteredFiring %>%
  doHoldingCurrANOVA()

holdingCurrANOVA <- holdingCurrANOVAs$flxTbl
holdingCurrANOVA_df <- holdingCurrANOVAs$anova
holdingCurrANOVA_text <- getComboTrtFResults(holdingCurrANOVA_df)
holdingCurrANOVA_par <- holdingCurrANOVA_text$paragraph

GABAfreqANOVAs <- GABApscsFilteredFiring %>%
  doFreqANOVA()

GABAfreqANOVA <- GABAfreqANOVAs$flxTbl
GABAfreqANOVA_df <- GABAfreqANOVAs$anova
GABAfreqANOVA_text <- getComboTrtFResults(GABAfreqANOVA_df)
GABAfreqANOVA_par <- GABAfreqANOVA_text$paragraph

GABAampANOVAs <- GABApscsFilteredFiring %>%
  doAmpANOVA()

GABAampANOVA <- GABAampANOVAs$flxTbl
GABAampANOVA_df <- GABAampANOVAs$anova
GABAampANOVA_text <- getComboTrtFResults(GABAampANOVA_df)
GABAampANOVA_par <- GABAampANOVA_text$paragraph
