addToHorzLines <- function(addVal, curVector = horzLines){
  lastVal <- curVector[length(curVector)]
  return(c(curVector, lastVal + addVal))
}

# Dam mass ----------------------

thisFigureLab <- "1B"
thisOutcomeLab <- "dam mass"

damMass_emmPairsTbl.earlyLifeTrt.CI <- damMass_lmm_EMM.earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

damMass_emmPairsTbl.PND.CI <- damMass_lmm_EMM.PND.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

pairsTable <- bind_rows(
  damMass_emmPairsTbl.earlyLifeTrt.CI
  , damMass_emmPairsTbl.PND.CI
)

horzLines <- c(1, 4)

# Male mass ----------------

thisFigureLab <- "2A"
thisOutcomeLab <- "male mass"

maleMass_emmPairsTbl.CI <- male_mass_lmm_emm.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo(noGroup = FALSE) %>%
  mutate(
    `group level` = paste("PND", day)
    , .before = contrast
  ) %>%
  select(-day)

pairsTable <- bind_rows(
  pairsTable
  , maleMass_emmPairsTbl.CI
)

horzLines <- addToHorzLines(5)

# AGD -------------

thisFigureLab <- "2D"
thisOutcomeLab <- "anogenital distance"

AGD_emmPairsTbl.CI <- AGD_lmm_emm_sex.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  mutate(
    contrast = ifelse(contrast == "F - M", "female - male", contrast)
  ) %>%
  addTableInfo()

pairsTable <- bind_rows(
  pairsTable
  , AGD_emmPairsTbl.CI
)

horzLines <- addToHorzLines(1)

# Male cort ----------------

thisFigureLab <- "4A"
thisOutcomeLab <- "male serum corticosterone"

maleCortLMM_emm.pairs_tbl.CI <- male_cort_lmm_ALPSTime_emm.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo(noGroup = FALSE) %>%
  mutate(
    contrast = ifelse(
      contrast == "time0 / time5"
      , "pre / post"
      , as.character(contrast)
    )
    , `group level` = case_when(
      time == 0 ~ "pre"
      , time == 5 ~ "post"
      , adultTrt == "CON" ~ "CON"
      , adultTrt == "ALPS" ~ "ALPS"
    )
    , .before = contrast
  ) %>%
  rename(
    estimate = ratio
  ) %>%
  select(
    - c(time, adultTrt, null)
  )

pairsTable <- bind_rows(
  pairsTable
  , maleCortLMM_emm.pairs_tbl.CI
)

horzLines <- addToHorzLines(2)
horzLines <- addToHorzLines(2)

# Female cort ------------------------

thisFigureLab <- "4B"
thisOutcomeLab <- "female serum corticosterone"

femaleCortLMM_emm_3way.pairs_tbl.CI <-  female_cort_lmm_emm_3way.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo(noGroup = FALSE) %>%
  mutate(
    contrast = ifelse(
      contrast == "time0 / time5"
      , "pre / post"
      , as.character(contrast)
    )
    , `group level` = case_when(
      time == 0 & Sac_cycle == "diestrus" ~ "di pre"
      , time == 5 & Sac_cycle == "diestrus" ~ "di post"
      , time == 0 & Sac_cycle == "proestrus" ~ "pro pre"
      , time == 5 & Sac_cycle == "proestrus" ~ "pro post"
      , Sac_cycle == "diestrus" & adultTrt == "CON" ~ "di CON"
      , Sac_cycle == "diestrus" & adultTrt == "ALPS" ~ "di ALPS"
      , Sac_cycle == "proestrus" & adultTrt == "CON" ~ "pro CON"
      , Sac_cycle == "proestrus" & adultTrt == "ALPS" ~ "pro ALPS"
      , time == 0 & adultTrt == "CON" ~ "pre CON"
      , time == 0 & adultTrt == "ALPS" ~ "pre ALPS"
      , time == 5 & adultTrt == "CON" ~ "post CON"
      , time == 5 & adultTrt == "ALPS" ~ "post ALPS"
    )
    , .before = contrast
  ) %>%
  rename(
    estimate = ratio
  ) %>%
  select(
    - c(time, Sac_cycle, adultTrt, null)
  )

pairsTable <- bind_rows(
  pairsTable
  , femaleCortLMM_emm_3way.pairs_tbl.CI
)

horzLines <- addToHorzLines(2)
horzLines <- addToHorzLines(2)
horzLines <- addToHorzLines(2)
horzLines <- addToHorzLines(2)
horzLines <- addToHorzLines(2)
horzLines <- addToHorzLines(2)


# Male masses ------------------------

thisFigureLab <- "4-1A"
thisOutcomeLab <- "body mass"

maleBodyMassAM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI <- maleBodyMassAM_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

maleBodyMassAM_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI <- maleBodyMassAM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

horzLines <- addToHorzLines(2)

thisFigureLab <- "4-1B"
thisOutcomeLab <- "% change body mass"

malePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI <- malePercChangeBodyMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

horzLines <- addToHorzLines(1)

thisFigureLab <- "4-1D"
thisOutcomeLab <- "adrenal mass normalized to body mass"

maleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI <- maleRelAdrenalMassPM_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

horzLines <- addToHorzLines(1)

thisFigureLab <- "4-1E"
thisOutcomeLab <- "seminal vesicle mass"

maleSeminalVesicleMass_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI <- maleSeminalVesicleMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

horzLines <- addToHorzLines(1)

thisFigureLab <- "4-1F"
thisOutcomeLab <- "seminal vesicle mass normalized to body mass"

maleRelSeminalVesicleMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI <- maleRelSeminalVesicleMassPM_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

maleRelSeminalVesicleMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI <- maleRelSeminalVesicleMassPM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

horzLines <- addToHorzLines(2)

thisFigureLab <- "4-1G"
thisOutcomeLab <- "testes mass"

maleTestesMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI <- maleTestesMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

maleTestesMass_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI <- maleTestesMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

horzLines <- addToHorzLines(2)

thisFigureLab <- "4-1G"
thisOutcomeLab <- "testes mass normalized to body mass"

maleRelTestesMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI <- maleRelTestesMassPM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

horzLines <- addToHorzLines(1)

maleALPS_RelPM_EMM.pairs_tbl.CI <- bind_rows(
  list(
    a = maleBodyMassAM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI
    , b = maleBodyMassAM_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI
    , d = malePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI
    , g = maleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI
    , j = maleSeminalVesicleMass_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI
    , k = maleRelSeminalVesicleMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI
    , l = maleRelSeminalVesicleMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI
    , m = maleTestesMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI
    , n = maleTestesMass_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI
    , p = maleRelTestesMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI
  )
)

pairsTable <- bind_rows(
  pairsTable
  , maleALPS_RelPM_EMM.pairs_tbl.CI
)

# Female masses ---------------------------------------

thisFigureLab <- "4-2B"
thisOutcomeLab <- "% change body mass"
horzLines <- addToHorzLines(2)

femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl.CI <- femalePercChangeBodyMass_ALPS_lmm_emm_Sac_cycle.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI <- femalePercChangeBodyMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

thisFigureLab <- "4-2C"
thisOutcomeLab <- "adrenal mass"
horzLines <- addToHorzLines(1)

femaleAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI <- femaleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

thisFigureLab <- "4-2D"
thisOutcomeLab <- "adrenal mass normalized to body mass"
horzLines <- addToHorzLines(1)

femaleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI <- femaleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

thisFigureLab <- "4-2E"
thisOutcomeLab <- "uterine mass"
horzLines <- addToHorzLines(1)

femaleUterineMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl.CI <- femaleUterineMass_ALPS_lmm_emm_Sac_cycle.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

thisFigureLab <- "4-2F"
thisOutcomeLab <- "uterine mass normalized to body mass"
horzLines <- addToHorzLines(1)

femaleRelUterineMassPM_ALPS_LMM_EMM.pairs_Sac_cycle_tbl.CI <- femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycle.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo()

femaleALPS_RelPM_EMM.pairs_tbl.CI <- bind_rows(
  list(
    a = femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl.CI
    , b = femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI
    , c = femaleAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl.CI
    , c2 = femaleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl.CI
    , d = femaleUterineMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl.CI
    , f = femaleRelUterineMassPM_ALPS_LMM_EMM.pairs_Sac_cycle_tbl.CI
  )
)

pairsTable <- bind_rows(
  pairsTable
  , femaleALPS_RelPM_EMM.pairs_tbl.CI
)

# Male cort admin -----------------

thisFigureLab <- "4-3B"
thisOutcomeLab <- "male cort admin serum corticosterone"
maleCortAdmin_cortLMM_emm.pairs_tbl.CI <- maleCortAdmin_cort_lmm_emm.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo() %>%
  mutate(
    contrast = ifelse(
      contrast == "dosage0 / dosage2"
      , "0mg/kg / 2mg/kg"
      , as.character(contrast)
    )
    , `group level` = paste0(time, "hr")
    , .before = contrast
  ) %>%
  select(
    -c(time, null)
  ) %>%
  rename(
    estimate = ratio
  )


pairsTable <- bind_rows(
  pairsTable
  , maleCortAdmin_cortLMM_emm.pairs_tbl.CI
)

horzLines <- addToHorzLines(6)

# Male cort admin masses -------------

thisFigureLab <- "4-3C"
thisOutcomeLab <- "% change body mass"
horzLines <- addToHorzLines(1)

malePercChangeBodyMass_dosage_LMM_EMM.pairs_dosage_tbl.CI <- malePercChangeBodyMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo() %>%
  mutate(
    contrast = ifelse(
      contrast == "dosage0 - dosage2"
      , "0mg/kg - 2mg/kg"
      , as.character(contrast)
    )
  )

thisFigureLab <- "4-3H"
thisOutcomeLab <- "testes mass"
horzLines <- addToHorzLines(1)

maleTestesMass_dosage_LMM_EMM.pairs_dosage_tbl.CI <- maleTestesMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo() %>%
  mutate(
    contrast = ifelse(
      contrast == "dosage0 - dosage2"
      , "0mg/kg - 2mg/kg"
      , as.character(contrast)
    )
  )

thisFigureLab <- "4-3I"
thisOutcomeLab <- "testes mass normalized to body mass"
horzLines <- addToHorzLines(1)

maleRelTestesMassPM_dosage_LMM_EMM.pairs_dosage_tbl.CI <- maleRelTestesMassPM_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput_CI() %>%
  addTableInfo() %>%
  mutate(
    contrast = ifelse(
      contrast == "dosage0 - dosage2"
      , "0mg/kg - 2mg/kg"
      , as.character(contrast)
    )
  )

male_dosage_RelPM_EMM.pairs_tbl.CI <- bind_rows(
  list(
    c = malePercChangeBodyMass_dosage_LMM_EMM.pairs_dosage_tbl.CI
    , m = maleTestesMass_dosage_LMM_EMM.pairs_dosage_tbl.CI
    , q = maleRelTestesMassPM_dosage_LMM_EMM.pairs_dosage_tbl.CI
  )
)

pairsTable <- bind_rows(
  pairsTable
  , male_dosage_RelPM_EMM.pairs_tbl.CI
)


# Flextable ------------------------

pairsFlextable <- pairsTable %>%
  mutate(
    row = row_number()
    # , .before = fig
  ) %>%
  makeManuscriptFlexTable(
    horzLines = horzLines
    , vertLines = c(1, 2, 3, 4, 10)
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t")
    , round3Cols = c("SEM")
    , vertMergeCols = c("fig", "outcome")
  )

pairsFlextable

