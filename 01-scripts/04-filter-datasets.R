cohorts <- c(7, 9, 10)
minLitterSize <- 5
damBehaviorTimes <- c(1, 15, 19)
surgeMin <- 3
exclude9011 <- TRUE

# This is a function that creates another function - makes it clearer what
# is being done for the filtering
filterLBNCohorts <- filterLBN(cohorts, minLitterSize, exclude9011)


# Dam Demo ----------------------------------------------------------------

damFiltered <- Demo_dam %>%
  filterLBNCohorts() 


# Dam Behavior ------------------------------------------------------------

damBehaviorFiltered <- dam_behavior %>%
  filterLBNCohorts() %>%
  rename(
    ZT = time
  )

filterBehaviorTimes <- filterDamBehaviorTimeFunc(damBehaviorTimes)

damBehaviorFiltered_ZTs <- damBehaviorFiltered %>%
  filterBehaviorTimes()

damBehaviorFiltered_P5_P6 <- damBehaviorFiltered %>%
  filter(
    PND %in% c(5, 6)
    , ! (PND == 5 & ZT == 1)
    , ! (PND == 6 & ZT == 15)
    , ! (PND == 6 & ZT == 19)
  )

damBehavior_byDam <- damBehaviorFiltered_ZTs %>%
  group_by(
    damID
  ) %>%
  summarizeDamBehavior() %>%
  arrange(
    cohort
    , damID
  )

damBehavior_byPND <- damBehaviorFiltered_ZTs %>%
  group_by(
    damID,
    PND
  ) %>%
  summarizeDamBehavior() %>%
  arrange(
    cohort
    , damID
    , PND
  )


damBehavior_byZT <- damBehaviorFiltered_ZTs %>%
  group_by(
    damID,
    ZT
  ) %>%
  summarizeDamBehavior() %>%
  arrange(
    cohort
    , damID
    , ZT
  )

damBehavior_byPND_ZT <- damBehaviorFiltered_ZTs %>%
  group_by(
    damID,
    PND,
    ZT
  ) %>% 
  summarizeDamBehavior() %>%
  arrange(
    cohort
    , damID
    , PND
    , ZT
  )

damBehavior_byLightDark <- damBehaviorFiltered_ZTs %>%
  mutate(
    lightDark = ifelse(ZT < 14, "light", "dark")
  ) %>%
  group_by(
    damID,
    lightDark
  ) %>%
  summarizeDamBehavior() %>%
  arrange(
    cohort
    , damID
    , desc(lightDark)
  )

damBehavior_byPNDLightDark <- damBehaviorFiltered_ZTs %>%
  mutate(
    lightDark = ifelse(ZT < 14, "light", "dark")
  ) %>%
  group_by(
    damID,
    PND,
    lightDark
  ) %>%
  summarizeDamBehavior() %>%
  arrange(
    cohort
    , damID
    , PND
    , desc(lightDark)
  )


## Frames -----------------------------------------------------------------

damFramesFiltered <- damFrames %>%
  filterLBNCohorts() %>%
  filter(
    confirmed == 1
  )

damFrames_byDam <- damFramesFiltered %>%
  group_by(
    damID
  ) %>%
  summarizeDamFrames() %>%
  arrange(
    cohort
    , damID
  )

damFramesAndBehaviorByDam <- damBehavior_byDam %>%
  select(
    -(earlyLifeTrt:Sac_or_stop)
  ) %>%
  left_join(
    damFrames_byDam %>%
      select(
        -(earlyLifeTrt:Sac_or_stop)
      ),
    by = "damID"
  ) %>%
  left_join(
    Demo_dam %>%
      select(
        -c(CyclesFolder, cyclingFolderPath)
      ),
    by = "damID"
  )

damFrames_byPND <- damFramesFiltered %>%
  group_by(
    damID,
    PND
  ) %>%
  summarizeDamFrames() %>%
  arrange(
    cohort
    , damID
    , PND
  )

damFramesAndBehavior_ByPND <- damFrames_byPND %>%
  full_join(
    damBehavior_byPND %>%
      select(
        -(earlyLifeTrt:Sac_or_stop)
      ),
    by = c("damID", "PND")
  )

damFrames_byZT <- damFramesFiltered %>%
  group_by(
    damID,
    ZT
  ) %>%
  summarizeDamFrames() %>%
  arrange(
    cohort
    , damID
    , ZT
  )

damFramesAndBehavior_byZT <- damFrames_byZT %>%
  full_join(
    damBehavior_byZT %>%
      select(
        -(earlyLifeTrt:Sac_or_stop)
      )
    , by = c("damID", "ZT")
  ) %>%
  arrange(
    cohort
    , damID
    , ZT
  )

damFrames_byPND_ZT <- damFramesFiltered %>%
  group_by(
    damID,
    PND,
    ZT
  ) %>%
  summarizeDamFrames() %>%
  arrange(
    cohort
    , damID
    , PND
    , ZT
  )

damFramesAndBehavior_ByPND_ZT <- damFrames_byPND_ZT %>%
  full_join(
    damBehavior_byPND_ZT %>%
      select(
        -(earlyLifeTrt:Sac_or_stop)
      )
    , by = c("damID", "PND", "ZT")
  ) %>%
  arrange(
    cohort
    , damID
    , PND
    , ZT
  )

damFrames_byLightDark <- damFramesFiltered %>%
  group_by(
    damID,
    lightDark
  ) %>%
  summarizeDamFrames() %>%
  arrange(
    cohort
    , damID
    , desc(lightDark)
  )

damFramesAndBehavior_byLightDark <- damFrames_byLightDark %>%
  full_join(
    damBehavior_byLightDark %>%
      select(
        -(earlyLifeTrt:Sac_or_stop)
      )
    , by = c("damID", "lightDark")
  ) %>%
  arrange(
    cohort
    , damID
    , desc(lightDark)
  )

damFrames_byPNDLightDark <- damFramesFiltered %>%
  group_by(
    damID,
    PND,
    lightDark
  ) %>%
  summarizeDamFrames() %>%
  arrange(
    cohort
    , damID
    , desc(lightDark)
  )

damFramesAndBehavior_byPNDLightDark <- damFrames_byPNDLightDark %>%
  full_join(
    damBehavior_byPNDLightDark %>%
      select(
        -(earlyLifeTrt:Sac_or_stop)
      )
    , by = c("damID", "PND", "lightDark")
  ) %>%
  arrange(
    cohort
    , damID
    , desc(lightDark)
  )

## Dam Mass ---------------------------------------------------------------

damMassFiltered <- damFiltered %>%
  makeDamMassLong()
  

# Mass --------------------------------------------------------------------

massFiltered <- Mass_off %>%
  filterLBNCohorts()

mass_byDam <- massFiltered %>%
  getAvgByDam(bySex = TRUE)

mass_long <- massFiltered %>%
  makeOffMassLong()

mass_byDam_long <- mass_byDam %>%
  makeOffMassLong()


# Maturation --------------------------------------------------------------

maturationFiltered <- Maturation_off %>%
  filterLBNCohorts()

maturation_byDam_f <- maturationFiltered %>%
  filter(
    sex == "F"
  ) %>%
  getAvgByDam()

maturation_byDam_m <- maturationFiltered %>%
  filter(
    sex == "M"
  ) %>%
  getAvgByDam()


maturationMassLong <- maturationFiltered %>%
  getAvgByDam(bySex = FALSE) %>%
  select(
    damID
    , ends_with("_mass")
  ) %>%
  rename(
    `mass_vaginal opening` = VO_mass
    , `mass_first estrus` = Estrus_mass
    , `mass_preputial separation` = PreputialSep_mass
  ) %>%
  pivot_longer(
    cols = starts_with("mass_")
    , names_to = "matType"
    , names_prefix = "mass_"
    , values_to = "mass"
    , values_drop_na = TRUE
  ) %>%
  mutate(
    matType = factor(matType, levels = c("vaginal opening", "first estrus", "preputial separation"))
  )

maturationMassLong_indivMice <- maturationFiltered %>%
  select(
    mouseID
    , damID
    , ends_with("_mass")
  ) %>%
  rename(
    `mass_vaginal opening` = VO_mass
    , `mass_first estrus` = Estrus_mass
    , `mass_preputial separation` = PreputialSep_mass
  ) %>%
  pivot_longer(
    cols = starts_with("mass_")
    , names_to = "matType"
    , names_prefix = "mass_"
    , values_to = "mass"
    , values_drop_na = TRUE
  ) %>%
  mutate(
    matType = factor(matType, levels = c("vaginal opening", "first estrus", "preputial separation"))
  )

maturationAgeLong <- maturationFiltered %>%
  getAvgByDam(bySex = FALSE) %>%
  select(
    damID
    , ends_with("_age")
  ) %>%
  rename(
    `age_vaginal opening` = VO_age
    , `age_first estrus` = Estrus_age
    , `age_preputial separation` = PreputialSep_age
  ) %>%
  pivot_longer(
    cols = starts_with("age_")
    , names_to = "matType"
    , names_prefix = "age_"
    , values_to = "age"
    , values_drop_na = TRUE
  ) %>%
  mutate(
    matType = factor(matType, levels = c("vaginal opening", "first estrus", "preputial separation"))
  )

maturationAgeLong_indivMice <- maturationFiltered %>%
  select(
    mouseID
    , damID
    , ends_with("_age")
  ) %>%
  rename(
    `age_vaginal opening` = VO_age
    , `age_first estrus` = Estrus_age
    , `age_preputial separation` = PreputialSep_age
  ) %>%
  pivot_longer(
    cols = starts_with("age_")
    , names_to = "matType"
    , names_prefix = "age_"
    , values_to = "age"
    , values_drop_na = TRUE
  ) %>%
  mutate(
    matType = factor(matType, levels = c("vaginal opening", "first estrus", "preputial separation"))
  )

maturationByDamLong <- maturationAgeLong %>%
  left_join(
    maturationMassLong
    , by = c("damID", "matType")
  ) %>%
  addDamDemoData(damDemo_forOff = Demo_dam_for_offspring)

maturationByMouseLong <- maturationAgeLong_indivMice %>%
  left_join(
    maturationMassLong_indivMice
    , by = c("mouseID", "damID", "matType")
  ) %>%
  addDamDemoData(damDemo_forOff = Demo_dam_for_offspring)


# Cycles ------------------------------------------------------------------

cyclesFiltered <- Cycles_off %>%
  filterLBNCohorts() %>%
  filter(
    !is.na(Day21)
  ) %>%
  select(
    -numCycles
    , -Cycle_length
  )

cyclesAllFiltered <- Cycles_off_all %>%
  filterLBNCohorts()

cyclesLong <- cyclesFiltered %>%
  makeCyclesLong()

cyclesSum <- cyclesLong %>%
  getCyclesInfo()

cyclesFiltered <- cyclesFiltered %>%
  left_join(
    cyclesSum
    , by = "mouseID"
  )

cyclesLong <- cyclesLong %>%
  addCycleStartCol()

cyclesPercLong <- cyclesFiltered %>%
  makeCyclesPercLong()


# Acute Stress ------------------------------------------------------------


acuteStressFiltered <- AcuteStress_off %>%
  filterLBNCohorts() %>%
  mutate(
    Sac_cycle = factor(Sac_cycle, levels = c("diestrus", "proestrus"))
    # Sac_cycle = factor(Sac_cycle, levels = c("proestrus", "diestrus"))
  )

cortFiltered <- Cort_off %>%
  filterLBNCohorts() %>%
  mutate(
    Sac_cycle = factor(Sac_cycle, levels = c("diestrus", "proestrus"))
    # Sac_cycle = factor(Sac_cycle, levels =c("proestrus", "diestrus"))
  )

LHFiltered <- LH_off %>%
  filterLBNCohorts() %>%
  mutate(
    Sac_cycle = factor(Sac_cycle, levels = c("diestrus", "proestrus"))
    # Sac_cycle = factor(Sac_cycle, levels = c("proestrus", "diestrus"))
  )

# Filter acute stress -----------------------------------------------------
# proUterineMin = 125
# proUterineMin = 115 # changed 2024-01-22
proUterineMin = 125 # changed 2024-01-22
diUterineMax = 100
exclude653 = TRUE
exclude723 = FALSE # Re-ran on 2023-07-16
excludeSmallTesticularMass <- TRUE

filterAcuteStressAll <- filterAcuteStressFunc(
  incSex = c("M", "F")
  , stages = c("proestrus", "diestrus") #cycle stages
  , exclude653 = exclude653
  , exclude723 = exclude723
  , filterUterineMass = FALSE
  , excludeSmallTesticularMass = excludeSmallTesticularMass
)

filterAcuteStress_M_DiPro <- filterAcuteStressFunc(
  incSex = c("M", "F")
  , stages = c("proestrus", "diestrus") #cycle stages
  , filterUterineMass = TRUE
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
  , excludeSmallTesticularMass = excludeSmallTesticularMass
  , exclude723 = exclude723
)

filterAcuteStressMales <- filterAcuteStressFunc(
  "M"
  , "" #cycle stages
  , excludeSmallTesticularMass = excludeSmallTesticularMass
)

filterAcuteStressFemales_all <- filterAcuteStressFunc(
  "F"
  , c("proestrus", "diestrus")
  , filterUterineMass = FALSE
  , exclude653 = exclude653
  , exclude723 = exclude723
)

filterUterineMass <- TRUE

filterAcuteStressFemales <- filterAcuteStressFunc(
  "F"
  , c("proestrus", "diestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
  , exclude723 = exclude723
)


filterAcuteStressPro <- filterAcuteStressFunc(
  "F"
  , c("proestrus") #cycle stages
  , exclude653 = exclude653
  , exclude723 = exclude723
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
)

filterAcuteStressDi <- filterAcuteStressFunc(
  "F"
  , c("diestrus") #cycle stages
  , exclude653 = exclude653
  , exclude723 = exclude723
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
)

acuteStressFilteredAll <- acuteStressFiltered %>%
  filterAcuteStressAll()

acuteStressFiltered_M_DiPro <- acuteStressFiltered %>%
  filterAcuteStress_M_DiPro() %>%
  mutate(
    hormoneStatus = ifelse(
      sex == "M"
      , "male"
      , as.character(Sac_cycle)
    )
  ) %>% mutate(
    hormoneStatus = factor(hormoneStatus, c("male","diestrus", "proestrus"))
    # hormoneStatus = factor(hormoneStatus, c("male","proestrus", "diestrus"))
  )

acuteStressFilteredMales <- acuteStressFiltered %>%
  filterAcuteStressMales()

acuteStressFilteredFemales_all <- acuteStressFiltered %>%
  filterAcuteStressFemales_all()

acuteStressFilteredFemales <- acuteStressFiltered %>%
  filterAcuteStressFemales()

acuteStressFilteredPro <- acuteStressFiltered %>%
  filterAcuteStressPro()

acuteStressFilteredDi <- acuteStressFiltered %>%
  filterAcuteStressDi()


## Filter Cort -------------------------------------------------------------
adjMaxMin = TRUE
cortMax = 1000 # changed when corrected the standards for cohort 7+8 and BD plates
cortMin = 3.9 # changed 2024-03-03

filterCortAll <- filterCortFunc(
  c("M", "F")
  , c("proestrus", "diestrus") #cycle stages
  , exclude653 = exclude653
  , exclude723 = exclude723
  , filterUterineMass = FALSE
  , cortMax = cortMax # make everything higher than this 1000ng/mL
  , cortMin = cortMin # make everything lower than this 3.9 ng/mL
)

filterCort_M_DiPro <- filterCortFunc(
  c("M", "F")
  , c("proestrus", "diestrus") #cycle stages
  , exclude653 = exclude653
  , exclude723 = exclude723
  , filterUterineMass = TRUE
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , cortMax = cortMax # make everything higher than this 1000ng/mL
  , cortMin = cortMin # make everything lower than this 3.9 ng/mL
)

filterCortMales <- filterCortFunc(
  "M"
  , "" #cycle stages
  , cortMax = cortMax # make everything higher than this 1000ng/mL
  , cortMin = cortMin # make everything lower than this 3.9 ng/mL
)

filterCortFemales_all <- filterCortFunc(
  "F"
  , c("proestrus", "diestrus")
  , filterUterineMass = FALSE
  , exclude653 = exclude653
  , exclude723 = exclude723
  , cortMax = cortMax # make everything higher than this 1000ng/mL
  , cortMin = cortMin # make everything lower than this 3.9 ng/mL
)

filterUterineMass <- TRUE

filterCortFemales <- filterCortFunc(
  "F"
  , c("proestrus", "diestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
  , exclude723 = exclude723
  , cortMax = cortMax # make everything higher than this 1000ng/mL
  , cortMin = cortMin # make everything lower than this 3.9 ng/mL
)


filterCortPro <- filterCortFunc(
  "F"
  , c("proestrus") #cycle stages
  , exclude653 = exclude653
  , exclude723 = exclude723
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , cortMax = cortMax # make everything higher than this 1000ng/mL
  , cortMin = cortMin # make everything lower than this 3.9 ng/mL
)

filterCortDi <- filterCortFunc(
  "F"
  , c("diestrus") #cycle stages
  , exclude653 = exclude653
  , exclude723 = exclude723
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , cortMax = cortMax # make everything higher than this 1000ng/mL
  , cortMin = cortMin # make everything lower than this 3.9 ng/mL
)

cortFilteredAll <- cortFiltered %>%
  filterCortAll()

cortFiltered_M_DiPro <- cortFiltered %>%
  filterCort_M_DiPro() %>%
  mutate(
    hormoneStatus = ifelse(
      sex == "M"
      , "male"
      , Sac_cycle
    )
  ) %>%
  mutate(
    hormoneStatus = factor(hormoneStatus, c("male","diestrus", "proestrus"))
    # hormoneStatus = factor(hormoneStatus, c("male","proestrus", "diestrus"))
  )

cortFilteredMales <- cortFiltered %>%
  filterCortMales()

cortFilteredFemales_all <- cortFiltered %>%
  filterCortFemales_all()

cortFilteredFemales <- cortFiltered %>%
  filterCortFemales()

cortFilteredPro <- cortFiltered %>%
  filterCortPro()

cortFilteredDi <- cortFiltered %>%
  filterCortDi()


## Filter LH ---------------------------------------------------------------

filterLHFemales_all <- filterLHFunc(
  c("proestrus", "diestrus")
  , filterUterineMass = FALSE
  , exclude653 = exclude653
  , exclude723 = exclude723
)

filterLHFemales <- filterLHFunc(
  c("proestrus", "diestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
  , exclude723 = exclude723
)

filterLHPro <- filterLHFunc(
  c("proestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
  , exclude723 = exclude723
)

filterLHDi <- filterLHFunc(
  c("diestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
  , exclude723 = exclude723
)



LHFilteredFemales_all <- LHFiltered %>%
  filterLHFemales_all()

LHFilteredFemales <- LHFiltered %>%
  filterLHFemales()

LHFilteredPro <- LHFiltered %>%
  filterLHPro()

LHFilteredDi <- LHFiltered %>%
  filterLHDi()


# Left so things don't break, but adding in surged to the general acuteStressFilteredPro df
surgedDF <- acuteStressFilteredPro %>%
  filter(
    !is.na(maxLH)
  ) %>%
  mutate(
    surged = maxLH > surgeMin
  )

acuteStressFilteredPro <- acuteStressFilteredPro %>%
  mutate(
    surged = ifelse(
      is.na(maxLH)
      , NA
      , maxLH > surgeMin
    )
    , forEphys = ifelse(
      mouseID %in% slicingInfo$mouseID
      , TRUE
      , FALSE
    )
  )

acuteStressFilteredPro_ephys <- acuteStressFilteredPro %>%
  filter(
    forEphys == TRUE
  )

acuteStressFilteredPro_sampling <- acuteStressFilteredPro %>%
  filter(
    forEphys == FALSE
  )

acuteStressFilteredDi <- acuteStressFilteredDi %>%
  left_join(
    LHFilteredDi %>%
      filter(
        time %in% c(5, 7.5)
      ) %>%
      group_by(
        mouseID
      ) %>%
      summarize(
        avgLH = mean(LH, na.rm = TRUE)
        , .groups = "drop"
      )
    , by = "mouseID"
  )


LHFilteredPro <- LHFilteredPro %>%
  mutate(
    forEphys = ifelse(
      mouseID %in% slicingInfo$mouseID
      , TRUE
      , FALSE
    )
  )

LHFilteredPro_ephys <- LHFilteredPro %>%
  filter(
    forEphys == TRUE
    , time %in% c(0, 5, 5.5)
  )

LHFilteredPro_sampling <- LHFilteredPro %>%
  filter(
    forEphys == FALSE
    , time %in% c(0, 5, 5.5, 6.5, 7.5, 8.5, 9.5)
  )


# GABA PSCs ---------------------------------------------------------------

GABApscsFiltered <- GABApscs %>%
  filterLBNCohorts() %>%
  filter(
    !is.na(adultTrt)
  ) %>%
  filterAcuteStressPro() # added 2023-03-26
GABApscs_120Filtered <- GABApscs_120 %>%
  filterLBNCohorts() %>%
  filter(
    !is.na(adultTrt)
  ) %>%
  filterAcuteStressPro() # added 2023-03-26
GABApscs_240Filtered <- GABApscs_240 %>%
  filterLBNCohorts() %>%
  filter(
    !is.na(adultTrt)
  ) %>%
  filterAcuteStressPro() # added 2023-03-26


# Passive properties updated 2023-06-02
filterByRseries <- TRUE
RseriesMin <- 0
RseriesMax <- 20
filterByRinput <- TRUE
RinputMin <- 500
RinputMax <- 2000
filterByHoldingCurr <- FALSE
holdingCurrMin <- -50
holdingCurrMax <- 10
filterByCapacitance <- TRUE
capacitanceMin <- 8
capacitanceMax <- 30

filterByNumCells <- FALSE
maxCellNum <- 3

filterBySacHr <- TRUE
maxSacHr <- 6 # time since sacrifice
filterByRecHr <- FALSE
recHrMin <- 13 # time since lights on
recHrMax <- 20

removeNoToInclude <- TRUE
removeExclude <- TRUE

filterEphys <- filterEphysFunc(
  filterByRseries = filterByRseries
  , RseriesMin = RseriesMin
  , RseriesMax = RseriesMax
  , filterByRinput = filterByRinput
  , RinputMin = RinputMin
  , RinputMax = RinputMax
  , filterByHoldingCurr = filterByHoldingCurr
  , holdingCurrMin =holdingCurrMin
  , holdingCurrMax = holdingCurrMax
  , filterByCapacitance = filterByCapacitance
  , capacitanceMin = capacitanceMin
  , capacitanceMax = capacitanceMax
  , filterByNumCells = filterByNumCells
  , maxCellNum = maxCellNum
  , filterBySacHr = filterBySacHr
  , maxSacHr =  maxSacHr# time since sacrifice
  , filterByRecHr = filterByRecHr
  , recHrMin =  recHrMin# time since lights on
  , recHrMax = recHrMax
  , removeNoToInclude = removeNoToInclude
  , removeExclude = removeExclude
)

GABApscsFilteredProps <- GABApscsFiltered %>%
  filterEphys()
GABApscs_120FilteredProps <- GABApscs_120Filtered %>%
  filterEphys()
GABApscs_240FilteredProps <- GABApscs_240Filtered %>%
  filterEphys()

filterByFreq <- TRUE

filterByFrequency <- function(df, doFilterByFreq = filterByFreq){
  if(doFilterByFreq){
    df <- df %>%
      filter(
        !is.na(frequency)
      )
  }
  return(df)
}

filterByMinRecDuration <- TRUE
minRecDuration <- 230 # buffer because not exact

GABApscsFilteredFiring <- GABApscsFilteredProps %>%
  filterByFrequency() 
GABApscs_120FilteredFiring <- GABApscs_120FilteredProps %>%
  filterByFrequency()
GABApscs_240FilteredFiring <- GABApscs_240FilteredProps %>%
  filterByFrequency()

if(filterByMinRecDuration){
  GABApscsFilteredFiring <- GABApscsFilteredFiring %>%
    filter(
      duration >= minRecDuration
    )
  GABApscs_240FilteredFiring <- GABApscs_240FilteredFiring %>%
    filter(
      duration >= minRecDuration
    )
}


# Male cort admin ---------------------------------------------------------

latterCortAdmin <- maleCortAdmin %>%
  filter(
    Sac_date >= date_parse("2023-07-19")
  )

latterCortAdmin_cort <- maleCortAdmin_cort %>%
  filter(
    Sac_date >= date_parse("2023-07-19")
  )

maleCortAdmin_filtered <- maleCortAdmin %>%
  filter(
    Sac_date >= date_parse("2023-07-19")
    , is.na(earlyLifeTrt) | earlyLifeTrt == "STD"
    , is.na(excludeCortAdmin) | excludeCortAdmin == FALSE
  )

maleCortAdmin_cort_filtered <- maleCortAdmin_cort %>%
  filter(
    Sac_date >= date_parse("2023-07-19")
    , is.na(earlyLifeTrt) | earlyLifeTrt == "STD"
    , is.na(excludeCortAdmin) | excludeCortAdmin == FALSE
  )
