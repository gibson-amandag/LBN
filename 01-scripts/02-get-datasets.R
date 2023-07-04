# LOAD EXCEL SHEETS -------------------------------------------------------

Breeding <- loadExcelSheet(dataFolder, LBN_DataName, "Breeding")
Litters <- loadExcelSheet(dataFolder, LBN_DataName, "Litters")
Dam_cort <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_cort")

# Load data sheets, and remove and extraneous "specific" mouseIDs (DamID-litterNum_earTag)
# Will add this info back in in R, but left in excel for historical ease of looking up data
Demo_off <- loadExcelSheet(dataFolder, LBN_DataName, "Demo_off")
Mass_litter_off <- loadExcelSheet(dataFolder, LBN_DataName, "Mass_litter_off")
Mass_postWean_off <- loadExcelSheet(dataFolder, LBN_DataName, "Mass_postWean_off") %>%
  select(-mouseID_spec)
Maturation_off <- loadExcelSheet(dataFolder, LBN_DataName, "Maturation_off")%>%
  select(-mouseID_spec)
EndPara_off <- loadExcelSheet(dataFolder, LBN_DataName, "EndParadigm_off")
Cycles_off <- loadExcelSheet(dataFolder, LBN_DataName, "Cycles_off")%>%
  select(-c(mouseID_spec, num_ID, cycleStartDate)) # num_ID and startDate calculated in R
Cycles_off_extra <- loadExcelSheet(dataFolder, LBN_DataName, "Cycles_off_extra")%>%
  select(-c(mouseID_spec, num_ID, cycleStartDate)) # num_ID and startDate calculated in R
CohortCyclingFolder <- loadExcelSheet(dataFolder, LBN_DataName, "CohortCyclingFolder")
Sacrifice_off <- loadExcelSheet(dataFolder, LBN_DataName, "Sacrifice_off")%>%
  select(-mouseID_spec)
Cort_off <- loadExcelSheet(dataFolder, LBN_DataName, "Cort_Dec2022")%>% # changed temporarily from Cort_off
  select(-mouseID_spec)
Cort_random <- loadExcelSheet(dataFolder, LBN_DataName, "Cort_random")
LH_code <- loadExcelSheet(dataFolder, LBN_DataName, "LH_code")%>%
  select(-mouseID_spec)
LH_random <- loadExcelSheet(dataFolder, LBN_DataName, "LH_random")
LH_off <- loadExcelSheet(dataFolder, LBN_DataName, "LH_off")
ChronicStress_off <- loadExcelSheet(dataFolder, LBN_DataName, "ChronicStress_off")
CRH_dam <- loadExcelSheet(dataFolder, LBN_DataName, "CRH_dam")
damBehavior <- loadExcelSheet(dataFolder, LBN_DataName, "damBehavior")
# behavior_ZT0 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT0")
# behavior_ZT14 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT14")
# behavior_ZT16 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_P5_ZT16")
# behavior_ZT19 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT19")
# behavior_ZT20 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_P5_ZT20")
# behavior_ZT9 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT9")
# behavior_ZT4 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_P6_ZT4")
niceNames <- loadExcelSheet(dataFolder, LBN_DataName, "plotLabels")


slicingInfo <- loadExcelSheet(dataFolder, LBN_DataName, "Slicing_off")
GABApscs <- loadExcelSheet(dataFolder, LBN_DataName, "GABAPSCs")
GABApscs_120 <- loadExcelSheet(dataFolder, LBN_DataName, "GABAPSCs_120")
GABApscs_240 <- loadExcelSheet(dataFolder, LBN_DataName, "GABAPSCs_240")
cellInfo <- loadExcelSheet(dataFolder, LBN_DataName, "cellInfo")
cellExclusion <- loadExcelSheet(dataFolder, LBN_DataName, "cellExclusion")

damFrames <- loadExcelSheet(dataFolder, LBN_DataName, "damFrames")

# FORMAT DATASETS ---------------------------------------------------------

#Make factor variables
# damID
# dam
# mouseID
# Paradigm
# Litter number
# cohort

Breeding <- Breeding %>%
  makeFactors(c(
    damID,
    dam,
    litterNum
  )) 

Litters <- Litters %>%
  makeFactors(c(
    damID,
    cohort,
    ParaType
  )) %>%
  orderEarlyLifeTrt()

Dam_cort <- Dam_cort %>%
  makeFactors(damID)

Demo_dam <- Breeding %>%
  left_join(
    Litters,
    by = "damID"
  ) %>%
  left_join(
    Dam_cort,
    by = "damID"
  )

Demo_off <- makeFactors(Demo_off, c(damID, mouseID, sex, mouseID_spec))

numMiceOffCages <- Demo_off %>%
  filter(
    !is.na(weanCageNumber)
  ) %>%
  group_by(
    weanCageNumber
  ) %>%
  summarize(
    numInCage = n()
  )

Demo_off <- Demo_off %>%
  left_join(
    numMiceOffCages
    , by = "weanCageNumber"
  )

damFrames <- damFrames %>%
  makeFactors(damID)

# Add the numeric mouseID to the litter mass dataframe
Mass_litter_off <- Mass_litter_off %>%
  makeFactors(mouseID_spec) %>%
  left_join(
    Demo_off %>% select(mouseID, mouseID_spec),
    by = "mouseID_spec"
  )

Mass_postWean_off <- makeFactors(Mass_postWean_off, mouseID)

#Combine pre-wean and post-wean mass dataframes
Mass_off <- Mass_litter_off %>%
  full_join(
    Mass_postWean_off,
    by = "mouseID"
  ) %>%
  relocate(
    mouseID,
    .after = "mouseID_spec"
  )

Mass_off <- makeFactors(Mass_off, mouseID)
Maturation_off <- makeFactors(Maturation_off, mouseID)
EndPara_off <- makeFactors(EndPara_off, mouseID)
Cycles_off <- makeFactors(Cycles_off, mouseID)
Cycles_off_extra <- makeFactors(Cycles_off_extra, mouseID)
CohortCyclingFolder <- makeFactors(CohortCyclingFolder, cohort)
Sacrifice_off <- Sacrifice_off %>%
  orderAdultTrt() %>%
  makeFactors(c(mouseID, adultTrt)) %>%
  calcOrganMassByBodyMass(ReproTract_mass) %>%
  calcOrganMassByBodyMass_AM(ReproTract_mass) %>%
  calcOrganMassByBodyMass(Gonad_mass) %>%
  calcOrganMassByBodyMass_AM(Gonad_mass) %>%
  calcOrganMassByBodyMass(Adrenal_mass) %>%
  calcOrganMassByBodyMass_AM(Adrenal_mass) %>%
  mutate(
    bodyMass_diff = Body_mass_sac - Body_mass_AM
  )
ChronicStress_off <- makeFactors(ChronicStress_off, mouseID)
CRH_dam <- makeFactors(CRH_dam, c(damID,dam))
damBehavior <- makeFactors(damBehavior, damID)
# behavior_ZT0 <- makeFactors(behavior_ZT0, damID)
# behavior_ZT14 <- makeFactors(behavior_ZT14, damID)
# behavior_ZT16 <- makeFactors(behavior_ZT16, damID)
# behavior_ZT19 <- makeFactors(behavior_ZT19, damID)
# behavior_ZT20 <- makeFactors(behavior_ZT20, damID)
# behavior_ZT9 <- makeFactors(behavior_ZT9, damID)
# behavior_ZT4 <- makeFactors(behavior_ZT4, damID)
Cort_off <- makeFactors(Cort_off, mouseID)
LH_code <- makeFactors(LH_code, c(sampleID, mouseID))
LH_off <- makeFactors(LH_off, c(sampleID))

slicingInfo <- makeFactors(slicingInfo, c(mouseID))
cellInfo <- makeFactors(cellInfo, c(mouseID, cellID))
cellExclusion <- makeFactors(cellExclusion, c(cellID))
GABApscs <- makeFactors(GABApscs, c(cellID))
GABApscs_120 <- makeFactors(GABApscs_120, c(cellID))
GABApscs_240 <- makeFactors(GABApscs_240, c(cellID))

# behaviorDFs <- list(
#   behavior_ZT9
#   , behavior_ZT14
#   , behavior_ZT16
#   , behavior_ZT19
#   , behavior_ZT20
#   , behavior_ZT0
#   , behavior_ZT4
# )

CohortCyclingFolder <- CohortCyclingFolder %>%
  mutate(
    cyclingFolderPath = ifelse(
      is.na(CyclesFolder), 
      NA,
      file.path(LBN_ServerFolder, paste0("LBN_", sprintf("%04d", cohort)), CyclesFolder)
    )
  )
## Format Dam Demo ------------------------------------------------------
Demo_dam <- Demo_dam %>%
  mutate(pupLoss = Litter_size_startPara - Litter_size_endPara) %>%
  convertStartPara() %>%
  left_join(CohortCyclingFolder, by = "cohort")

# DAM BEHAVIOR ------------------------------------------------------------

# Long-form - combined in one
# dam_behavior_noDemo <- bind_rows(behaviorDFs)
dam_behavior_noDemo <- damBehavior

# Add dam demo info
dam_behavior <- dam_behavior_noDemo %>%
  left_join(
    Demo_dam,
    by = "damID"
  )

# Make wide behavior table
dam_behavior_wide <- dam_behavior_noDemo %>%
  pivot_wider(
    id_cols = damID,
    names_from = PND:time,
    values_from = Duration:Avg_dur_on_nest,
    names_glue = "P{PND}_ZT{time}_{.value}"#,
    # names_sep = "_"
  )

# Add behavior wide to Demo_dam

# 2023-01-20 - removed this; gets way too large. Pain to remove when trying to add dam information later
# may cause some errors in old combo code, though
# Demo_dam <- Demo_dam %>%
#   left_join(dam_behavior_wide, by = "damID")

damFrames_wide <- damFrames %>%
  select(
    -lightDark
  ) %>%
  pivot_wider(
    id_cols = damID,
    names_from = PND:minute,
    values_from = damOnNest:clump8,
    names_glue = "P{PND}_ZT{ZT}_min{minute}_{.value}"
  )

# Add dam demographic info to damFrames
damFrames <- damFrames %>%
  left_join(Demo_dam, by = "damID") %>%
  mutate(
    across(starts_with("clump"), ~ .x / Litter_size * 100, .names = "{.col}_percLitter")
    , .after = clump8
  )

damFrames_wide <- damFrames_wide %>%
  left_join(
    Demo_dam, by = "damID"
  )

# 2023-01-12, didn't add dam frames data to general dam demo frame

# CORT AND LH -------------------------------------------------------------
Cort_off <- Cort_off %>%
  filter(
    is.na(removeDup) | !removeDup
  )%>%
  mutate(
    exclude = ifelse(is.na(exclude), FALSE, TRUE)
  )

Cort_off_wide <- Cort_off %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = cort,
    names_prefix = "cort_hr"
  )

Cort_exclude_wide <- Cort_off %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = exclude,
    names_prefix = "exclude_cort_hr"
  )

Cort_off_wide <- Cort_off_wide %>%
  left_join(
    Cort_exclude_wide,
    by = "mouseID"
  ) %>%
  left_join(
    Cort_off %>%
      filter(
        time == 0 ## assumes on same plate
      ) %>%
      select(
        mouseID,
        plateQC
      )
  )

# Add cort data to Sacrifice_off to make AcuteStress_off
AcuteStress_off <- Sacrifice_off %>%
  left_join(
    Cort_off_wide,
    by = "mouseID"
  ) %>%
  mutate(
    exclude_cort_hr0 = ifelse(!is.na(exclude_cort_hr0), exclude_cort_hr0, FALSE),
    exclude_cort_hr5 = ifelse(!is.na(exclude_cort_hr5), exclude_cort_hr5, FALSE)
  )

# Add stress day demo to long cort df
Cort_off <- Cort_off %>%
  left_join(
    Sacrifice_off,
    by = "mouseID"
  )

# Add mouse and time info to LH values
LH_off <- LH_off %>%
  left_join(
    LH_code,
    by = "sampleID"
  )

# Get max LH value after baseline for each mouse
LH_max <- LH_off %>%
  filter(time !=0) %>% # missing initially -> max could have been AM
  getMaxFromRepMeasures_plusOtherVal(
    col = LH,
    maxColName = maxLH,
    groupingVar = mouseID,
    valCol = time,
    valAtMaxColName = timeAtMax
  )

LH_AUC <- LH_off %>%
  group_by(
    mouseID
  ) %>%
  summarize(
    LH_AUC = AUC(time, LH)
  )

LH_off <- LH_off %>%
  left_join(
    LH_max,
    by = "mouseID"
  ) %>%
  left_join(
    LH_AUC,
    by = "mouseID"
  )

# Make wide version of LH
LH_off_wide <- LH_off %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = LH,
    names_prefix = "LH_hr",
  ) %>%
  left_join( # Add max column
    LH_max,
    by = "mouseID"
  ) %>%
  left_join(
    LH_AUC,
    by = "mouseID"
  )

# Add LH data to AcuteStress_off
AcuteStress_off <- AcuteStress_off %>%
  left_join(
    LH_off_wide,
    by = "mouseID"
  )

# Add stress day demo to long cort df
LH_off <- LH_off %>%
  left_join(
    Sacrifice_off,
    by = "mouseID"
  )


# GABA PSCs ---------------------------------------------------------------

GABApscs <- GABApscs %>%
  select(-group) %>%
  left_join(
    cellExclusion %>%
      select(
        cellID, incCell, exclude
      )
    , by = "cellID"
  ) %>%
  left_join(
    cellInfo,
    by = "cellID"
  ) %>%
  left_join(
    slicingInfo,
    by = "mouseID"
  ) %>%
  mutate(
    timeHr = ifelse(!is.na(time), time * 24, NA),
    .after = time
  ) %>%
  mutate(
    recHr = ifelse(Daylight_Savings == "Y", timeHr - 4, timeHr - 3), # hours since lights on
    timeSinceSac = recHr - Sac_hr,
    .after = timeHr
  ) %>%
  select(
    -time, timeHr
  )

GABApscs_120 <- GABApscs_120 %>%
  select(-group) %>%
  left_join(
    cellExclusion %>%
      select(
        cellID, incCell, exclude
      )
    , by = "cellID"
  ) %>%
  left_join(
    cellInfo,
    by = "cellID"
  ) %>%
  left_join(
    slicingInfo,
    by = "mouseID"
  ) %>%
  mutate(
    timeHr = ifelse(!is.na(time), time * 24, NA),
    .after = time
  ) %>%
  mutate(
    recHr = ifelse(Daylight_Savings == "Y", timeHr - 4, timeHr - 3), # hours since lights on
    timeSinceSac = recHr - Sac_hr,
    .after = timeHr
  ) %>%
  select(
    -time, timeHr
  )

GABApscs_240 <- GABApscs_240 %>%
  select(-group) %>%
  left_join(
    cellExclusion %>%
      select(
        cellID, incCell, exclude
      )
    , by = "cellID"
  ) %>%
  left_join(
    cellInfo,
    by = "cellID"
  ) %>%
  left_join(
    slicingInfo,
    by = "mouseID"
  ) %>%
  mutate(
    timeHr = ifelse(!is.na(time), time * 24, NA),
    .after = time
  ) %>%
  mutate(
    recHr = ifelse(Daylight_Savings == "Y", timeHr - 4, timeHr - 3), # hours since lights on
    timeSinceSac = recHr - Sac_hr,
    .after = timeHr
  ) %>%
  select(
    -time, timeHr
  )

# COMBINE ALL DFS INTO ONE ------------------------------------------------

LBN_all <- Demo_off %>%
  left_join(Demo_dam, by = "damID") %>%
  full_join(select(Mass_off, -ParaType), by = c("mouseID", "mouseID_spec")) %>%
  # full_join(Maturation_off, by = "mouseID") %>%
  full_join(EndPara_off, by = "mouseID") %>%
  full_join(Cycles_off, by = "mouseID") %>%
  full_join(Cycles_off_extra, by = "mouseID")%>%
  full_join(AcuteStress_off, by = "mouseID") %>%
  full_join(ChronicStress_off, by = "mouseID") %>%
  full_join(slicingInfo, by = "mouseID")

# DAM DEMO FOR OFFSPRING --------------------------------------------------

Demo_dam_for_offspring <- Demo_dam %>%
  select(
    damID,
    earlyLifeTrt,
    litterNum,
    DOB, 
    cohort,
    cyclingFolderPath,
    ParaType,
    pupLoss,
    Litter_size,
    Avg_litter_mass_startPara,
    Mass_P2,
    Mass_P4,
    dam,
    damCage, 
    damStrain,
    strain,
    sire, 
    Litter_size_startPara, 
    Litter_size_endPara,
    Pups_through_wean,
    Sac_or_stop
    # , P5_ZT12_Duration:P6_ZT4_Avg_dur_on_nest # removed 2023-01-20
  )

# Update offspring demographics
Demo_off <- addDamDemoData(
  Demo_off,
  Demo_dam_for_offspring
)


# COMBINE OFFSPRING DATA INTO ONE -----------------------------------------
LBN_data <- Demo_off %>%
  left_join(select(Mass_off, -ParaType), by = c("mouseID", "mouseID_spec")) %>%
  # left_join(Maturation_off, by = "mouseID") %>%
  left_join(EndPara_off, by = "mouseID") %>%
  left_join(Cycles_off, by = "mouseID") %>%
  left_join(Cycles_off_extra, by = "mouseID") %>%
  left_join(AcuteStress_off, by = "mouseID") %>%
  left_join(ChronicStress_off, by = "mouseID") 


# ADD OFFSPRING DEMO DATA -------------------------------------------------
Mass_off <- Mass_off %>%
  select(-ParaType) %>%
  addOffspringDemoData(addBy = c("mouseID", "mouseID_spec")) %>%
  relocate(
    Avg_litter_mass_startPara,
    .before = "Mass_P9"
  )

Maturation_off <- Maturation_off %>%
  addOffspringDemoData() %>%
  left_join(Mass_off %>% select(
    mouseID,
    Mass_P22,
    Mass_P23,
    Mass_P24,
    Mass_P70,
    Mass_P71,
    Mass_P72
    ),
    by = "mouseID"
  ) %>%
  setUpMaturation() 

EndPara_off <- EndPara_off %>%
  addOffspringDemoData()

Cycles_off <- Cycles_off %>%
  addOffspringDemoData() %>%
  countEstrousStageDays() %>%
  mutate(
    cycleStartDate = DOB + 70
  )

Cycles_off_all <- Cycles_off %>%
  left_join(Cycles_off_extra, by = "mouseID")

AcuteStress_off <- AcuteStress_off %>%
  addOffspringDemoData() %>%
  combineStress() %>% # combined stress label
  calcAgeInDays()

GABApscs <- GABApscs %>%
  left_join(
    AcuteStress_off,
    by = "mouseID"
  )
GABApscs_120 <- GABApscs_120 %>%
  left_join(
    AcuteStress_off,
    by = "mouseID"
  )
GABApscs_240 <- GABApscs_240 %>%
  left_join(
    AcuteStress_off,
    by = "mouseID"
  )

ChronicStress_off <- ChronicStress_off %>%
  addOffspringDemoData()

Cort_off <- Cort_off %>%
  addOffspringDemoData() %>%
  combineStress()

LH_off <- LH_off %>%
  addOffspringDemoData() %>%
  combineStress()

LH_code <- LH_code %>%
  left_join(
    Sacrifice_off,
    by = "mouseID"
  )%>%
  addOffspringDemoData() %>%
  combineStress()

# UPDATE COMBO FRAMES WITH MATURATION -------------------------------------
LBN_all <- LBN_all %>%
  left_join(
    Maturation_off %>% 
      select(
        mouseID,
        # AGD_wean:AGD_P72
        VO_day:AGD_P72
      ), 
    by = "mouseID"
  )

LBN_data <- LBN_data %>%
  left_join(
    Maturation_off %>%
      select(
        mouseID,
        # AGD_wean:AGD_P72
        VO_day:AGD_P72
      ), 
    by = "mouseID"
  )

# something's causing a problem here. Didn't change anything. Don't get it. 2022-03-07

# Filter out extra second litter males -------
AcuteStress_males_2ndLitter <-  AcuteStress_off %>%
  # mutate(
  #   exclude_cort_hr0 = as_logical(exclude_cort_hr0),
  #   exclude_cort_hr5 = as_logical(exclude_cort_hr5)
  # )%>%
  filter(
    litterNum == 2,
    sex == "M",
    !(is.na(cort_hr0) | is.na(cort_hr5)),
    !(exclude_cort_hr0 | exclude_cort_hr5)
  ) %>%
  arrange(
    num_ID
  )

#Randomize the LBN-CON mice
AcuteStress_males_2ndLitter_LBN_CON <- AcuteStress_males_2ndLitter %>%
  filter(comboTrt == "LBN-CON")
LBN_CON_rows <- sample(nrow(AcuteStress_males_2ndLitter_LBN_CON))
#Keep the first 7
LBN_CON_rows_keep <- LBN_CON_rows[1:7]

#Get the mice that are being kept
AcuteStress_males_2ndLitter_LBN_CON_keep <- AcuteStress_males_2ndLitter_LBN_CON[LBN_CON_rows_keep, ] %>%
  select(
    mouseID
  )

#Randomize the LBN-ALPS mice
set.seed(42)
AcuteStress_males_2ndLitter_LBN_ALPS <- AcuteStress_males_2ndLitter %>%
  filter(comboTrt == "LBN-ALPS")
LBN_ALPS_rows <- sample(nrow(AcuteStress_males_2ndLitter_LBN_ALPS))
#Keep the first 7
LBN_ALPS_rows_keep <- LBN_ALPS_rows[1:7]
LBN_ALPS_rows_keep

#Get the mice that are being kept
AcuteStress_males_2ndLitter_LBN_ALPS_keep <- AcuteStress_males_2ndLitter_LBN_ALPS[LBN_ALPS_rows_keep, ] %>%
  select(
    mouseID
  )
AcuteStress_males_2ndLitter_LBN_ALPS_keep

#Keep all STD males
AcuteStress_males_2ndLitter_STD <- AcuteStress_males_2ndLitter %>%
  filter(earlyLifeTrt == "STD")
STD_rows <- sample(nrow(AcuteStress_males_2ndLitter_STD))
AcuteStress_males_2ndLitter_STD_keep <- AcuteStress_males_2ndLitter_STD[STD_rows, ] %>%
  select(
    mouseID
  )

keepMales_mouseID <- bind_rows(
  AcuteStress_males_2ndLitter_STD_keep,
  AcuteStress_males_2ndLitter_LBN_CON_keep,
  AcuteStress_males_2ndLitter_LBN_ALPS_keep
)

AcuteStress_off <- AcuteStress_off %>%
  mutate(
    includeMaleCort = ifelse(
      sex == "F",
      NA,
      ifelse(
        litterNum == 1,
        TRUE,
        ifelse(
          mouseID %in% keepMales_mouseID$mouseID,
          TRUE,
          FALSE
        )
      )
    )
  )%>%
  relocate(
    includeMaleCort,
    .before = "cort_hr0"
  )

Cort_off <- Cort_off %>%
  mutate(
    includeMaleCort = ifelse(
      sex == "F", 
      NA, 
      ifelse(
        litterNum == 1,
        TRUE,
        ifelse(
          mouseID %in% keepMales_mouseID$mouseID, 
          TRUE, 
          FALSE
        )
      )
    )
  )%>%
  relocate(
    includeMaleCort,
    .before = "cort"
  )
