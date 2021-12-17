# LOAD EXCEL SHEETS -------------------------------------------------------

Breeding <- loadExcelSheet(dataFolder, LBN_DataName, "Breeding")
Litters <- loadExcelSheet(dataFolder, LBN_DataName, "Litters")
Dam_cort <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_cort")

Demo_off <- loadExcelSheet(dataFolder, LBN_DataName, "Demo_off")
Off_ID <- loadExcelSheet(dataFolder, LBN_DataName, "Off_ID")
Mass_litter_off <- loadExcelSheet(dataFolder, LBN_DataName, "Mass_litter_off")
Mass_postWean_off <- loadExcelSheet(dataFolder, LBN_DataName, "Mass_postWean_off")
Maturation_off <- loadExcelSheet(dataFolder, LBN_DataName, "Maturation_off")
EndPara_off <- loadExcelSheet(dataFolder, LBN_DataName, "EndParadigm_off")
Cycles_off <- loadExcelSheet(dataFolder, LBN_DataName, "Cycles_off")
Cycles_off_extra <- loadExcelSheet(dataFolder, LBN_DataName, "Cycles_off_extra")
CohortCyclingFolder <- loadExcelSheet(dataFolder, LBN_DataName, "CohortCyclingFolder")
Sacrifice_off <- loadExcelSheet(dataFolder, LBN_DataName, "Sacrifice_off")
Cort_off <- loadExcelSheet(dataFolder, LBN_DataName, "Cort_off")
Cort_random <- loadExcelSheet(dataFolder, LBN_DataName, "Cort_random")
LH_code <- loadExcelSheet(dataFolder, LBN_DataName, "LH_code")
LH_random <- loadExcelSheet(dataFolder, LBN_DataName, "LH_random")
LH_off <- loadExcelSheet(dataFolder, LBN_DataName, "LH_off")
ChronicStress_off <- loadExcelSheet(dataFolder, LBN_DataName, "ChronicStress_off")
CRH_dam <- loadExcelSheet(dataFolder, LBN_DataName, "CRH_dam")
behavior_ZT0 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT0")
behavior_ZT14 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT14")
behavior_ZT19 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT19")
niceNames <- loadExcelSheet(dataFolder, LBN_DataName, "plotLabels")


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

Demo_off <- makeFactors(Demo_off, c(damID, mouseID, sex))
Off_ID <- makeFactors(Off_ID, c(mouseID))

Demo_off <- Demo_off %>%
  left_join(Off_ID, by = "mouseID")

#Combine pre-wean and post-wean mass dataframes
Mass_off <- Mass_litter_off %>%
  full_join(
    Mass_postWean_off,
    by = "mouseID"
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
  calcOrganMassByBodyMass(Gonad_mass) %>%
  calcOrganMassByBodyMass(Adrenal_mass)
ChronicStress_off <- makeFactors(ChronicStress_off, mouseID)
CRH_dam <- makeFactors(CRH_dam, c(damID,dam))
behavior_ZT0 <- makeFactors(behavior_ZT0, damID)
behavior_ZT14 <- makeFactors(behavior_ZT14, damID)
Cort_off <- makeFactors(Cort_off, mouseID)
LH_code <- makeFactors(LH_code, c(sampleID, mouseID))
LH_off <- makeFactors(LH_off, c(sampleID))

behaviorDFs <- list(
  behavior_ZT0,
  behavior_ZT14,
  behavior_ZT19
)

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
dam_behavior_noDemo <- bind_rows(behaviorDFs)

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
    names_from = time,
    values_from = Duration:Avg_dur_on_nest,
    names_prefix = "ZT",
    names_sep = "_"
  )

# Add behavior wide to Demo_dam
Demo_dam <- Demo_dam %>%
  left_join(dam_behavior_wide, by = "damID")


# CORT AND LH -------------------------------------------------------------
Cort_off <- Cort_off %>%
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
  )

# Add cort data to Sacrifice_off to make AcuteStress_off
AcuteStress_off <- Sacrifice_off %>%
  left_join(
    Cort_off_wide,
    by = "mouseID"
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
  getMaxFromRepMeasures(
    col = LH,
    maxColName = maxLH,
    groupingVar = mouseID
  )

LH_off <- LH_off %>%
  left_join(
    LH_max,
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

# COMBINE ALL DFS INTO ONE ------------------------------------------------

LBN_all <- Demo_off %>%
  left_join(Demo_dam, by = "damID") %>%
  full_join(select(Mass_off, -ParaType), by = "mouseID") %>%
  # full_join(Maturation_off, by = "mouseID") %>%
  full_join(EndPara_off, by = "mouseID") %>%
  full_join(Cycles_off, by = "mouseID") %>%
  full_join(Cycles_off_extra, by = "mouseID")%>%
  full_join(AcuteStress_off, by = "mouseID") %>%
  full_join(ChronicStress_off, by = "mouseID")

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
    Duration_ZT0:Avg_dur_on_nest_ZT14
  )

# Update offspring demographics
Demo_off <- addDamDemoData(
  Demo_off,
  Demo_dam_for_offspring
)


# COMBINE OFFSPRING DATA INTO ONE -----------------------------------------
LBN_data <- Demo_off %>%
  left_join(select(Mass_off, -ParaType), by = "mouseID") %>%
  # left_join(Maturation_off, by = "mouseID") %>%
  left_join(EndPara_off, by = "mouseID") %>%
  left_join(Cycles_off, by = "mouseID") %>%
  left_join(Cycles_off_extra, by = "mouseID") %>%
  left_join(AcuteStress_off, by = "mouseID") %>%
  left_join(ChronicStress_off, by = "mouseID")


# ADD OFFSPRING DEMO DATA -------------------------------------------------
Mass_off <- Mass_off %>%
  select(-ParaType) %>%
  addOffspringDemoData() %>%
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

# Filter out extra second litter males -------
AcuteStress_males_2ndLitter <-  AcuteStress_off %>%
  filter(
    litterNum == 2,
    sex == "M",
    !(is.na(cort_hr0) | is.na(cort_hr5)),
    !(exclude_cort_hr5 | exclude_cort_hr5)
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
