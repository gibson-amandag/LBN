# LOAD EXCEL SHEETS -------------------------------------------------------

Demo_dam <- loadExcelSheet(dataFolder, LBN_DataName, "Demo_dam")
Demo_off <- loadExcelSheet(dataFolder, LBN_DataName, "Demo_off")
Off_ID <- loadExcelSheet(dataFolder, LBN_DataName, "Off_ID")
Mass_off <- loadExcelSheet(dataFolder, LBN_DataName, "Mass_off")
Maturation_off <- loadExcelSheet(dataFolder, LBN_DataName, "Maturation_off")
EndPara_off <- loadExcelSheet(dataFolder, LBN_DataName, "EndParadigm_off")
Cycles_off <- loadExcelSheet(dataFolder, LBN_DataName, "Cycles_off")
Cycles_off_extra <- loadExcelSheet(dataFolder, LBN_DataName, "Cycles_off_extra")
CohortCyclingFolder <- loadExcelSheet(dataFolder, LBN_DataName, "CohortCyclingFolder")
Sacrifice_off <- loadExcelSheet(dataFolder, LBN_DataName, "Sacrifice_off")
Cort_off <- loadExcelSheet(dataFolder, LBN_DataName, "Cort_off")
LH_code <- loadExcelSheet(dataFolder, LBN_DataName, "LH_code")
LH_off <- loadExcelSheet(dataFolder, LBN_DataName, "LH_off")
ChronicStress_off <- loadExcelSheet(dataFolder, LBN_DataName, "ChronicStress_off")
CRH_dam <- loadExcelSheet(dataFolder, LBN_DataName, "CRH_dam")
behavior_ZT0 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT0")
behavior_ZT14 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT14")
behavior_ZT19 <- loadExcelSheet(dataFolder, LBN_DataName, "Dam_behavior_ZT19")
niceNames <- loadExcelSheet(dataFolder, LBN_DataName, "plotLabels")


# FORMAT DATASETS ---------------------------------------------------------

#Make factor variables
# Dam_ID
# Dam
# Mouse_ID
# Paradigm
# Litter number
# Cohort

Demo_dam <- Demo_dam %>%
  makeFactors(c(
    Dam_ID,
    Dam,
    ParaType,
    Litter_num,
    Cohort
  )) %>%
  orderEarlyLifeTrt()

Demo_off <- makeFactors(Demo_off, c(Dam_ID, Mouse_ID, sex))
Off_ID <- makeFactors(Off_ID, c(Mouse_ID))

Demo_off <- Demo_off %>%
  left_join(Off_ID, by = "Mouse_ID")

Mass_off <- makeFactors(Mass_off, Mouse_ID)
Maturation_off <- makeFactors(Maturation_off, Mouse_ID)
EndPara_off <- makeFactors(EndPara_off, Mouse_ID)
Cycles_off <- makeFactors(Cycles_off, Mouse_ID)
Cycles_off_extra <- makeFactors(Cycles_off_extra, Mouse_ID)
CohortCyclingFolder <- makeFactors(CohortCyclingFolder, Cohort)
Sacrifice_off <- Sacrifice_off %>%
  orderAdultTrt() %>%
  makeFactors(c(Mouse_ID, adultTrt)) %>%
  calcOrganMassByBodyMass(ReproTract_mass) %>%
  calcOrganMassByBodyMass(Gonad_mass) %>%
  calcOrganMassByBodyMass(Adrenal_mass)
ChronicStress_off <- makeFactors(ChronicStress_off, Mouse_ID)
CRH_dam <- makeFactors(CRH_dam, c(Dam_ID,Dam))
behavior_ZT0 <- makeFactors(behavior_ZT0, Dam_ID)
behavior_ZT14 <- makeFactors(behavior_ZT14, Dam_ID)
Cort_off <- makeFactors(Cort_off, Mouse_ID)
LH_code <- makeFactors(LH_code, c(sampleID, Mouse_ID))
LH_off <- makeFactors(LH_off, c(sampleID))

behaviorDFs <- list(
  behavior_ZT0,
  behavior_ZT14,
  behavior_ZT19
)

CohortCyclingFolder <- CohortCyclingFolder %>%
  mutate(
    cyclingFolderPath = ifelse(!is.na(CyclesFolder), file.path(LBN_ServerFolder, paste0("LBN_", sprintf("%04d", Cohort)), CyclesFolder), NA)
  )
## Format Dam Demo ------------------------------------------------------
Demo_dam <- Demo_dam %>%
  mutate(pupLoss = Litter_size_startPara - Litter_size_endPara) %>%
  convertStartPara() %>%
  left_join(CohortCyclingFolder, by = "Cohort")

# DAM BEHAVIOR ------------------------------------------------------------

# Long-form - combined in one
dam_behavior_noDemo <- bind_rows(behaviorDFs)

# Add dam demo info
dam_behavior <- dam_behavior_noDemo %>%
  left_join(
    Demo_dam,
    by = "Dam_ID"
  )

# Make wide behavior table
dam_behavior_wide <- dam_behavior_noDemo %>%
  pivot_wider(
    id_cols = Dam_ID,
    names_from = time,
    values_from = Duration:Avg_dur_on_nest,
    names_prefix = "ZT",
    names_sep = "_"
  )

# Add behavior wide to Demo_dam
Demo_dam <- Demo_dam %>%
  left_join(dam_behavior_wide, by = "Dam_ID")


# CORT AND LH -------------------------------------------------------------


Cort_off_wide <- Cort_off %>%
  pivot_wider(
    id_cols = Mouse_ID,
    names_from = time,
    values_from = cort,
    names_prefix = "cort_hr",
  )

# Add cort data to Sacrifice_off to make AcuteStress_off
AcuteStress_off <- Sacrifice_off %>%
  left_join(
    Cort_off_wide,
    by = "Mouse_ID"
  )

# Add stress day demo to long cort df
Cort_off <- Cort_off %>%
  left_join(
    Sacrifice_off,
    by = "Mouse_ID"
  )

# Add mouse and time info to LH values
LH_off <- LH_off %>%
  left_join(
    LH_code,
    by = "sampleID"
  )

# Get max LH value after baseline for each mouse
LH_max <- LH_off %>%
  getMaxFromRepMeasures(
    col = LH,
    maxColName = maxLH,
    groupingVar = Mouse_ID
  )

# Make wide version of LH
LH_off_wide <- LH_off %>%
  pivot_wider(
    id_cols = Mouse_ID,
    names_from = time,
    values_from = LH,
    names_prefix = "LH_hr",
  ) %>%
  left_join( # Add max column
    LH_max,
    by = "Mouse_ID"
  )

# Add LH data to AcuteStress_off
AcuteStress_off <- AcuteStress_off %>%
  left_join(
    LH_off_wide,
    by = "Mouse_ID"
  )

# Add stress day demo to long cort df
LH_off <- LH_off %>%
  left_join(
    Sacrifice_off,
    by = "Mouse_ID"
  )

# COMBINE ALL DFS INTO ONE ------------------------------------------------

LBN_all <- Demo_off %>%
  left_join(Demo_dam, by = "Dam_ID") %>%
  full_join(select(Mass_off, -ParaType), by = "Mouse_ID") %>%
  # full_join(Maturation_off, by = "Mouse_ID") %>%
  full_join(EndPara_off, by = "Mouse_ID") %>%
  full_join(Cycles_off, by = "Mouse_ID") %>%
  full_join(Cycles_off_extra, by = "Mouse_ID")%>%
  full_join(AcuteStress_off, by = "Mouse_ID") %>%
  full_join(ChronicStress_off, by = "Mouse_ID")

# DAM DEMO FOR OFFSPRING --------------------------------------------------

Demo_dam_for_offspring <- Demo_dam %>%
  select(
    Dam_ID,
    earlyLifeTrt,
    Litter_num,
    DOB, 
    Cohort,
    cyclingFolderPath,
    ParaType,
    pupLoss,
    Litter_size,
    Avg_litter_mass_startPara,
    Mass_P2,
    Mass_P4,
    Dam,
    Dam_cage, 
    Dam_Strain,
    Strain,
    Sire, 
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
  left_join(select(Mass_off, -ParaType), by = "Mouse_ID") %>%
  # left_join(Maturation_off, by = "Mouse_ID") %>%
  left_join(EndPara_off, by = "Mouse_ID") %>%
  left_join(Cycles_off, by = "Mouse_ID") %>%
  left_join(Cycles_off_extra, by = "Mouse_ID") %>%
  left_join(AcuteStress_off, by = "Mouse_ID") %>%
  left_join(ChronicStress_off, by = "Mouse_ID")


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
    Mouse_ID,
    Mass_P22,
    Mass_P23,
    Mass_P24,
    Mass_P70,
    Mass_P71,
    Mass_P72
    ),
    by = "Mouse_ID"
  ) %>%
  setUpMaturation() 

EndPara_off <- EndPara_off %>%
  addOffspringDemoData()

Cycles_off <- Cycles_off %>%
  addOffspringDemoData() %>%
  countEstrousStageDays()

Cycles_off_all <- Cycles_off %>%
  left_join(Cycles_off_extra, by = "Mouse_ID")

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

# UPDATE COMBO FRAMES WITH MATURATION -------------------------------------
LBN_all <- LBN_all %>%
  left_join(
    Maturation_off %>% 
      select(
        Mouse_ID,
        AGD_wean:AGD_P72
      ), 
    by = "Mouse_ID"
  )

LBN_data <- LBN_data %>%
  left_join(
    Maturation_off %>%
      select(
        Mouse_ID,
        AGD_wean:AGD_P72
      ), 
    by = "Mouse_ID"
  )

