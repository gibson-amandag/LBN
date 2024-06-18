dataName = "AGG_sagittalGABA_stressAVPV_data.xlsx"
isManuscript = 0

# LOAD EXCEL SHEETS -------------------------------------------------------

damInfo_s24 <- loadExcelSheet(dataFolder, dataName, "DamInfo")
offspringInfo_s24 <- loadExcelSheet(dataFolder, dataName, "OffspringInfo")
cycles_s24 <- loadExcelSheet(dataFolder, dataName, "Cycles_off")
fileInfo_s24 <- loadExcelSheet(dataFolder, dataName, "fileInfo")
stressInfo_s24 <- loadExcelSheet(dataFolder, dataName, "stressInfo")
slicingInfo_s24 <- loadExcelSheet(dataFolder, dataName, "slicingInfo")
cellInfo_s24 <- loadExcelSheet(dataFolder, dataName, "cellInfo")
LH_s24 <- loadExcelSheet(dataFolder, dataName, "LH")
cort_s24 <- loadExcelSheet(dataFolder, dataName, "cort")
LH_code_s24 <- loadExcelSheet(dataFolder, dataName, "LH_code")
GABApscs_s24 <- loadExcelSheet(dataFolder, dataName, "GABAPSCs")
AVPVfiring_s24 <- loadExcelSheet(dataFolder, dataName, "AVPVfiring")

# FORMAT DATASETS ---------------------------------------------------------

#Make factor variables
# damID
# dam
# mouseID
# Paradigm
# Litter number
# cohort

damInfo_s24 <- damInfo_s24 %>%
  makeFactors(c(
    damID
    , dam
    , damStrain
    , strain
    , sireStrain
  ))

offspringInfo_s24 <- offspringInfo_s24 %>%
  makeFactors(c(
    mouseID
    , damID
    , sex
  ))

cycles_s24 <- cycles_s24 %>%
  makeFactors(c(
    mouseID
  ))

stressInfo_s24 <- stressInfo_s24 %>%
  orderAdultTrt() %>%
  makeFactors(c(
    mouseID
    , adultTrt
  )) %>%
  calcOrganMassByBodyMass(ReproTract_mass) %>%
  calcOrganMassByBodyMass_AM(ReproTract_mass) %>%
  calcOrganMassByBodyMass(Gonad_mass) %>%
  calcOrganMassByBodyMass_AM(Gonad_mass) %>%
  calcOrganMassByBodyMass(Adrenal_mass) %>%
  calcOrganMassByBodyMass_AM(Adrenal_mass) %>%
  mutate(
    bodyMass_diff = Body_mass_sac - Body_mass_AM
    , percChangeBodyMass = bodyMass_diff / Body_mass_AM * 100
  )

slicingInfo_s24 <- slicingInfo_s24 %>%
  makeFactors(c(
    mouseID
  ))

cellInfo_s24 <- cellInfo_s24 %>%
  makeFactors(c(
    mouseID
    , cellID
  ))

LH_s24 <- LH_s24 %>%
  makeFactors(c(
    sampleID
  ))

LH_code_s24 <- LH_code_s24 %>%
  makeFactors(c(
    sampleID
    , mouseID
    , originalID
  ))

cort_s24 <- cort_s24 %>%
  makeFactors(c(
    mouseID
    , QC_ID
    , plateID
    , modelType
  ))

GABApscs_s24 <- GABApscs_s24 %>%
  makeFactors(c(
    cellID
  ))

## ADD AVPV firing

AVPVfiring_s24 <- AVPVfiring_s24 %>%
  makeFactors(c(
    cellID
  ))

# Cycling image location
# could specify this within the environment instead of saving this in the file to make it more robust across computers.
# for now (2024-05-13) the full path is in the excel file to be able to easy load into
# RShiny app and see files
fileInfo_s24 <- fileInfo_s24 %>%
  mutate(
    cyclingFolderPath = file.path(summer2024_ServerFolder)
  )


# CORT AND LH -------------------------------------------------------------
cort_s24 <- cort_s24 %>%
  mutate(
    exclude = ifelse(is.na(exclude), FALSE, TRUE)
  )

cort_s24_wide <- cort_s24 %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = cort,
    names_prefix = "cort_hr"
  )

cort_s24__exclude_wide <- cort_s24 %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = exclude,
    names_prefix = "exclude_cort_hr"
  )

cort_s24_wide <- cort_s24_wide %>%
  left_join(
    cort_s24__exclude_wide
    , by = "mouseID"
  ) %>%
  left_join(
    cort_s24 %>%
      filter(
        time == 0 # assumes on same plate
      ) %>%
      select(
        mouseID
        , cortCV
        , plateQC
        , QC_ID
        , plateID
        , modelType
      )
  )

# Add cort data to stressInfo_s24 to make acuteStress_s24
acuteStress_s24 <- stressInfo_s24 %>%
  left_join(
    cort_s24_wide
    , by = "mouseID"
  ) %>%
  mutate(
    # uncomment once have data
    # exclude_cort_hr0 = ifelse(!is.na(exclude_cort_hr0), exclude_cort_hr0, FALSE),
    # exclude_cort_hr5 = ifelse(!is.na(exclude_cort_hr5), exclude_cort_hr5, FALSE)
  )

# Add stress day demo to long cort df
cort_s24 <- cort_s24 %>%
  left_join(
    stressInfo_s24
    , by = "mouseID"
  )

# Add mouse and time info to LH values
LH_s24 <- LH_s24 %>%
  left_join(
    LH_code_s24,
    by = "sampleID"
  )

# Get max LH value after baseline for each mouse
LH_max_s24 <- LH_s24 %>%
  filter(time !=0) # missing initially -> max could have been AM

if (nrow(LH_max_s24)>0) {
  LH_max_s24 <- LH_max_s24 %>%
  getMaxFromRepMeasures_plusOtherVal(
    col = LH,
    maxColName = maxLH,
    groupingVar = mouseID,
    valCol = time,
    valAtMaxColName = timeAtMax
  )
} else{
  LH_max_s24 <- LH_max_s24 %>%
    mutate(
      maxLH = NA
      , timeAtMax = NA
    ) %>%
    select(
      mouseID
      , maxLH
      , timeAtMax
    )
}

LH_AUC_s24 <- LH_s24 %>%
  group_by(
    mouseID
  ) %>%
  summarize(
    LH_AUC = AUC(time, LH)
  )

LH_s24 <- LH_s24 %>%
  left_join(
    LH_max_s24,
    by = "mouseID"
  ) %>%
  left_join(
    LH_AUC_s24,
    by = "mouseID"
  )

# Make wide version of LH
LH_s24_wide <- LH_s24 %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = LH,
    names_prefix = "LH_hr",
  ) %>%
  left_join( # Add max column
    LH_max_s24,
    by = "mouseID"
  ) %>%
  left_join(
    LH_AUC_s24,
    by = "mouseID"
  )

# Add LH data to acuteStress_s24
acuteStress_s24 <- acuteStress_s24 %>%
  left_join(
    LH_s24_wide,
    by = "mouseID"
  )

# Add stress day demo to long cort df
LH_s24 <- LH_s24 %>%
  left_join(
    stressInfo_s24,
    by = "mouseID"
  )

# GABA PSCs ---------------------------------------------------------------

GABApscs_s24 <- GABApscs_s24 %>%
  left_join(
    cellInfo_s24,
    by = "cellID"
  ) %>%
  left_join(
    slicingInfo_s24,
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

# AVPV  ---------------------------------------------------------------

AVPVfiring_s24 <- AVPVfiring_s24 %>%
  left_join(
    cellInfo_s24,
    by = "cellID"
  ) %>%
  left_join(
    slicingInfo_s24,
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

s24_all <- offspringInfo_s24 %>%
  left_join(damInfo_s24, by = "damID") %>%
  full_join(cycles_s24, by = "mouseID") %>%
  full_join(acuteStress_s24, by = "mouseID") %>%
  full_join(slicingInfo_s24, by = "mouseID") %>%
  mutate(
    cycleImgFolder = fileInfo_s24$cycleImgFolder[[1]]
    , cyclingFolderPath = fileInfo_s24$cyclingFolderPath[[1]]
  )

# DAM DEMO FOR OFFSPRING --------------------------------------------------

damInfoForOffspring <- damInfo_s24 %>%
  select(
    damID,
    DOB, 
    dam,
    damCage, 
    damStrain,
    strain,
    sire,
    sireStrain
  )

# Update offspring demographics
offspringInfo_s24 <- addDamDemoData(
  offspringInfo_s24,
  damInfoForOffspring
)


# COMBINE OFFSPRING DATA INTO ONE -----------------------------------------
s24_data <- offspringInfo_s24 %>%
  left_join(cycles_s24, by = "mouseID") %>%
  left_join(acuteStress_s24, by = "mouseID") %>%
  mutate(
    cycleImgFolder = fileInfo_s24$cycleImgFolder[[1]]
    , cyclingFolderPath = fileInfo_s24$cyclingFolderPath[[1]]
  )


# ADD OFFSPRING DEMO DATA -------------------------------------------------
cycles_s24 <- cycles_s24 %>%
  addOffspringDemoData(offDemo_toAdd = offspringInfo_s24) %>%
  countEstrousStageDays()

acuteStress_s24 <- acuteStress_s24 %>%
  addOffspringDemoData(offDemo_toAdd = offspringInfo_s24) %>%
  calcAgeInDays()

GABApscs_s24 <- GABApscs_s24 %>%
  left_join(
    acuteStress_s24,
    by = "mouseID"
  )

cort_s24 <- cort_s24 %>%
  addOffspringDemoData(offDemo_toAdd = offspringInfo_s24)

LH_s24 <- LH_s24 %>%
  addOffspringDemoData(offDemo_toAdd = offspringInfo_s24)

LH_code_s24 <- LH_code_s24 %>%
  left_join(
    stressInfo_s24,
    by = "mouseID"
  )%>%
  addOffspringDemoData(offDemo_toAdd = offspringInfo_s24)

