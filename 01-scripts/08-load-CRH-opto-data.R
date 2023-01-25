

# Load sheets from excel --------------------------------------------------


CRH_opto_path <- file.path(dataFolder, "AGG_opto_data.xlsx")
optoDamInfo <- loadExcelSheet_fromFile(CRH_opto_path, "DamInfo")
optoOffspringInfo <- loadExcelSheet_fromFile(CRH_opto_path, "OffspringInfo")
optoCycles <- loadExcelSheet_fromFile(CRH_opto_path, "Cycles_off")
optoSlicingInfo <- loadExcelSheet_fromFile(CRH_opto_path, "slicingInfo")
optoCellInfo <- loadExcelSheet_fromFile(CRH_opto_path, "cellInfo")
optoOutput <- loadExcelSheet_fromFile(CRH_opto_path, "optoOutput")

optoFileInfo <- loadExcelSheet_fromFile(CRH_opto_path, "fileInfo")
opto_cycleDir <- optoFileInfo$cycleImgFolder[1]

optoLHcode <- loadExcelSheet_fromFile(CRH_opto_path, "LH_code")
optoLH <- loadExcelSheet_fromFile(CRH_opto_path, "LH")

optoSamplingInfo <- loadExcelSheet_fromFile(CRH_opto_path, "samplingInfo")

# Make factors ------------------------------------------------------------

optoDamInfo <- optoDamInfo %>%
  makeFactors(c(
    damID
    , dam
  ))

optoOffspringInfo <- optoOffspringInfo %>%
  makeFactors(c(
    damID
    , mouseID
    , sex
  ))

optoCycles <- optoCycles %>%
  makeFactors(c(
    mouseID
  ))

optoSlicingInfo <- optoSlicingInfo %>%
  makeFactors(mouseID)

optoCellInfo <- optoCellInfo %>%
  makeFactors(c(
    cellID
    , mouseID
  ))

optoOutput <- optoOutput %>%
  makeFactors(c(
    cellID
    , seriesName
  ))

optoLHcode <- optoLHcode %>%
  makeFactors(
    c(
      mouseID
      , sampleID
    )
  )

optoLH <- optoLH %>%
  makeFactors(
    c(
      sampleID
    )
  )

optoSamplingInfo <- optoSamplingInfo %>%
  makeFactors(
    c(
      mouseID
    )
  )


# Combine datasets --------------------------------------------------------

optoOffspringInfo <- optoOffspringInfo %>%
  left_join(
    optoDamInfo,
    by = "damID"
  ) %>%
  mutate(
    cyclingFolderPath = opto_cycleDir,
    earlyLifeTrt = NA,
    cohort = NA
  )

optoCycles <- optoCycles %>%
  mutate(
    num_ID = cycleID
    , .after = cycleID
  ) 



optoCycles <- optoCycles %>%
  left_join(
    optoOffspringInfo
    , by = "mouseID"
  )

optoOffspringInfo <- optoOffspringInfo %>%
  left_join(
    optoCycles %>%
      select(
        mouseID
        , cycleID
        , num_ID
      )
    , by = "mouseID"
  )

optoMouseInfo <- optoOffspringInfo %>%
  left_join(
    optoSlicingInfo
    , by = "mouseID"
  )

optoSlicingInfo <- optoSlicingInfo %>%
  left_join(
    optoOffspringInfo
    , by = "mouseID"
  )

optoCellInfo <- optoCellInfo %>%
  left_join(
    optoSlicingInfo
    , by = "mouseID"
  )

optoOutput <- optoOutput %>%
  left_join(
    optoCellInfo
    , by = "cellID"
  )

optoLH <- optoLHcode %>%
  left_join(
    optoLH,
    by = "sampleID"
  )

optoSamplingInfo <- optoSamplingInfo %>%
  rename(
    samplingStage = cycleStage
  ) %>%
  left_join(
    optoLH,
    by = c("mouseID", "day")
  ) %>%
  left_join(
    optoMouseInfo,
    by = c("mouseID")
  )


# Filter ------------------------------------------------------------------

optoRemainingFemaleTriples <- optoMouseInfo %>%
  filter(
    sex == "F"
    , !is.na(cycleID)
    , is.na(sacDateOff)
    , is.na(recordingDate)
  ) %>%
  relocate(
    cycleID
    , .after = mouseID
  ) %>%
  mutate(
    comboTrt = NA
  )

optoSampledFemaleTriples <- optoMouseInfo %>%
  filter(
    sex == "F"
    , !is.na(cycleID)
    , is.na(sacDateOff)
    , !is.na(recordingDate)
  ) %>%
  relocate(
    cycleID
    , .after = mouseID
  ) %>%
  mutate(
    comboTrt = NA
  )
