# File information -------------------
JL_filePrefix  <-  "JL_CRHcre-L10a_"
JL_width <- 11.5
JL_height <- 5


# File paths --------------------------
JL_filePath <- normalizePath("C:\\Users\\percs\\OneDrive - Umich\\Moenter lab\\LBN_0002_OneDrive\\DataForR\\AGG_JL_CRH-CrexL10a-GFP_data.xlsx")
JL_fileInfo <- loadExcelSheet_fromFile(JL_filePath, "fileInfo")
JL_cycleDir <- JL_fileInfo$cycleImgFolder[1]


# Load and prep dataframes -----------------
JL_damInfo <- loadExcelSheet_fromFile(JL_filePath, "DamInfo") %>%
  makeFactors("damID")

JL_offspringInfo <- loadExcelSheet_fromFile(JL_filePath, "OffspringInfo") %>% makeFactors(c("mouseID", "damID"))

JL_offspringInfo <- JL_offspringInfo %>%
  left_join(
    JL_damInfo,
    by = "damID"
  ) %>%
  mutate(
    cyclingFolderPath = JL_cycleDir,
    earlyLifeTrt = NA
  )

JL_cycles <- loadExcelSheet_fromFile(JL_filePath, "Cycles_off") %>%
  makeFactors(
    "mouseID"
  )%>%
  mutate(
    num_ID = cycleID
  )

JL_allInfo <- JL_offspringInfo %>%
  left_join(
    JL_cycles,
    by = "mouseID"
  )

JL_cycles <- JL_cycles %>%
  left_join(
    JL_offspringInfo,
    by = "mouseID"
  )

JL_sacrifice <- loadExcelSheet_fromFile(JL_filePath, "Sacrifice_off") %>%
  makeFactors(c(mouseID)) %>%
  orderAdultTrt()

JL_allInfo <- JL_allInfo %>%
  left_join(
    JL_sacrifice,
    by = "mouseID"
  )
JL_sacrifice <- JL_sacrifice %>%
  left_join(
    JL_offspringInfo,
    by = "mouseID"
  ) %>%
  left_join(
    JL_cycles %>%
      select(
        mouseID
        , cycleID
      )
    , by = "mouseID"
  ) %>%
  unite(
    comboTrt
    , adultTrt
    , perfusionHr
    , sep = "-"
    , remove = FALSE
  )

## Cort and LH ---------------
JL_cort <- loadExcelSheet_fromFile(JL_filePath, "cort") %>%
  makeFactors(
    "mouseID"
  )

JL_cort_wide <- JL_cort %>%
  pivot_wider(
    id_cols = mouseID
    , names_from = time
    , values_from = cort
    , names_prefix = "cort_hr"
  )


JL_acuteStress <- JL_sacrifice %>%
  left_join(
    JL_cort_wide
    , by = "mouseID"
  )

JL_cort <- JL_cort %>%
  left_join(
    JL_sacrifice
    , by = "mouseID"
  )

JL_LH <- loadExcelSheet_fromFile(JL_filePath, "LH") %>%
  makeFactors(
    c(sampleID)
  )

JL_LHcode <- loadExcelSheet_fromFile(JL_filePath, "LH_code") %>%
  makeFactors(
    c(mouseID, sampleID)
  )

JL_LH <- JL_LH %>%
  left_join(
    JL_LHcode
    , by = "sampleID"
  )

JL_LH_max <- JL_LH %>%
  filter(
    time != 0
  ) %>%
  getMaxFromRepMeasures_plusOtherVal(
    col = LH,
    maxColName = maxLH
    , groupingVar = mouseID
    , valCol = time
    , valAtMaxColName = timeAtMax
  )

JL_LH <- JL_LH %>%
  left_join(
    JL_LH_max,
    by = "mouseID"
  )

JL_LH_wide <- JL_LH %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = LH,
    names_prefix = "LH_hr",
  ) %>%
  left_join(
    JL_LH_max
    , by = "mouseID"
  )

JL_acuteStress <- JL_acuteStress %>%
  left_join(
    JL_LH_wide
    , by = "mouseID"
  )

JL_LH <- JL_LH %>%
  left_join(
    JL_acuteStress %>%
      select(
        -maxLH
        , -timeAtMax
      )
    , by = "mouseID"
  )
