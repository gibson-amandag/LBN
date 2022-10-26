JL_filePath <- normalizePath("C:\\Users\\percs\\OneDrive - Umich\\Moenter lab\\LBN_0002_OneDrive\\DataForR\\AGG_JL_CRH-CrexL10a-GFP_data.xlsx")
JL_fileInfo <- loadExcelSheet_fromFile(JL_filePath, "fileInfo")
JL_cycleDir <- JL_fileInfo$cycleImgFolder[1]

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


JL_filePrefix  <-  "JL_CRHcre-L10a_"
JL_width <- 11.5
JL_height <- 5

JL_sacrifice <- loadExcelSheet_fromFile(JL_filePath, "Sacrifice_off") %>%
  makeFactors("mouseID")
JL_allInfo <- JL_allInfo %>%
  left_join(
    JL_sacrifice,
    by = "mouseID"
  )
JL_sacrifice <- JL_sacrifice %>%
  left_join(
    JL_offspringInfo,
    by = "mouseID"
  )