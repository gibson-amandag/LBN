
# Load Data ---------------------------------------------------------------


mouseInfo_BW <- loadExcelSheet(dataFolder, "BW-CBA_B6-LH_data.xlsx", "mouseInfo") %>%
  makeFactors(mouseID) %>%
  orderAdultTrt()
LH_code_BW <- loadExcelSheet(dataFolder, "BW-CBA_B6-LH_data.xlsx", "LH_code") %>%
  makeFactors(c(mouseID, sampleID))
LH_values_BW <- loadExcelSheet(dataFolder, "BW-CBA_B6-LH_data.xlsx", "LH_values") %>%
  makeFactors(sampleID)



# Combine Datasets --------------------------------------------------------

# Add the LH values to the code dataframe
LH_info_BW <- LH_code_BW %>%
  left_join(
    LH_values_BW,
    by = "sampleID"
  ) %>%
  select(
    -originalID
  ) 

# Get the max LH for each mouse
LH_max_BW <- LH_info_BW %>%
  getMaxFromRepMeasures(
    col = LH,
    maxColName = maxLH,
    groupingVar = mouseID
  )

# Add the max to the long dataframe
LH_info_BW <- LH_info_BW %>%
  left_join(
    LH_max_BW,
    by = "mouseID"
  )

# Make a wide dataframe with LH values
LH_info_BW_wide <- LH_info_BW %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = LH,
    names_prefix = "LH_hr"
  ) %>%
  left_join(
    LH_max_BW,
    by = "mouseID"
  )

# Add the LH data to the mouse data - long
BW_data <- LH_info_BW %>%
  left_join(
    mouseInfo_BW,
    by = "mouseID"
  )


# Add the LH data to the mouse data - wide
BW_data_wide <- LH_info_BW_wide %>%
  left_join(
    mouseInfo_BW,
    by = "mouseID"
  )