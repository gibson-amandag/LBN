surgeMin <- 3.8

# LOAD EXCEL SHEETS -------------------------------------------------------

BD_damInfo <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "DamInfo")
BD_offspringInfo <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "OffspringInfo")
BD_cycles <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "Cycles_off")
BD_fileInfo <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "fileInfo")
BD_sampling <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "samplingInfo")
BD_ateNutella <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "ateNutella")
BD_cort <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "cort")
BD_LHcode <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "LH_code")
BD_LH <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "LH")
BD_sampling1 <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "Sacrifice_off1")
BD_sampling2 <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "Sacrifice_off2")
BD_sampling3 <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "Sacrifice_off3")
BD_sampling4 <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "SamplingInfo4")
BD_samplingALPS <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "SamplingInfoALPS")
BD_cort1 <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "Cort_off1")
BD_cort2 <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "Cort_off2")
BD_cort3 <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "Cort_off3")
BD_cort4 <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "Cort4")
BD_cortALPS <- loadExcelSheet(cortAdminFolder, cortAdminFileName, "CortALPS")


# niceNames <- loadExcelSheet(dataFolder, LBN_DataName, "plotLabels")


# FORMAT DATASETS ---------------------------------------------------------

#Make factor variables
# damID
# dam
# mouseID
# cohort

BD_damInfo <- BD_damInfo %>%
  makeFactors(c(
    damID,
    dam
  ))

BD_offspringInfo <- BD_offspringInfo %>%
  makeFactors(c(
    mouseID,
    originalID,
    cohort,
    sex,
    damID
  ))

BD_fileInfo <- BD_fileInfo %>%
  makeFactors(c(
    cohort
  ))

BD_cycles <- BD_cycles %>% makeFactors(c(mouseID))

BD_sampling <- BD_sampling %>% makeFactors(c(mouseID, Sampling_cycle, adultTrt, dosage, primaryExperimenter))
BD_sampling1 <- BD_sampling1 %>% makeFactors(c(mouseID, Sac_cycle, adultTrt, dosage, primaryExperimenter))
BD_sampling2 <- BD_sampling2 %>% makeFactors(c(mouseID, Sac_cycle, adultTrt, dosage, primaryExperimenter))
BD_sampling3 <- BD_sampling3 %>% makeFactors(c(mouseID, Sac_cycle, adultTrt, dosage, primaryExperimenter))
BD_sampling4 <- BD_sampling4 %>% makeFactors(c(mouseID, Sac_cycle, adultTrt, dosage, primaryExperimenter))
BD_samplingALPS <- BD_samplingALPS %>% makeFactors(c(mouseID, Sac_cycle, adultTrt, primaryExperimenter))

BD_cort <- BD_cort %>% makeFactors(c(mouseID, atePrevNutella))
BD_cort1 <- BD_cort1 %>% makeFactors(c(mouseID, atePrevNutella))
BD_cort2 <- BD_cort2 %>% makeFactors(c(mouseID, atePrevNutella))
BD_cort3 <- BD_cort3 %>% makeFactors(c(mouseID, atePrevNutella))
BD_cort4 <- BD_cort4 %>% makeFactors(c(mouseID, atePrevNutella))
BD_cortALPS <- BD_cortALPS %>% makeFactors(c(mouseID))

BD_LHcode <- BD_LHcode %>% makeFactors(c(mouseID, sampleID))
BD_LH <- BD_LH %>% makeFactors(c(sampleID))



# DAM DEMO FOR OFFSPRING --------------------------------------------------

# Update offspring demographics
BD_offspringInfo <- BD_offspringInfo %>%
  left_join(
    BD_damInfo,
    by = "damID"
  ) %>%
  left_join(
    BD_fileInfo,
    by = "cohort"
  )

# Cycles ------------------------------------------------------------------
  
  BD_cycles <- BD_cycles %>%
    left_join(
      BD_offspringInfo,
      by = "mouseID"
    )
  
  BD_offspringInfo <- BD_offspringInfo %>%
    left_join(
      BD_cycles %>%
        select(
          mouseID, cycleID
        ),
        by = "mouseID"
    ) %>% mutate(
      numID = cycleID
    ) %>%
    relocate(
      cycleID, numID,
      .after = originalID
    )

# CORT AND LH -------------------------------------------------------------
makeBDCortWider <- function(df){
  aboveLimitExists <- ifelse("aboveLimit" %in% names(df), TRUE, FALSE)
  if(!aboveLimitExists){
    df <- df %>%
      mutate(
        aboveLimit = NA
      )
  }
  spec <- build_wider_spec(
    df,
    names_from = time, 
    values_from = c(cort, cortCV, exclude, aboveLimit),
    names_sep = "_hr"
  )
  spec <- arrange(spec, time, .value)
  wider <- pivot_wider_spec(
    df %>%
      select(-belowLimit) %>%
      select_if("atePrevNutella" != names(.)),
    spec
  )
  return(wider)
}

processCort <- function(
  cortDF,
  samplingDF,
  demoDF = BD_offspringInfo,
  mouseIDName = "mouseID"
){
  cortDF <- cortDF %>%
    mutate(
      exclude = ifelse(is.na(exclude), FALSE, TRUE)
    )
  
  cortWide <- cortDF %>%
    makeBDCortWider()
  
  if(!"dosage" %in% colnames(samplingDF)){
    samplingDF <- samplingDF %>%
      mutate(
        dosage = NA
      )
  }
  samplingDF <- samplingDF %>%
    unite(
      comboTrt,
      adultTrt,
      dosage,
      sep = "-",
      remove = FALSE
    ) %>%
    mutate(
      comboTrt = factor(comboTrt)
    )
  
  samplingDF_joined <- samplingDF %>%
    left_join(
      cortWide,
      by = mouseIDName
    ) %>%
    mutate(
      across(starts_with("exclude_cort"), ~ ifelse(!is.na(.x), .x, FALSE))
    ) 
  
  samplingDF_withDemo <- samplingDF_joined %>%
    left_join(
      demoDF,
      by = mouseIDName
    )
  
  cortDF <- cortDF %>%
    left_join(
      samplingDF,
      by = mouseIDName
    )%>%
    left_join(
      demoDF,
      by = mouseIDName
    )
  
  return(
    list(
      "cort" = cortDF,
      "cort_wide" = cortWide,
      "sampling" = samplingDF_withDemo,
      "samplingNoDemo" = samplingDF_joined
    )
  )
}

cortSampling <- processCort(BD_cort, BD_sampling)
cortSampling1 <- processCort(BD_cort1, BD_sampling1)
cortSampling2 <- processCort(BD_cort2, BD_sampling2)
cortSampling3 <- processCort(BD_cort3, BD_sampling3)
cortSampling4 <- processCort(BD_cort4, BD_sampling4)
cortSamplingALPS <- processCort(BD_cortALPS, BD_samplingALPS)

allSampling <- bind_rows(
  cortSampling$samplingNoDemo,
  cortSampling1$samplingNoDemo,
  cortSampling2$samplingNoDemo,
  cortSampling4$samplingNoDemo,
  cortSamplingALPS$samplingNoDemo,
)

BD_offspringInfo <-  BD_offspringInfo %>%
  left_join(
    allSampling,
    by = "mouseID"
  )

remove(allSampling)

BD_cort <- cortSampling$cort
BD_sampling <- cortSampling$sampling

BD_cort1 <- cortSampling1$cort
BD_sampling1 <- cortSampling1$sampling

BD_cort2 <- cortSampling2$cort
BD_sampling2 <- cortSampling2$sampling

BD_cort3 <- cortSampling3$cort
BD_sampling3 <- cortSampling3$sampling

BD_cort4 <- cortSampling4$cort
BD_sampling4 <- cortSampling4$sampling

BD_cortALPS <- cortSamplingALPS$cort
BD_samplingALPS <- cortSamplingALPS$sampling

# Calculated ovulated true/fasle

BD_sampling <- BD_sampling %>%
  mutate(
    ovulated = ifelse(oocytes1>0 | oocytes2 > 0, TRUE, FALSE),
    numOocytes = oocytes1 + oocytes2,
    .after = oocytes2
  )

# Add mouse and time info to LH values
BD_LH <- BD_LH %>%
  filter(
    !is.na(sampleID)
  ) %>%
  left_join(
    BD_LHcode %>%
      filter(
        !is.na(sampleID)
      ),
    by = "sampleID"
  )

# Get max LH value after baseline for each mouse
BD_LH_max <- BD_LH %>%
  filter(time !=0) %>% # missing initially -> max could have been AM
  getMaxFromRepMeasures_plusOtherVal(
    col = LH,
    maxColName = maxLH,
    groupingVar = mouseID,
    valCol = time,
    valAtMaxColName = timeAtMax
  )

BD_LH <- BD_LH %>%
  left_join(
    BD_LH_max,
    by = "mouseID"
  ) %>%
  mutate(
    surged = maxLH > surgeMin
  )

# Make wide version of LH
BD_LH_wide <- BD_LH %>%
  pivot_wider(
    id_cols = mouseID,
    names_from = time,
    values_from = LH,
    names_prefix = "LH_hr",
  ) %>%
  left_join( # Add max column
    BD_LH_max,
    by = "mouseID"
  ) %>%
  mutate(
    surged = maxLH > surgeMin
  )

# Add stress day demo to long cort df
BD_LH <- BD_LH %>%
  left_join(
    BD_sampling,
    by = "mouseID"
  )

# Add LH data to BD_sampling
BD_sampling <- BD_sampling %>%
  left_join(
    BD_LH_wide,
    by = "mouseID"
  )

# Add surge and ovulation data to cort df
BD_cort <- BD_cort %>%
  left_join(
    BD_sampling %>%
      select(
        mouseID
        , surged
        , ovulated
      )
    , by = "mouseID"
  )


BD_offspringInfo <- BD_offspringInfo %>%
  left_join(
    BD_LH_wide,
    by = "mouseID"
  )

# Combine datasets -------------
BD_comboNutALPS <- rbind(
  BD_cortALPS %>%
    mutate(
      atePrevNutella = NA,
      ateNutella = NA,
      cortNutTrt = adultTrt
    ), 
  BD_cort4 %>% 
    filter(Sac_date == date_parse("2022-05-27")) %>%
    mutate(
      cortNutTrt = dosage
    )
)
BD_cort1 <- BD_cort1 %>%
  mutate(ateAllNutella = ifelse(is.na(ateNutella), "ate all Nutella", "did not eat all Nutella" ))
BD_cort2 <- BD_cort2 %>%
  mutate(ateAllNutella = ifelse(is.na(ateNutella), "ate all Nutella", "did not eat all Nutella" ))
BD_cort3 <- BD_cort3 %>%
  mutate(ateAllNutella = ifelse(is.na(ateNutella), "ate all Nutella", "did not eat all Nutella" ))
BD_cort4 <- BD_cort4 %>%
  filter(
    Sac_date == "2022-05-27"
  ) %>%
  mutate(ateAllNutella = ifelse(is.na(ateNutella), "ate all Nutella", "did not eat all Nutella" ))

## serum cort - final dataset ------
cortAdminCortDF <- BD_cort %>%
  filter(
    time %in% c(0, 5)
    , is.na(exclude) | exclude == FALSE
    , trust == TRUE
  )
