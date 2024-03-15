getMeanCVfromReplicates <- function(assayPlate, colToSum = netOD) {
  summarizedPlate <- assayPlate %>%
    group_by(plateID) %>%
    summarise(
      "mean{{ colToSum }}" := mean({{ colToSum }}, na.rm = TRUE),
      "CV{{ colToSum }}" := sd({{ colToSum }}, na.rm = TRUE)/mean({{ colToSum }}, na.rm = TRUE) * 100
    )
  return(summarizedPlate)
}

loadCortAssayPlate <- function(filePath){
  # This will combine all of the different plate information
  # layouts into a single dataframe with one well per row
  
  assayPlate <- read_plate(filePath, "wells") %>%
    mutate(
      type = factor(type, c("NSB", "bufferCtrl", "STD", "QC", "sample"))
    )%>%
    arrange(type, plateID)
  return(assayPlate)
}

calcMeanCV_netOD <- function(assayPlate){
  assayPlate_sum <- assayPlate %>%
    getMeanCVfromReplicates()
  return(assayPlate_sum)
}

addMeanCVToIndividual <- function(assayPlate, assayPlate_sum) {
  comboPlate <- assayPlate %>%
    left_join(
      assayPlate_sum,
      by = "plateID"
    )
}

calcBufferCtrlOD <- function(assayPlate_sum){
  bufferCtrlDF <- assayPlate_sum %>%
    filter(
      plateID == "bufferCtrl"
    )
  bufferCtrlOD <- bufferCtrlDF$meannetOD[1]
  return(bufferCtrlOD)
}

calcPercBinding <- function(assayPlate, bufferCtrlOD){
  assayPlate_percBinding <- assayPlate %>%
    mutate(
      percBinding = netOD / bufferCtrlOD * 100
    )
  return(assayPlate_percBinding)
}

# Initial processing of plate from file to percent binding
processCortEIAtoPercBinding <- function(
  filePath
){
  assayPlate <- loadCortAssayPlate(filePath)
  assayPlate_netOD_meanCV <- calcMeanCV_netOD(assayPlate)
  assayPlate_indPlusMean_netOD <- addMeanCVToIndividual(assayPlate, assayPlate_netOD_meanCV)
  bufferCtrlOD <- calcBufferCtrlOD(assayPlate_netOD_meanCV)
  assayPlate_percBinding <- calcPercBinding(assayPlate, bufferCtrlOD)
  return(
    list(
      assayPlate = assayPlate,
      assayPlate_netOD_meanCV = assayPlate_netOD_meanCV,
      assayPlate_indPlusMean_netOD = assayPlate_indPlusMean_netOD,
      bufferCtrlOD = bufferCtrlOD,
      assayPlate_percBinding = assayPlate_percBinding
    )
  )
}

getCortEIAStandards <- function(assayPlate_percBinding){
  stds <- assayPlate_percBinding %>%
    filter(
      type == "STD"
    )
  return(stds) 
}

calcStandardCurveCortEIA <- function(standards, modelType){
  if(modelType == "4PLC"){
    stdCurve <- drm(
      percBinding ~ stdPgPerWell, 
      data = standards, 
      fct = LL.4()
    )
  } else if(modelType == "linear") {
    stdCurve <- lm(
      percBinding ~ log10(stdPgPerWell),
      data = standards
    )
  } else{
    stop("Select 4PLC or linear as the model types for the cort EIA assay")
  }
  return(stdCurve)
}

estimateSampleConc <- function(assayPlate_percBinding, stdCurve, modelType){
  df <- assayPlate_percBinding
  
  if(modelType == "4PLC"){
    df <- df %>%
      rowwise()%>%
      mutate(
        pgPerWell = ifelse(
          type == "STD",
          stdPgPerWell,
          ifelse(
            type == "QC" | type == "sample",
            ED(stdCurve, percBinding, type="absolute", display=F)[1,1],
            NA
          )
        )
      )
  } else if(modelType == "linear"){
    df <- df %>%
      rowwise() %>%
      mutate(
        logPgPerWell = ifelse(
          type == "STD",
          log10(stdPgPerWell),
          ifelse(
            type == "QC" | type == "sample",
            (percBinding - coef(stdCurve)[[1]]) / coef(stdCurve)[[2]],
            NA
          )
        ),
        pgPerWell = ifelse(
          is.na(logPgPerWell),
          NA,
          10^logPgPerWell
        )
      )
  } else{
    stop("Select 4PLC or linear as the model types for the cort EIA assay")
  }
  df <- df %>%
    rowwise() %>%
    mutate(
      pgPer_mL = ifelse(is.na(pgPerWell), NA, pgPerWell / volPerWell),
      ngPer_mL = ifelse(is.na(pgPer_mL), NA, pgPer_mL / 1000),
      sampleConc_ngPer_mL = ifelse(is.na(ngPer_mL), NA, ngPer_mL * dilutionFactor)
    )
  return(df)
}

# group before calling this
calcMeanCV_concEstimates <- function(assayPlate_concEstimates,
                                     nameCortMean = cortMean, 
                                     nameCortCV = cortCV){
  df_mean <- assayPlate_concEstimates %>%
    summarise(
      "{{nameCortMean}}" := mean(sampleConc_ngPer_mL, na.rm = TRUE),
      "{{nameCortCV}}" := sd(sampleConc_ngPer_mL, na.rm = TRUE) / {{ nameCortMean }} * 100,
      .groups = "drop"
    )
  return(df_mean)
}

addMeanCVToCortEstimation <- function(assayPlate_concEstimates,
                                      nameCortMean = cortMean, 
                                      nameCortCV = cortCV){
  df_mean <- assayPlate_concEstimates %>%
    group_by(plateID) %>%
    calcMeanCV_concEstimates({{ nameCortMean }}, {{ nameCortCV }})
  df_addMean <- addMeanCVToIndividual(assayPlate_concEstimates, df_mean)
  return(df_addMean)
}

getSamplesConcEstimates <- function(assayPlate_concEstimates){
  assayPlate_concEstimates %>%
    filter(
      type == "QC" | type == "sample"
    ) %>%
    arrange(
      type,
      mouseID,
      time
    )
}

calcPlateQC <- function(assayPlate_concEstimates){
  plateQC <- assayPlate_concEstimates %>%
    filter(
      type == "QC"
    )
  
  plateQCmean <- mean(plateQC$sampleConc_ngPer_mL, na.rm = TRUE)
  plateQCID <- plateQC$plateID[1]
  
  return(
    list(
      mean = plateQCmean
      , ID = plateQCID
    )
  )
}

calcMeanSampleCortEstimates <- function(
    assayPlate_concEstimates
    , modelType
    , addQC = FALSE
    , thisPlateID = NULL
){
  sampleMeans <- assayPlate_concEstimates %>%
    filter(
      type == "sample"
    ) %>%
    group_by(mouseID, time) %>%
    calcMeanCV_concEstimates(
      cort,
      cortCV
    )
  
  if(addQC){
    QC_info <- calcPlateQC(assayPlate_concEstimates)
    sampleMeans <- sampleMeans %>%
      mutate(
        plateQC = QC_info$mean
        , QC_ID = QC_info$ID
      )
  }
  
  if(!is.null(thisPlateID)){
    sampleMeans <- sampleMeans %>%
      mutate(
        plateID = thisPlateID
      )
  }
  
  sampleMeans <- sampleMeans %>%
    mutate(
      modelType = modelType
    )
  
  return(sampleMeans)
}

processCortEIAtoSamplesEstimates <- function(
  assayPlate_percBinding,
  standards,
  modelType
  , addQC = FALSE
  , plateID = NULL
){
  stdCurve <- calcStandardCurveCortEIA(standards, modelType)
  assayPlate_concEstimates <- estimateSampleConc(assayPlate_percBinding, stdCurve, modelType)
  assayPlate_concEstimates_meanCV <- addMeanCVToCortEstimation(assayPlate_concEstimates)
  samplesEst <- getSamplesConcEstimates(assayPlate_concEstimates)
  meanSampleResults <- calcMeanSampleCortEstimates(assayPlate_concEstimates, modelType, addQC, plateID)
  plateQC <- calcPlateQC(assayPlate_concEstimates)$mean
  plateQC_id <- calcPlateQC(assayPlate_concEstimates)$ID
    
  
  return(
    list(
      stdCurve = stdCurve,
      assayPlate_concEstimates = assayPlate_concEstimates,
      assayPlate_concEstimates_meanCV = assayPlate_concEstimates_meanCV,
      samplesEst = samplesEst,
      meanSampleResults = meanSampleResults,
      QC_val = plateQC,
      QC_ID = plateQC_id
    )
  )
}


# Google Sheets -----------------------------------------------------------
addCSVFileToGoogleSheet <- function(
    csvFilePath
    , googleSheetDoc
    , newTabName
) {
  csvAsDF = read_csv(csvFilePath)
  
  if(! newTabName %in% sheet_names(googleSheetDoc)){
    googleSheetDoc %>%
      sheet_add(
        newTabName
      )
  }
  
  googleSheetDoc %>%
    range_write(
      csvAsDF
      , sheet = newTabName
    )
}

addMetaRowToGoogle <- function(
    metaDF,
    driveSS,
    plateID,
    modelType,
    sheetName = "plateInfo"
){
  plateInfo <- driveSS %>%
    read_sheet(
      sheet = sheetName
    )
  
  # Find previously entered matching values for the plate/model type
  matchingRows <- which(plateInfo$plateID == plateID & plateInfo$modelType == modelType)
  
  if(length(matchingRows)>0){
    deleteRows <- matchingRows + 1
    driveSS %>%
      range_delete(
        sheet = sheetName
        , range = cell_rows(deleteRows)
      )
  }
  
  # Add the overall plate info row
  driveSS %>%
    sheet_append(
      metaDF
      , sheet = sheetName
    )
}

addStdsToGoogle <- function(
    indivPlusMeanDF,
    driveSS,
    plateID,
    modelType,
    sheetName = "stdInfo"
){
  stdInfo <- driveSS %>%
    read_sheet(
      sheet = sheetName
    )
  
  # Find previously entered matching values for the plate/model type
  matchingRows <- which(stdInfo$plateID == plateID & stdInfo$modelType == modelType)
  
  if(length(matchingRows)>0){
    deleteRows <- matchingRows + 1
    driveSS %>%
      range_delete(
        sheet = sheetName
        , range = cell_rows(deleteRows)
      )
  }
  
  # Add the overall plate info row
  driveSS %>%
    sheet_append(
      indivPlusMeanDF %>%
        filter(type == "STD") %>%
        select(
          -mouseID
          , - time
        ) %>%
        rename(
          stdName = plateID
        ) %>%
        mutate(
          plateID = plateID
          , modelType = modelType
        )
      , sheet = sheetName
    )
}

addSamplesToGoogle <- function(
    meanSampleResults
    , driveSS
    , plateID
    , modelType
){
  meanSampleResults %>%
    sheet_write(
      ss = driveSS
      , sheet = paste0(modelType, "_", plateID)
    )
}

