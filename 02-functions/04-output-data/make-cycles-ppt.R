
# File Names --------------------------------------------------------------
buildCycleFileName <- function(
  cycleID,
  date,
  cohort
){
  cohortText <- paste0("LBN_000", cohort)
  idText <- sprintf("%04d", as.integer(cycleID))
  fileName <- paste0(date, "-", cohortText, "-", idText, ".jpg")
  return(fileName)
}

regExCycleFileName <- function(
  date,
  cycleID
){
  idText <- sprintf("%04d", as.integer(cycleID))
  yyyy <- year(date)
  mm <- month(date)
  dd <- day(date)
  regEx <- paste0("^.*", date, ".*[-_]", idText, "\\.jpg$")
  return(regEx)
}
regExUterinePicFileName <- function(
  mouseID
){
  regEx <- paste0("^.*uterus-", mouseID, ".*\\.jpg$")
  return(regEx)
}


# Get Paths for Data Frame ------------------------------------------------

addRegExForSamplingDF <- function(
  samplingDF
){
  df <- samplingDF %>%
    select(
      Mouse_ID,
      num_ID,
      Sac_date,
      Cohort,
      cyclingFolderPath,
      ReproTract_mass,
      maxLH,
      AgeInDays
    ) %>%
    arrange(
      ReproTract_mass
    ) %>%
    mutate(
      endCycleDay = AgeInDays - 70 + 1,
      startCycleDay = ifelse(endCycleDay - 20 > 0, endCycleDay - 20, 1),
      amRegEx = regExCycleFileName(Sac_date, num_ID),
      ayerRegEx = regExCycleFileName(Sac_date - 1, num_ID),
      anteAyerRegEx = regExCycleFileName(Sac_date - 2, num_ID),
      uterinePicRegEx = regExUterinePicFileName(Mouse_ID)
    )
  return(df)
}

addSamplingImgFilePaths <- function(
  samplingDF_withRegEx,
  uterinePicFolder = LBN_uterinePicsFolder
){
  df <- samplingDF_withRegEx %>%
    rowwise() %>%
    mutate(
      AMPath = dir_ls(
        path = cyclingFolderPath,
        all = TRUE,
        recurse = TRUE,
        type = "any",
        regexp = amRegEx,
        invert = FALSE,
        fail = TRUE
      ),
      ayerPath = dir_ls(
        path = cyclingFolderPath,
        all = TRUE,
        recurse = TRUE,
        type = "any",
        regexp = ayerRegEx,
        invert = FALSE,
        fail = TRUE
      ),
      anteAyerPath = dir_ls(
        path = cyclingFolderPath,
        all = TRUE,
        recurse = TRUE,
        type = "any",
        regexp = anteAyerRegEx,
        invert = FALSE,
        fail = TRUE
      ),
      uterinePicPath = ifelse(
        length(
          dir_ls(
            path = uterinePicFolder,
            all = TRUE,
            recurse = FALSE,
            type = "any",
            regexp = uterinePicRegEx,
            invert = FALSE,
            fail = FALSE
          )
        ) > 0,
        dir_ls(
          path = uterinePicFolder,
          all = TRUE,
          recurse = FALSE,
          type = "any",
          regexp = uterinePicRegEx,
          invert = FALSE,
          fail = FALSE
        ),
        NA
      )
    )
  return(df)
}

# Search Directory ---------------------------------------------------------------

getMouseCycleImgPaths <- function(
  cohortCyclingFolderPath,
  mouseFolderName
){
  cycleImgPaths <- dir_ls(
    path = file.path(cohortCyclingFolderPath, mouseFolderName),
    all = FALSE,
    recurse = FALSE,
    type = "file",
    glob = "*.jpg",
  )
  return(cycleImgPaths)
}


# Add to PPT --------------------------------------------------------------


addImgsToCyclePPT <- function(
  cycleImgPaths,
  sectionLabel,
  cyclePPT,
  numPerSlide = 12 # 9 or 12
) {
  # Number of images within directory
  numImgs <- length(cycleImgPaths)
  
  # Add a section header and add section label
  cyclePPT <- add_slide(x = cyclePPT, layout = "Section Header")
  cyclePPT <- ph_with(x = cyclePPT, value = sectionLabel, location = ph_location_type("title"))
  
  # First index
  iImg <- 1
  for(path in cycleImgPaths){
    fileName <- path %>% path_file() %>% path_ext_remove() # get the file name w/o path or extension
    img <- external_img(path, width = 2.68, heigh = 2.14) # get the image
    textID <- paste0("text", iImg)
    imgID <- paste0("img", iImg)
    
    # If img index is 1, add a new slide
    if(iImg == 1){
      slideLayout <- paste0("estrousCycle", numPerSlide)
      cyclePPT <- add_slide(x = cyclePPT, layout = slideLayout)
    }
    
    # add the title
    cyclePPT <- ph_with(
      x = cyclePPT, value = fileName,
      location = ph_location_label(
        textID
      ),
      use_loc_size = TRUE)
    
    # add the image
    cyclePPT <- ph_with(
      x = cyclePPT, value = img,
      location = ph_location_label(
        imgID
      ),
      use_loc_size = TRUE)
    
    # if index is less than the number per slide, add one, otherwise, restart at 1
    if(iImg < numPerSlide) {
      iImg <- iImg + 1
    } else {
      iImg <- 1
    }
  }
  return(cyclePPT)
}

addMouseFolderImgsTocyclePPT <- function(
  mouseFolder,
  cohortCyclingFolderPath,
  cyclePPT,
  numPerSlide = 12
){
  cyclingImgPaths <- getMouseCycleImgPaths(
    cohortCyclingFolderPath = cohortCyclingFolderPath, 
    mouseFolderName = mouseFolder
  )
  cyclePPT <- addImgsToCyclePPT(
    cycleImgPaths = cyclingImgPaths,
    sectionLabel = mouseFolder,
    cyclePPT = cyclePPT,
    numPerSlide = numPerSlide
  )
  return(cyclePPT)
}


# Sampling Slide ----------------------------------------------------------

createSamplingSlide <- function(
  mouseID,
  cycleID,
  maxLH = NULL,
  uterineMass,
  amImgPath,
  prevDayImgPath,
  twoDayPrevImgPath,
  uterinePicPath,
  startCycleDay,
  endCycleDay,
  samplingPPT,
  cyclingDF_long,
  slideVersion = 2
){
  samplingPPT <- add_slide(samplingPPT, layout = paste0("samplingSlide", slideVersion))
  # print(amImgPath)
  amImg <- external_img(amImgPath, width = 2.68, height = 2.14)
  ayerImg <- external_img(prevDayImgPath, width = 2.68, height = 2.14)
  anteAyerImg <- external_img(twoDayPrevImgPath, width = 2.68, height = 2.14)
  
  mouseLabel <- paste0(mouseID, " - ", cycleID)
  maxLHLabel <- ifelse(!is.na(maxLH), paste0(maxLH, " ng/mL"), "")
  uterineMassLabel <- ifelse(!is.na(uterineMass), paste0(uterineMass, " mg"), "")
  
  # print(paste("Mouse:", mouseID, "Start Day:", startCycleDay, "End Day:", endCycleDay))
  cyclingPlot <- cyclingDF_long %>%
    filter(
      Mouse_ID == mouseID,
      day <= endCycleDay & day >= startCycleDay
    ) %>%
    plotCycleTraces_single()
  
  samplingPPT <- ph_with(
    x = samplingPPT,
    value = mouseLabel,
    location = ph_location_label(
      "mouseID"
    ),
    use_loc_size = TRUE
  )
  samplingPPT <- ph_with(
    x = samplingPPT,
    value = maxLHLabel,
    location = ph_location_label(
      "maxLH"
    ),
    use_loc_size = TRUE
  )
  samplingPPT <- ph_with(
    x = samplingPPT,
    value = uterineMassLabel,
    location = ph_location_label(
      "uterineMass"
    ),
    use_loc_size = TRUE
  )
  samplingPPT <- ph_with(
    x = samplingPPT,
    value = amImg,
    location = ph_location_label(
      "imgAM"
    ),
    use_loc_size = TRUE
  )
  samplingPPT <- ph_with(
    x = samplingPPT,
    value = ayerImg,
    location = ph_location_label(
      "imgAyer"
    ),
    use_loc_size = TRUE
  )
  samplingPPT <- ph_with(
    x = samplingPPT,
    value = anteAyerImg,
    location = ph_location_label(
      "imgAnteAyer"
    ),
    use_loc_size = TRUE
  )
  
  if(!is.na(uterinePicPath) & slideVersion == 2){
    uterineImg <- external_img(uterinePicPath)
    samplingPPT <- ph_with(
      x = samplingPPT,
      value = uterineImg,
      location = ph_location_label(
        "uterusPic"
      ),
      use_loc_size = FALSE
    )
  }
  
  samplingPPT <- ph_with(
    x = samplingPPT,
    value = cyclingPlot,
    location = ph_location_label(
      "cyclePlot"
    )
  )
  return(samplingPPT)
}

addSamplingSlidesFromDF <- function(
  samplingDF,
  cyclingDF,
  samplingPPT,
  slideVersion = 2
){
  cyclingDF_long <- cyclingDF %>%
    makeCyclesLong()
  pwalk(
    list(
      mouseID = samplingDF$Mouse_ID,
      cycleID = samplingDF$num_ID,
      maxLH = samplingDF$maxLH,
      uterineMass = samplingDF$ReproTract_mass,
      amImgPath = samplingDF$AMPath,
      prevDayImgPath = samplingDF$ayerPath,
      twoDayPrevImgPath = samplingDF$anteAyerPath,
      uterinePicPath = samplingDF$uterinePicPath,
      startCycleDay = samplingDF$startCycleDay,
      endCycleDay = samplingDF$endCycleDay
    ),
    createSamplingSlide,
    samplingPPT = samplingPPT,
    cyclingDF_long = cyclingDF_long,
    slideVersion = slideVersion
  )
}
