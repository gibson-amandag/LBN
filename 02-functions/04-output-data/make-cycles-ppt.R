
# File Names --------------------------------------------------------------
buildLBNCycleFileName <- function(
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
  cycleID,
  excludePM = TRUE,
  excludeUterus = TRUE
){
  idText <- sprintf("%04d", as.integer(cycleID))
  yyyy <- year(date)
  mm <- sprintf("%02d", month(date))
  dd <- sprintf("%02d", day(date))
  regEx <- paste0(
    # "^.*/", # looks for the whole file path when matching, not just file name
    "^", # looks for the whole file path when matching, not just file name
    if(excludePM)"(?!.*PM)",
    if(excludeUterus)"(?!.*uterus)",
    ".*/",
    yyyy,
    "[-_]",
    mm,
    "[-_]",
    dd,
    # date, 
    ".*[-_]", 
    "*",
    idText, "\\.jpg$")
  # print(regEx)
  return(regEx)
}
regExUterinePicFileName <- function(
  MouseID
){
  regEx <- paste0("^.*uterus-", MouseID, ".*\\.jpg$")
  return(regEx)
}

# Get Paths for Data Frame ------------------------------------------------

addRegExForSamplingDF <- function(
  samplingDF,
  arrangeByCycle = FALSE
){
  df <- samplingDF %>%
    select(
      mouseID,
      num_ID,
      Sac_date,
      Cohort,
      comboTrt,
      Sac_cycle,
      cyclingFolderPath,
      ReproTract_mass,
      maxLH,
      AgeInDays
    ) %>%
    arrange(
      if(arrangeByCycle) Sac_cycle,
      ReproTract_mass
    ) %>%
    mutate(
      endCycleDay = AgeInDays,
      startCycleDay = ifelse(endCycleDay - 20 > 69, endCycleDay - 20, 70),
      amRegEx = regExCycleFileName(Sac_date, num_ID),
      ayerRegEx = regExCycleFileName(Sac_date - 1, num_ID),
      anteAyerRegEx = regExCycleFileName(Sac_date - 2, num_ID),
      uterinePicRegEx = regExUterinePicFileName(mouseID)
    )
  return(df)
}

#' Find file matching regular expression within folder
#' 
#' This will return the first match, even if there are multiple found
#'
#' @param folderPath folder path to search within
#' @param regEx a regular expression for the name of the file. The entire file path must match
#' @param searchSubFolders TRUE/FALSE - should it search within the subfolders of the folderPath
#'
#' @return a single file path
#' @export
#'
#' @examples
findMatchingFile <- function(
  folderPath,
  regEx,
  MouseID = NULL,
  type = NULL,
  searchSubFolders = TRUE
){
  foundPaths <- dir_ls(
    path = folderPath,
    all = TRUE,
    recurse = searchSubFolders,
    type = "any",
    regexp = regEx,
    invert = FALSE,
    fail = TRUE,
    perl = T
  )
  if(length(foundPaths) == 0){
    print(paste0("no file found ", MouseID, " ", type))
  } else if(length(foundPaths) > 1){
    print(paste0("multiple files found ", MouseID, " ", type))
    print(foundPaths)
  }
  path <- ifelse(length(foundPaths) > 0, foundPaths[1], NA)
  return(path)
}

addSamplingImgFilePaths <- function(
  samplingDF_withRegEx,
  uterinePicFolder = LBN_uterinePicsFolder
){
  df <- samplingDF_withRegEx %>%
    rowwise() %>%
    mutate(
      AMPath = findMatchingFile(cyclingFolderPath, amRegEx, mouseID, "AM pic"),
      ayerPath = findMatchingFile(cyclingFolderPath, ayerRegEx, mouseID, "yesterday pic"),
      anteAyerPath = findMatchingFile(cyclingFolderPath, anteAyerRegEx, mouseID, "2 days before pic"),
      uterinePicPath = findMatchingFile(uterinePicFolder, uterinePicRegEx, mouseID, "uterus pic")
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
  MouseID,
  cycleID,
  maxLH = NULL,
  uterineMass,
  trt = NULL,
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
  
  mouseLabel <- paste0(MouseID, " - ", cycleID)
  maxLHLabel <- ifelse(!is.na(maxLH), paste0(maxLH, " ng/mL - ", trt), "")
  uterineMassLabel <- ifelse(!is.na(uterineMass), paste0(uterineMass, " mg"), "")
  
  # print(paste("Mouse:", MouseID, "Start Day:", startCycleDay, "End Day:", endCycleDay))
  cyclingPlot <- cyclingDF_long %>%
    filter(
      mouseID == MouseID,
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
  
  if(!is.na(amImgPath)){
    amImg <- external_img(amImgPath, width = 2.68, height = 2.14)
    samplingPPT <- ph_with(
      x = samplingPPT,
      value = amImg,
      location = ph_location_label(
        "imgAM"
      ),
      use_loc_size = TRUE
    )
  }
  
  if(!is.na(prevDayImgPath)){
    ayerImg <- external_img(prevDayImgPath, width = 2.68, height = 2.14)
    samplingPPT <- ph_with(
      x = samplingPPT,
      value = ayerImg,
      location = ph_location_label(
        "imgAyer"
      ),
      use_loc_size = TRUE
    )
  }
  
  if(!is.na(twoDayPrevImgPath)){
    anteAyerImg <- external_img(twoDayPrevImgPath, width = 2.68, height = 2.14)
    samplingPPT <- ph_with(
      x = samplingPPT,
      value = anteAyerImg,
      location = ph_location_label(
        "imgAnteAyer"
      ),
      use_loc_size = TRUE
    )
  }
  
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
      MouseID = samplingDF$mouseID,
      cycleID = samplingDF$num_ID,
      maxLH = samplingDF$maxLH,
      trt = samplingDF$comboTrt,
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
