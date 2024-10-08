
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

regExMouseAllFiles <- function(
  cycleID,
  excludePM = TRUE,
  excludeUterus = TRUE
){
  idText <- sprintf("%04d", as.integer(cycleID))
  regEx <- paste0(
    # "^.*/", # looks for the whole file path when matching, not just file name
    "^", # looks for the whole file path when matching, not just file name
    if(excludePM)"(?!.*PM)",
    if(excludeUterus)"(?!.*uterus)",
    ".*/",
    ".*[-_]", 
    "*",
    idText, "\\.jpg$")
  # print(regEx)
  return(regEx)
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
  # regEx <- paste0("^.*uterus-", MouseID, ".*\\.jpg$")
  regEx <- paste0("^.*uterus-.*", MouseID, ".*\\.jpg$") ## AGG - Changed now that mouseID is numeric
  return(regEx)
}

regExUterinePicFileName_old <- function(
  MouseID
){
  regEx <- paste0("^.*uterus-", MouseID, ".*\\.jpg$")
  # regEx <- paste0("^.*uterus-.*", MouseID, ".*\\.jpg$") ## AGG - Changed now that mouseID is numeric
  return(regEx)
}

# Get Paths for Data Frame ------------------------------------------------

addRegExForSamplingDF <- function(
  samplingDF,
  arrangeByCycle = FALSE,
  arrangeByLH = FALSE,
  arrangeByTrt = FALSE,
  numIDVar = num_ID
){
  df <- samplingDF %>%
    # select(
    #   mouseID,
    #   num_ID,
    #   Sac_date,
    #   cohort,
    #   comboTrt,
    #   Sac_cycle,
    #   cyclingFolderPath,
    #   ReproTract_mass,
    #   maxLH,
    #   AgeInDays,
    #   cycleStartDate
    # ) %>%
    arrange(
      if(arrangeByCycle) Sac_cycle,
      if(arrangeByTrt) comboTrt,
      if(arrangeByLH) maxLH,
      ReproTract_mass
    ) %>%
    mutate(
      endCycleDay_pres = AgeInDays,
      startCycleDay_pres = ifelse(endCycleDay_pres - 20 > 69, endCycleDay_pres - 20, 70),
      # amRegEx = regExCycleFileName(Sac_date, num_ID),
      amRegEx = regExCycleFileName(Sac_date, {{numIDVar}}),
      # ayerRegEx = regExCycleFileName(Sac_date - 1, num_ID),
      ayerRegEx = regExCycleFileName(Sac_date - 1, {{numIDVar}}),
      # anteAyerRegEx = regExCycleFileName(Sac_date - 2, num_ID),
      anteAyerRegEx = regExCycleFileName(Sac_date - 2, {{numIDVar}}),
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
  searchSubFolders = TRUE,
  date = NULL
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
    # print(paste0("no file found ", MouseID, " ", type, "date ", date))
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
  CohortCyclingFolderPath,
  mouseFolderName
){
  cycleImgPaths <- dir_ls(
    path = file.path(CohortCyclingFolderPath, mouseFolderName),
    all = FALSE,
    recurse = FALSE,
    type = "file",
    glob = "*.jpg",
  )
  return(cycleImgPaths)
}

getAllMouseCycleImgs <- function(
  folderPath,
  cycleNum,
  searchSubFolders = TRUE
){
  regEx <- regExMouseAllFiles(cycleNum)
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
  return(foundPaths)
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
  CohortCyclingFolderPath,
  cyclePPT,
  numPerSlide = 12
){
  cyclingImgPaths <- getMouseCycleImgPaths(
    CohortCyclingFolderPath = CohortCyclingFolderPath, 
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

addMouseImgsToCyclePPT <- function(
  folderPath,
  cycleNum,
  cyclePPT,
  numPerSlide = 12
){
  # print(paste("mouse", cycleNum))
  cyclingImgPaths <- getAllMouseCycleImgs(folderPath, cycleNum)
  # print(paste("path", cyclingImgPaths))
  cyclePPT <- addImgsToCyclePPT(
    cycleImgPaths = cyclingImgPaths,
    sectionLabel = paste("Mouse", cycleNum),
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
  startCycleDay_pres,
  endCycleDay_pres,
  samplingPPT,
  cyclingDF_long,
  slideVersion = 2
){
  # print("in createSamplingSlide")
  print(paste0(MouseID))
  samplingPPT <- add_slide(samplingPPT, layout = paste0("samplingSlide", slideVersion))
  # print(amImgPath)
  
  mouseLabel <- paste0(MouseID, " - ", cycleID)
  maxLHLabel <- ifelse(!is.na(maxLH), paste0(maxLH, " ng/mL - ", trt), "")
  uterineMassLabel <- ifelse(!is.na(uterineMass), paste0(uterineMass, " mg"), "")
  
  # print(paste("Mouse:", MouseID, "Start Day:", startCycleDay_pres, "End Day:", endCycleDay_pres))
  # print(cyclingDF_long$mouseID)
  cyclingPlot <- cyclingDF_long %>%
    filter(
      mouseID == as.character(MouseID),
      PND <= endCycleDay_pres & PND >= startCycleDay_pres
    ) %>%
    plotCycleTraces_single(
      day = PND
    )
  
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
  slideVersion = 2,
  trtVar = comboTrt
){
  print("addSamplingSlidesFromDF")
  cyclingDF_long <- cyclingDF %>%
    makeCyclesLong() %>%
    addPNDForCyles()
  
  # print(paste("maxLH", samplingDF$maxLH))
  # print(paste("cyclingDF", cyclingDF$mouseID))
  # print(paste("numID/cycleID", samplingDF$num_ID))
  # print(paste("cyclingLong", cyclingDF_long$mouseID))
  
  pwalk(
    list(
      MouseID = samplingDF$mouseID,
      cycleID = samplingDF$num_ID,
      maxLH = samplingDF$maxLH,
      uterineMass = samplingDF$ReproTract_mass,
      # trt = samplingDF %>% select({{trtVar}}), ## this doesn't work for whatever reason - gives the whole list of them, not just the one, leads to rep of first trt
      trt = samplingDF$comboTrt,
      amImgPath = samplingDF$AMPath,
      prevDayImgPath = samplingDF$ayerPath,
      twoDayPrevImgPath = samplingDF$anteAyerPath,
      uterinePicPath = samplingDF$uterinePicPath,
      startCycleDay_pres = samplingDF$startCycleDay_pres,
      endCycleDay_pres = samplingDF$endCycleDay_pres
    ),
    createSamplingSlide,
    samplingPPT = samplingPPT,
    cyclingDF_long = cyclingDF_long,
    slideVersion = slideVersion
  )
}
