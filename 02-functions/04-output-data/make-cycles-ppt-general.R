
# File Names --------------------------------------------------------------

#' buildMouseRegEx_allFiles
#'
#' @param cycleID Numeric cycle ID. looks for this number at the end of the file name
#' @param excludePM If TRUE, don't include file names that contain PM
#' @param excludeUterus if TRUE, don't include file names that contain uterus
#'
#' @return regular expression string looking for a jpg file to match cycle id
#' xxxxxx-####.jpg
#' This regex will find all files for the mouse, not just a particular day
#'
#' @examples
buildMouseRegEx_allFiles <- function(
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

#' buildRegExForDate
#'
#' @param date 
#' @param cycleID 
#' @param excludePM 
#' @param excludeUterus 
#'
#' @return regular expression string to find a cycling image for specified
#' mouse on the specified date
#' Looking for yyyy-mm-dd...-####.jpg
#' Underscores are also okay
#'
#' @examples
buildRegExForDate <- function(
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
    "^", # looks for the whole file path when matching, not just file name
    if(excludePM)"(?!.*PM)",
    if(excludeUterus)"(?!.*uterus)",
    ".*/",
    yyyy,
    "[-_]",
    mm,
    "[-_]",
    dd, 
    ".*[-_]", 
    "*",
    idText, "\\.jpg$")
  # print(regEx)
  return(regEx)
}


#' buildRegExUterinePicFileName
#'
#' @param MouseID 
#'
#' @return regular expression for a file that contains "uterus-" and the 
#' mouseID somewhere in the file, and ends in jpg
#' 
buildRegExUterinePicFileName <- function(
  MouseID
){
  idText <- sprintf("%04d", as.integer(MouseID))
  regEx <- paste0(
    "^", # looks for the whole file path when matching, not just file name
    ".*/",
    "uterus",
    "[-_]",
    ".*",
    idText, "\\.jpg$")
  # print(regEx)
  return(regEx)
}

# Get Paths for Data Frame ------------------------------------------------


#' Title
#'
#' @param samplingDF 
#' @param arrangeByCycle 
#' @param arrangeByLH 
#' @param numIDVar 
#'
#' @return
#' @export
#' 
#' Requires AgeInDays, Sac_date
#'
#' @examples
addImgRegExColsForSamplingDF <- function(
  samplingDF,
  arrangeByCycle = FALSE,
  arrangeByLH = FALSE,
  numIDVar = num_ID,
  maxPrevDays = 20,
  minCycleAge = 70,
  dateVar = Sac_date,
  ageVar = AgeInDays,
  includeUterus = TRUE,
  includeNextDay = FALSE,
  arrangeByDate = FALSE
){
  if(includeNextDay){
    addDays = 1
  } else {
    addDays = 0
  }
  df <- samplingDF %>%
    arrange(
      if(arrangeByLH) maxLH,
      if(arrangeByCycle) Sac_cycle,
      if(arrangeByDate) {{ dateVar }},
      ReproTract_mass
    ) %>%
    mutate(
      endCycleDay_pres = {{ ageVar }} + addDays,
      startCycleDay_pres = ifelse(
        .data$endCycleDay_pres - (.env$maxPrevDays + addDays) > .env$minCycleAge - 1, 
        .data$endCycleDay_pres - (.env$maxPrevDays + addDays), 
        .env$minCycleAge # If haven't reached maximum previous days, start at min cycle age
        ),
      amRegEx = buildRegExForDate({{ dateVar }}, {{numIDVar}}),
      ayerRegEx = buildRegExForDate({{ dateVar }} - 1, {{numIDVar}}),
      anteAyerRegEx = buildRegExForDate({{ dateVar }} - 2, {{numIDVar}}),
    )
  if(includeUterus){
    df <- df %>%
      mutate(
        uterinePicRegEx = buildRegExUterinePicFileName({{numIDVar}})
      )
  }
  if(includeNextDay){
    df <- df %>%
      mutate(
        nextDayRegEx = buildRegExForDate({{ dateVar }} + 1, {{ numIDVar }})
      )
  }
  return(df)
}

#' addImgRegExCols_forDate
addImgRegExCols_forDate <- function(
  samplingDF,
  numIDVar = cycleID,
  maxPrevDays = 20,
  minCycleAge = 70,
  date = Sys.Date(), ## default to today
  DOBVar = DOB,
  includeUterus = TRUE
){
  df <- samplingDF %>%
    mutate(
      thisDate = .env$date
    ) %>%
    calcAgeInDays(
      {{ DOBVar }},
      .data$thisDate
    )%>%
    mutate(
      endCycleDay_pres = AgeInDays,
      startCycleDay_pres = ifelse(
        .data$endCycleDay_pres - .env$maxPrevDays > .env$minCycleAge - 1, 
        .data$endCycleDay_pres - .env$maxPrevDays, 
        .env$minCycleAge # If haven't reached maximum previous days, start at min cycle age
        ),
      amRegEx = buildRegExForDate(.data$thisDate, {{numIDVar}}),
      ayerRegEx = buildRegExForDate(.data$thisDate - 1, {{numIDVar}}),
      anteAyerRegEx = buildRegExForDate(.data$thisDate - 2, {{numIDVar}}),
    )
  if(includeUterus){
    df <- df %>%
      mutate(
        uterinePicRegEx = buildRegExUterinePicFileName({{numIDVar}})
      )
  }
  return(df)
}

addGenSamplingImgFilePaths <- function(
  samplingDF_withRegEx
  , includeUterus = TRUE
  , includeNextDay = FALSE
){
  df <- samplingDF_withRegEx %>%
    rowwise() %>%
    mutate(
      AMPath = findMatchingFile(.data$cyclingFolderPath, .data$amRegEx, .data$mouseID, "AM pic"),
      ayerPath = findMatchingFile(.data$cyclingFolderPath, .data$ayerRegEx, .data$mouseID, "yesterday pic"),
      anteAyerPath = findMatchingFile(.data$cyclingFolderPath, .data$anteAyerRegEx, .data$mouseID, "2 days before pic"),
    )
  if(includeUterus){
    df <- df %>%
      mutate(
        uterinePicPath = findMatchingFile(.data$cyclingFolderPath, .data$uterinePicRegEx, .data$mouseID, "uterus pic")
      )
  }
  if(includeNextDay){
    df <- df %>%
      mutate(
        nextDayPath = findMatchingFile(.data$cyclingFolderPath, .data$nextDayRegEx, .data$mouseID, " next day pic"),
      )
  }
  return(df)
}

# Search Directory ---------------------------------------------------------------

# getMouseCycleImgPaths <- function(
#   CohortCyclingFolderPath,
#   mouseFolderName
# ){
#   cycleImgPaths <- dir_ls(
#     path = file.path(CohortCyclingFolderPath, mouseFolderName),
#     all = FALSE,
#     recurse = FALSE,
#     type = "file",
#     glob = "*.jpg",
#   )
#   return(cycleImgPaths)
# }

# getAllMouseCycleImgs <- function(
#   folderPath,
#   cycleNum,
#   searchSubFolders = TRUE
# ){
#   regEx <- buildMouseRegEx_allFiles(cycleNum)
#   foundPaths <- dir_ls(
#     path = folderPath,
#     all = TRUE,
#     recurse = searchSubFolders,
#     type = "any",
#     regexp = regEx,
#     invert = FALSE,
#     fail = TRUE,
#     perl = T
#   )
#   return(foundPaths)
# }


# Add to PPT --------------------------------------------------------------


# addImgsToCyclePPT <- function(
#   cycleImgPaths,
#   sectionLabel,
#   cyclePPT,
#   numPerSlide = 12 # 9 or 12
# ) {
#   # Number of images within directory
#   numImgs <- length(cycleImgPaths)
#   
#   # Add a section header and add section label
#   cyclePPT <- add_slide(x = cyclePPT, layout = "Section Header")
#   cyclePPT <- ph_with(x = cyclePPT, value = sectionLabel, location = ph_location_type("title"))
#   
#   # First index
#   iImg <- 1
#   for(path in cycleImgPaths){
#     fileName <- path %>% path_file() %>% path_ext_remove() # get the file name w/o path or extension
#     img <- external_img(path, width = 2.68, heigh = 2.14) # get the image
#     textID <- paste0("text", iImg)
#     imgID <- paste0("img", iImg)
#     
#     # If img index is 1, add a new slide
#     if(iImg == 1){
#       slideLayout <- paste0("estrousCycle", numPerSlide)
#       cyclePPT <- add_slide(x = cyclePPT, layout = slideLayout)
#     }
#     
#     # add the title
#     cyclePPT <- ph_with(
#       x = cyclePPT, value = fileName,
#       location = ph_location_label(
#         textID
#       ),
#       use_loc_size = TRUE)
#     
#     # add the image
#     cyclePPT <- ph_with(
#       x = cyclePPT, value = img,
#       location = ph_location_label(
#         imgID
#       ),
#       use_loc_size = TRUE)
#     
#     # if index is less than the number per slide, add one, otherwise, restart at 1
#     if(iImg < numPerSlide) {
#       iImg <- iImg + 1
#     } else {
#       iImg <- 1
#     }
#   }
#   return(cyclePPT)
# }

# addMouseFolderImgsTocyclePPT <- function(
#   mouseFolder,
#   CohortCyclingFolderPath,
#   cyclePPT,
#   numPerSlide = 12
# ){
#   cyclingImgPaths <- getMouseCycleImgPaths(
#     CohortCyclingFolderPath = CohortCyclingFolderPath, 
#     mouseFolderName = mouseFolder
#   )
#   cyclePPT <- addImgsToCyclePPT(
#     cycleImgPaths = cyclingImgPaths,
#     sectionLabel = mouseFolder,
#     cyclePPT = cyclePPT,
#     numPerSlide = numPerSlide
#   )
#   return(cyclePPT)
# }

# addMouseImgsToCyclePPT <- function(
#   folderPath,
#   cycleNum,
#   cyclePPT,
#   numPerSlide = 12
# ){
#   # print(paste("mouse", cycleNum))
#   cyclingImgPaths <- getAllMouseCycleImgs(folderPath, cycleNum)
#   # print(paste("path", cyclingImgPaths))
#   cyclePPT <- addImgsToCyclePPT(
#     cycleImgPaths = cyclingImgPaths,
#     sectionLabel = paste("Mouse", cycleNum),
#     cyclePPT = cyclePPT,
#     numPerSlide = numPerSlide
#   )
#   return(cyclePPT)
# }


# Sampling Slide ----------------------------------------------------------

createSamplingSlideGen <- function(
  MouseID,
  cycleID,
  maxLH = NULL,
  uterineMass,
  trt = NULL,
  amImgPath,
  prevDayImgPath,
  twoDayPrevImgPath,
  nextDayImgPath,
  uterinePicPath,
  startCycleDay_pres,
  endCycleDay_pres,
  samplingPPT,
  cyclingDF_long,
  slideVersion = 2,
  cycleLineVar = cohort,
  includeUterus = TRUE,
  includeNextDay = FALSE,
  includeNotes = FALSE,
  notes = NULL
){
  # print("in createSamplingSlideGen")
  print(paste0(MouseID))
  samplingPPT <- add_slide(samplingPPT, layout = paste0("samplingSlide", slideVersion))
  # print(amImgPath)
  
  mouseLabel <- paste0(MouseID, " - ", cycleID)
  maxLHLabel <- ifelse(!is.na(maxLH), paste0(maxLH, " ng/mL - ", trt), "")
  if(includeUterus){
    uterineMassLabel <- ifelse(!is.na(uterineMass), paste0(uterineMass, " mg"), "")
  }
  
  # print(paste("Mouse:", MouseID, "Start Day:", startCycleDay_pres, "End Day:", endCycleDay_pres))
  # print(cyclingDF_long$mouseID)
  cyclingPlot <- cyclingDF_long %>%
    filter(
      mouseID == as.character(MouseID),
      PND <= endCycleDay_pres & PND >= startCycleDay_pres
    ) %>%
    plotCycleTraces_single(
      day = PND,
      lineColorVar = {{ cycleLineVar }}
    ) + coord_cartesian(xlim = c(startCycleDay_pres, endCycleDay_pres))
  
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
  if(includeUterus){
    samplingPPT <- ph_with(
      x = samplingPPT,
      value = uterineMassLabel,
      location = ph_location_label(
        "uterineMass"
      ),
      use_loc_size = TRUE
    )
  }
  
  if(includeNotes){
    if(slideVersion == 3 | slideVersion == 4){
      samplingPPT <- ph_with(
        x = samplingPPT,
        value = notes,
        location = ph_location_label(
          "notes"
        ), 
        use_loc_size = TRUE
      )
    }
  }
  
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
  
  if(includeUterus & !is.na(uterinePicPath) & slideVersion == 2){
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
  if(includeNextDay & !is.na(nextDayImgPath) & (slideVersion == 3 || slideVersion == 4)){
    nextDayImg <- external_img(nextDayImgPath, width = 2.68, height = 2.14)
    samplingPPT <- ph_with(
      x = samplingPPT,
      value = nextDayImg,
      location = ph_location_label(
        "imgNext"
      ),
      use_loc_size = TRUE
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

addSamplingSlidesFromDFGen <- function(
  samplingDF,
  cyclingDF,
  samplingPPT,
  slideVersion = 2,
  trtVar = comboTrt,
  includeUterus = TRUE,
  includeNextDay = FALSE,
  includeNotes = FALSE,
  notesVar = notes
){
  print("addSamplingSlidesFromDF")
  cyclingDF_long <- cyclingDF %>%
    makeCyclesLong(
      afterVar = cohort ## hard coded
    ) %>%
    addPNDForCyles()
  
  samplingDF <- samplingDF %>%
    mutate(
      samplingTrtVar = {{ trtVar }}
    )
  
  var <- enquo(notesVar)
  info <- getSymAndString(!! var)
  
  
  vars <- c(
    "mouseID",
    "cycleID",
    "maxLH",
    "ReproTract_mass",
    "samplingTrtVar",
    "AMPath",
    "ayerPath",
    "anteAyerPath",
    "uterinePicPath",
    "startCycleDay_pres",
    "endCycleDay_pres",
    "nextDayPath",
    info$string
    )
  
  for(var in vars){
    if(!var %in% names(samplingDF)){
      print(paste("adding", var, "to dataframe"))
      samplingDF <- samplingDF %>%
        mutate(
          "{var}" := NA
        )
    }
  }
  
  samplingDF <- samplingDF %>%
    mutate(
      thisNotesVar = {{ notesVar }}
    )
  
  
  pwalk(
    list(
      MouseID = samplingDF$mouseID,
      cycleID = samplingDF$cycleID,
      maxLH = samplingDF$maxLH,
      uterineMass = samplingDF$ReproTract_mass,
      # trt = samplingDF %>% select({{trtVar}}), ## this doesn't work for whatever reason - gives the whole list of them, not just the one, leads to rep of first trt
      ## try this .data[[var]] samplingDF %>% sampling
      trt = samplingDF$samplingTrtVar,
      amImgPath = samplingDF$AMPath,
      prevDayImgPath = samplingDF$ayerPath,
      twoDayPrevImgPath = samplingDF$anteAyerPath,
      uterinePicPath = samplingDF$uterinePicPath,
      startCycleDay_pres = samplingDF$startCycleDay_pres,
      endCycleDay_pres = samplingDF$endCycleDay_pres,
      nextDayImgPath = samplingDF$nextDayPath,
      notes = samplingDF$thisNotesVar
    ),
    createSamplingSlideGen,
    samplingPPT = samplingPPT,
    cyclingDF_long = cyclingDF_long,
    slideVersion = slideVersion,
    includeUterus = includeUterus,
    includeNextDay = includeNextDay,
    includeNotes = includeNotes
  )
}

createRemainingPPT <- function(
  df,
  cyclesDF = BD_cycles,
  thisDate = Sys.Date(),
  trtVar = comboTrt,
  useVar = Sac_date,
  outPath = file.path(reportOutputFolder, "remainingPPTs"),
  addToName = "",
  includeYesterday = FALSE
){
  if(includeYesterday){
    addDay <- 1
  } else {
    addDay <- 0
  }
  remainingDF <- df %>%
    filter(
      is.na({{ useVar }}) | {{ useVar }} >= (thisDate - addDay)
    ) %>%
    addImgRegExCols_forDate(
      date = thisDate
    ) %>%
    addGenSamplingImgFilePaths(
      includeUterus = FALSE
    ) %>%
    arrange(
      {{ useVar }}
    )
  
  samplingPPT <- read_pptx("./samplingSlideTemplate.pptx")
  samplingPPT <- add_slide(samplingPPT, layout = "Title Slide")
  samplingPPT <- ph_with(
    samplingPPT,
    value = paste("Mice remaining on", thisDate),
    location = ph_location_label("Title 1")
  )
  addSamplingSlidesFromDFGen(
    remainingDF,
    samplingPPT = samplingPPT,
    cyclingDF = cyclesDF %>%
      filter(
        mouseID %in% remainingDF$mouseID
      ),
    includeUterus = FALSE
  )

  print(samplingPPT, target = file.path(reportOutputFolder, "remainingPPTs", paste0("remaining_", addToName, "_", thisDate, ".pptx")))
  return(remainingDF)
}

createOvulationPPT <- function(
  df,
  cyclesDF = BD_cycles,
  trtVar = comboTrt,
  useVar = Sampling_date,
  outPath = file.path(reportOutputFolder, "samplingPPTs"),
  addToName = "",
  slideVersion = 4,
  sortByCycle = FALSE,
  sortByLH = TRUE,
  sortByDate = FALSE
){
  if(!"maxLH" %in% names(df)){
    df <- df %>%
      mutate(
        maxLH = NA
      )
  }
  samplingDF <- df %>%
    filter(
      !is.na({{ useVar }})
    ) %>%
    calcAgeInDays(
      ageAtDateVar = {{ useVar }}
    ) %>%
    addImgRegExColsForSamplingDF(
      arrangeByCycle = sortByCycle,
      arrangeByLH = sortByLH,
      numIDVar = cycleID,
      dateVar = {{ useVar }},
      ageVar = AgeInDays,
      includeUterus = FALSE,
      includeNextDay = TRUE
    ) %>%
    addGenSamplingImgFilePaths(
      includeUterus = FALSE
      , includeNextDay = TRUE
    ) %>%
    mutate(
      comboNotes = paste0(
        "trt: ", {{trtVar}},
        "\nsmear: ", Sac_stage, 
        ", uterus: ", Uterine_description, 
        "\noocytes 1: ", ifelse(!is.na(oocytes1), oocytes1, ""),
        ", oocytes 2: ", ifelse(!is.na(oocytes2), oocytes2, ""),
        "\nadditional notes: ", gsub("_x000D_", "", notes)
        )
    ) %>%
    arrange(
      - trust,
      {{ trtVar }},
      maxLH,
      if(sortByDate) {{ useVar }}
    )
  
  print(samplingDF %>% select(comboNotes))
  
  samplingPPT <- read_pptx("./samplingSlideTemplate.pptx")
  samplingPPT <- add_slide(samplingPPT, layout = "Title Slide")
  samplingPPT <- ph_with(
    samplingPPT,
    value = paste("Sampled mice as of", Sys.Date()),
    location = ph_location_label("Title 1")
  )
  addSamplingSlidesFromDFGen(
    samplingDF,
    samplingPPT = samplingPPT,
    cyclingDF = cyclesDF %>%
      filter(
        # mouseID %in% samplingDF$mouseID
      ),
    includeUterus = FALSE,
    includeNextDay = TRUE,
    slideVersion = slideVersion,
    includeNotes = TRUE,
    notesVar = comboNotes
  )

  print(samplingPPT, target = file.path(reportOutputFolder, "samplingPPTs", paste0("sampling_", addToName, "_", Sys.Date(), ".pptx")))
  return(samplingDF)
}
