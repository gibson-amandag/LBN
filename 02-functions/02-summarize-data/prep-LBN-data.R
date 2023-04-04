filterLBN <- function(
    cohorts = c(2, 4, 6, 7, 8, 9)
    , minSize = 5
    , exclude9011 = TRUE # malocclusion 
    , sizeVar = Litter_size_startPara
) {
  filterFunc <- function(
    df
    ){
    df <- df %>%
      filter(
        {{ sizeVar }} >= minSize
        , cohort %in% cohorts
        , is.na(Pups_through_wean) | Pups_through_wean == TRUE
      )
    if(exclude9011){
      df <- df %>%
        filter(
          across(
            # will remove row where mouseID == 9011, if mouseID column exists in DF
            any_of("mouseID"), ~ .x != 9011
          )
        )
    }
    return(df)
  }
  return(filterFunc)
}

filterDamBehaviorTimeFunc <- function(
    behaviorTimes
) {
  filterFunc <- function(
    df
  ) {
    df <- df %>%
      filter(
        ZT %in% behaviorTimes
      )
    return(df)
  }
  return(filterFunc)
}


filterCortFunc <- function(
  incSex #c() - can use, but don't have to if just one
  , stages #c()
  , filterUterineMass = TRUE
  , proUterineMin = 125
  , diUterineMax = 100
  , exclude653 = TRUE
  , adjMaxMin = TRUE
  , cortMax = 500
  , cortMin = 1.95
){
  filterFunc <- function(df){
    df <- df %>%
      filter(
        !is.na(cort)
        , ! exclude
      ) 
    
    if(adjMaxMin){
      df <- df %>%
        mutate(
          adjMax = ifelse(cort>=cortMax, TRUE, FALSE)
          , adjMin = ifelse(cort <= cortMin, TRUE, FALSE)
          , cort = ifelse(
              cort >= cortMax
              , cortMax
              , ifelse(cort <= cortMin, cortMin, cort)
            )
        )
    }
    
    df <- df %>%
      filter(
        sex %in% incSex
        , (sex == "M" | Sac_cycle %in% stages)
      )
    
    if(filterUterineMass){
      df <- df %>%
        filter(
          (sex == "M" | 
             Sac_cycle == "proestrus" & ReproTract_mass >= proUterineMin |
             Sac_cycle == "diestrus" & ReproTract_mass <= diUterineMax
           )
          
        )
    }
    
    if(exclude653){
      df <- df %>%
        filter(
          mouseID != 653
        )
    }
    
    return(df)
  }
  return(filterFunc)
}

filterLHFunc <- function(
  stages #c()
  , filterUterineMass = TRUE
  , proUterineMin = 125
  , diUterineMax = 100
  , exclude653 = TRUE
){
  filterFunc <- function(df){
    df <- df %>%
      filter(
        !is.na(LH)
      ) 
    
    
    df <- df %>%
      filter(
        sex == "F"
        , Sac_cycle %in% stages
      )
    
    if(filterUterineMass){
      df <- df %>%
        filter(
          (  Sac_cycle == "proestrus" & ReproTract_mass >= proUterineMin |
             Sac_cycle == "diestrus" & ReproTract_mass <= diUterineMax
          )
        )
    }
    
    if(exclude653){
      df <- df %>%
        filter(
          mouseID != 653
        )
    }
    
    return(df)
  }
  return(filterFunc)
}

filterAcuteStressFunc <- function(
    incSex #c() - can use, but don't have to if just one
    , stages #c()
    , filterUterineMass = TRUE
    , proUterineMin = 125
    , diUterineMax = 100
    , exclude653 = TRUE
){
  filterFunc <- function(df){
    df <- df %>%
      filter(
        sex %in% incSex
        , (sex == "M" | Sac_cycle %in% stages)
      )
    
    if(filterUterineMass){
      df <- df %>%
        filter(
          (sex == "M" | 
             Sac_cycle == "proestrus" & ReproTract_mass >= proUterineMin |
             Sac_cycle == "diestrus" & ReproTract_mass <= diUterineMax
          )
          
        )
    }
    
    if(exclude653){
      df <- df %>%
        filter(
          mouseID != 653
        )
    }
    
    return(df)
  }
  return(filterFunc)
}


# GABA PSCs ---------------------------------------------------------------



filterEphysFunc <- function(
    filterByRseries = TRUE
    , RseriesMin = 0
    , RseriesMax = 20
    , filterByRinput = TRUE
    , RinputMin = 500
    , RinputMax = 1500
    , filterByHoldingCurr = FALSE
    , holdingCurrMin = -50
    , holdingCurrMax = 10
    , filterByCapacitance = TRUE
    , capacitanceMin = 5
    , capacitanceMax = 20
    , filterByNumCells = FALSE
    , maxCellNum = 3
    , filterBySacHr = TRUE
    , maxSacHr = 6 # time since sacrifice
    , filterByRecHr = FALSE
    , recHrMin = 13 # time since lights on
    , recHrMax = 20
    , removeNoToInclude = TRUE
    , removeExclude = FALSE
){
  filterFunc <- function(df){
    df <- df %>%
      filterByPassives(
        filterByRseries = filterByRseries
        , RseriesMin = RseriesMin
        , RseriesMax = RseriesMax
        , filterByRinput = filterByRinput
        , RinputMin = RinputMin
        , RinputMax = RinputMax
        , filterByHoldingCurr = filterByHoldingCurr
        , holdingCurrMin =holdingCurrMin
        , holdingCurrMax = holdingCurrMax
        , filterByCapacitance = filterByCapacitance
        , capacitanceMin = capacitanceMin
        , capacitanceMax = capacitanceMax
      ) %>%
      filterByCellNum(
        filterByNumCells = filterByNumCells
        , maxCellNum = maxCellNum
      ) %>%
      filterByTime(
        filterBySacHr = filterBySacHr
        , maxSacHr =  maxSacHr# time since sacrifice
        , filterByRecHr = filterByRecHr
        , recHrMin =  recHrMin# time since lights on
        , recHrMax = recHrMax
      ) %>%
      filterByInclude(
        removeNoToInclude = removeNoToInclude
      ) %>%
      filterByExclude(
        removeExclude = removeExclude
      )
    return(df)
  }
  return(filterFunc)
}

getComboTrtFResults <- function(df){
  earlyLifeF <- getFText(df, "earlyLifeTrt")
  adultF <- getFText(df, "adultTrt")
  earlyLifeAdultF <- getFText(df, "earlyLifeTrt:adultTrt")
  
  paragraph <- paste0(
    "Early-life trt: "
    , earlyLifeF
    # , "\n"
    , "; "
    , "adult trt: "
    , adultF
    # , "\n"
    , "; "
    , "early-life x adult trt: "
    , earlyLifeAdultF
  )
  
  return(
    list(
      earlyLifeF = earlyLifeF
      , adultF = adultF
      , earlyLifeAdultF = earlyLifeAdultF
      , paragraph = paragraph
    )
  )
}

getTrtLitterFResults <- function(df, sepText = "\n"){
  earlyLifeF <- getFText(df, "earlyLifeTrt")
  litterF <- getFText(df, "litterNum")
  earlyLifeLitterF <- getFText(df, "earlyLifeTrt:litterNum")
  
  paragraph <- paste0(
    "Early-life trt: "
    , earlyLifeF
    , sepText
    , "experience: "
    , litterF
    , sepText
    , "early-life x experience: "
    , earlyLifeLitterF
  )
  
  return(
    list(
      earlyLifeF = earlyLifeF
      , litterF = litterF
      , earlyLifeLitterF = earlyLifeLitterF
      , paragraph = paragraph
    )
  )
}

getTrtFResults <- function(df, sepText = "\n"){
  earlyLifeF <- getFText(df, "earlyLifeTrt")
  
  paragraph <- paste0(
    "Early-life trt: "
    , earlyLifeF
    , sepText
  )
  
  return(
    list(
      earlyLifeF = earlyLifeF
      , paragraph = paragraph
    )
  )
}

getMale3WayFResults <- function(df, sepText = "\n"){
  earlyLifeAdultTimeF <- getFText(df, "earlyLifeTrt:adultTrt:time")
  adultTrtTimeF <- getFText(df, "adultTrt:time")
  earlyLifeTimeF <- getFText(df, "earlyLifeTrt:time")
  
  paragraph <- paste0(
    "Early-life x adult x time: "
    , earlyLifeAdultTimeF
    , sepText
    , "adult x time: "
    , adultTrtTimeF
    , sepText
    , "early-life x time: "
    , earlyLifeTimeF
  )
  
  return(
    list(
      earlyLifeAdultTimeF = earlyLifeAdultTimeF
      , adultTrtTimeF = adultTrtTimeF
      , earlyLifeTimeF = earlyLifeTimeF
      , paragraph = paragraph
    )
  )
}
