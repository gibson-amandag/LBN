filterLBN <- function(
    cohorts = c(2, 4, 6, 7, 8, 9)
    , minSize = 5
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
        time %in% behaviorTimes
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
      )
    return(df)
  }
  return(filterFunc)
}
