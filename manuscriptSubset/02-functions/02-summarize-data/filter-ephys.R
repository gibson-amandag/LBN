filterByPassives <- function(
  df
  , filterByRseries = TRUE
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
){
  if(filterByRseries){
    df <- df %>%
      filter(
        Rseries <= RseriesMax & Rseries >= RseriesMin
      )
  }
  if(filterByRinput){
    df <- df %>%
      filter(
        Rinput <= RinputMax & Rinput >= RinputMin
      )
  }
  if(filterByHoldingCurr){
    df <- df %>%
      filter(
        holdingCurrent <= holdingCurrMax & holdingCurrent >= holdingCurrtMin
      )
  }
  if(filterByCapacitance){
    df <- df %>%
      filter(
        capacitance <= capacitanceMax & capacitance >= capacitanceMin
      )
  }
  return(df)
}

filterByCellNum <- function(
  df
  , filterByNumCells = FALSE
  , maxCellNum = 3
){
  if(filterByNumCells){
    df <- df %>%
      group_by(
        mouseID
      ) %>%
      mutate(
        cellNum = 1:n()
      ) %>%
      ungroup(
        mouseID
      )
    
    df <- df %>%
      filter(
        cellNum <= maxCellNum
      )
  }
  return(df)
}

filterByTime <- function(
    df
    , filterBySacHr = TRUE
    , maxSacHr = 6 # time since sacrifice
    , filterByRecHr = FALSE
    , recHrMin = 13 # time since lights on
    , recHrMax = 20
){
  if(filterBySacHr){
    df <- df %>%
      filter(
        timeSinceSac <= maxSacHr
      )
  }
  if(filterByRecHr){
    df <- df %>%
      filter(
        recHr <= recHrMax & recHr >= recHrMin
      )
  }
  return(df)
}

filterByInclude <- function(
  df
  , removeNoToInclude = TRUE
){
  if(removeNoToInclude){
    df <- df %>%
      filter(
        incCell != "no" | is.na(incCell)
      )
  }
  return(df)
}

filterByExclude <- function(
  df
  , removeExclude = FALSE
){
  if(removeExclude){
    df <- df %>%
      filter(
        exclude == FALSE | is.na(exclude)
      )
  }
  return(df)
}
