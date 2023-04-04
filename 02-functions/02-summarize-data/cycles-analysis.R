countEstrousStageDays <- function(df, minDaysToSum = 10){
  df <- df %>%
    mutate(
      cycleDays = rowSums(
        !is.na(
          select(., starts_with("day"))
        )
      ),
      numE = rowSums(
        select(., starts_with("day")) == 1,
        na.rm = TRUE
      ),
      numD = rowSums(
        select(., starts_with("day")) == 2,
        na.rm = TRUE
      ),
      numP = rowSums(
        select(., starts_with("day")) == 3,
        na.rm = TRUE
      ),
      percE = ifelse(cycleDays >= minDaysToSum, numE / cycleDays * 100, NA),
      percD = ifelse(cycleDays >= minDaysToSum, numD / cycleDays * 100, NA),
      percP = ifelse(cycleDays >= minDaysToSum, numP / cycleDays * 100, NA),
      .after = sex
    )
  
  return(df)
}

addCycleStartCol <- function(
    cycleDF_long
){
  df <- cycleDF_long %>%
    arrange(
      mouseID
      , day
    ) %>%
    group_by(
      mouseID
    ) %>%
    mutate(
      isStartCycle = ifelse(
        # day != 1 & # don't count an E on first day - this should be taken care of by lag, but just to be safe
        #   day - 1 == lag(day) & # make sure that the previous row is actually the day before
        #   # lag(stage) %in% c(2, 3) &  # check if the day before was either diestrus or proestrus
        #   lag(stage) %in% c(3) &  # check if the day before was either diestrus or proestrus
        #   stage == 1 # check if today is estrus
        stage == 3 # if proestrus
        , TRUE
        , FALSE
      )
      , .after = stage
    ) %>%
    ungroup()
  return(df)
}

getCycleLength <- function(
    cycleDF_long
){
  df <- cycleDF_long %>%
    filter(
      isStartCycle == TRUE
    ) %>%
    arrange(
      mouseID
      , day
    ) %>%
    group_by(
      mouseID
    ) %>%
    mutate(
      cycleLength = day - lag(day)
    ) %>%
    ungroup()
  return(df)
}

summarizeCycles <- function(
    cyclesDF_long
    , daysAnalyzed = 21
){
  df <- cyclesDF_long %>%
    group_by(mouseID) %>%
    summarize(
      numCycles = n()
      , cycleLength = mean(cycleLength, na.rm = TRUE)
      , firstCycleDay = first(day)
      , lastCycleDay = last(day)
      , numDaysNotInCycles = ifelse(numCycles>0, daysAnalyzed - (lastCycleDay-firstCycleDay), daysAnalyzed)
      , percDaysInCycles = (daysAnalyzed-numDaysNotInCycles) / daysAnalyzed
      , .groups = "drop"
    )
  return(df)
}

getCyclesInfo <- function(
    cyclesDF_long
){
  df <- cyclesDF_long %>%
    addCycleStartCol() %>%
    getCycleLength() %>%
    summarizeCycles()
  return(df)
}