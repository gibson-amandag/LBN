# AD - amp, decay, etc --------------

allEventsRes <- runFourWayAD_allProps(pscProps)

## Pairwise -----------

allEventsDistTestPs_amplitude <- dist_pairwise(allEventsRes, "amplitude")
allEventsDistTestPs_riseTime <- dist_pairwise(allEventsRes, "riseTime")
allEventsDistTestPs_decayTime <- dist_pairwise(allEventsRes, "decayTime")
allEventsDistTestPs_fwhm <- dist_pairwise(allEventsRes, "fwhm")



# BS - amp, decay, etc ------------
bootstrapRes_iterations5000_maxPerCellNone <- pscProps %>%
  select(
    cellID
    , earlyLifeTrt
    , adultTrt
    , amplitude
    , riseTime
    , decay9010
    , fwhm
  ) %>%
  analyzeBootstrapResults(
    nBootstrap = 5000
    , maxPerCell = NULL
    , propDifferent = 0.15
    , setSeed = 123
    , CIprop = 0.05
  )

# AD - interval ------------
allEventsRes_int <- pscAllEvents_filtered %>%
  select(
    cellID
    , earlyLifeTrt
    , adultTrt
    , interval
  ) %>%
  filter(
    !is.na(interval)
  ) %>%
  fourWayAD(
    interval
    , "interval (s)"
    , zoom_x = TRUE
    , xmin = 0
    , xmax = 20
  )

## Pairwise -----------
allEventsDistTestPs_int <- dist_pairwise(allEventsRes_int, "interval", isInt = TRUE)

allEventsDistTestPs <- bind_rows(list(
  "amplitude" = allEventsDistTestPs_amplitude$joinedRes
  , "riseTime" = allEventsDistTestPs_riseTime$joinedRes
  , "decay9010" = allEventsDistTestPs_decayTime$joinedRes
  , "fwhm" = allEventsDistTestPs_fwhm$joinedRes
  , "interval" = allEventsDistTestPs_int$joinedRes
), .id = "variable")

# BS - interval ------------
bootstrapRes_iterations5000_maxPerCellNone_int <- pscAllEvents_filtered %>%
  select(
    cellID
    , earlyLifeTrt
    , adultTrt
    , interval
  ) %>%
  filter(
    !is.na(interval)
  ) %>%
  analyzeBootstrapResults(
    nBootstrap = 5000
    , maxPerCell = NULL
    , propDifferent = 0.15
    , setSeed = 123
    , CIprop = 0.05
    , forInterval = TRUE
  )

bootstrapRes_iterations5000_maxPerCellNone_int$errorPlots$interval
