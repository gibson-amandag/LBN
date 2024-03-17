# Interevent interval ------------

## AD -------------

ADres_interval <- pscAllEvents_filtered %>%
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

### Pairwise AD -------------

ADres_pairwise_Ps_interval <- dist_pairwise(ADres_interval, "interval")

## Bootstrapping --------------


bootstrapRes_iterations5000_maxPerCellNone_interval <- pscAllEvents_filtered %>%
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
    , propColsAsText = c("interval")
  )

# bootstrapRes_iterations50_maxPerCellNone_interval <- pscAllEvents_filtered %>%
#   select(
#     cellID
#     , earlyLifeTrt
#     , adultTrt
#     , interval
#   ) %>%
#   filter(
#     !is.na(interval)
#   ) %>%
#   analyzeBootstrapResults(
#     nBootstrap = 50
#     , maxPerCell = NULL
#     , propDifferent = 0.15
#     , setSeed = 123
#     , CIprop = 0.05
#     , propColsAsText = c("interval")
#   )

# bootstrapRes_iterations500_maxPerCell20_interval <- pscAllEvents_filtered %>%
#   select(
#     cellID
#     , earlyLifeTrt
#     , adultTrt
#     , interval
#   ) %>%
#   filter(
#     !is.na(interval)
#   ) %>%
#   analyzeBootstrapResults(
#     nBootstrap = 500
#     , maxPerCell = 20
#     , propDifferent = 0.15
#     , setSeed = 123
#     , CIprop = 0.05
#     , propColsAsText = c("interval")
#     , makeBootPlots = TRUE
#   )

# Amplitude --------------------

## AD -----------------

ADres_amplitude <- pscAmplitude %>%
  select(
    cellID
    , earlyLifeTrt
    , adultTrt
    , amplitude
  ) %>%
  filter(
    !is.na(amplitude)
  ) %>%
  fourWayAD(
    amplitude
    , "amplitude (pA)"
    , zoom_x = TRUE
    , xmin = 0
    , xmax = 125
  )

### Pairwise AD -------------------
ADres_pairwise_Ps_amplitude <- dist_pairwise(ADres_amplitude, "amplitude")

### Bootstrapping -----------------

bootstrapRes_iterations5000_maxPerCellNone_amplitude <- pscAmplitude %>%
  select(
    cellID
    , earlyLifeTrt
    , adultTrt
    , amplitude
  ) %>%
  analyzeBootstrapResults(
    nBootstrap = 5000
    , maxPerCell = NULL
    , propDifferent = 0.15
    , setSeed = 123
    , CIprop = 0.05
    , propColsAsText = c("amplitude")
  )

# bootstrapRes_iterations5000_maxPerCell10_amplitude <- pscAmplitude %>%
#   select(
#     cellID
#     , earlyLifeTrt
#     , adultTrt
#     , amplitude
#   ) %>%
#   analyzeBootstrapResults(
#     nBootstrap = 5000
#     , maxPerCell = 10
#     , propDifferent = 0.15
#     , setSeed = 123
#     , CIprop = 0.05
#     , propColsAsText = c("amplitude")
#   )

# Combined pairwise -----------

AD_testPs <- bind_rows(list(
  "interval" = ADres_pairwise_Ps_interval$AD %>%
    select(
      -p
    ) %>%
    rename(
      p.AD = `holm adj p`
    )
  , "amplitude" = ADres_pairwise_Ps_amplitude$AD %>%
    select(
      -p
    ) %>%
    rename(
      p.AD = `holm adj p`
    )
  )
  , .id = "variable")

boot_testPs <- bind_rows(
  list(
    "interval" = bootstrapRes_iterations5000_maxPerCellNone_interval$P_formatted %>%
      select(
        -p
      ) %>%
      rename(
        p.boot = p.adj
      )
    , "amplitude" = bootstrapRes_iterations5000_maxPerCellNone_amplitude$P_formatted %>%
      select(
        -p
      ) %>%
      rename(
        p.boot = p.adj
      )
  )
)

psc_pairwisePs <- left_join(
  AD_testPs
  , boot_testPs
  , by = c("variable", "comp")
)

psc_pairwisePs_header <- data.frame(
  col_keys = colnames(psc_pairwisePs)
  , line1 = c(rep("", 2), rep("# PSCs", 2), rep("Anderson-Darling", 3), rep("bootstrap", 3))
  , line2 = c("variable", "comparison", "group 1", "group 2", "AD", "T AD", "p", "mean difference", "CI", "p")
  , stringsAsFactors = FALSE
)

psc_pairwisePs_flexTable <- psc_pairwisePs %>%
  makeManuscriptFlexTable(
    headerDF = psc_pairwisePs_header
    , vertLines = c(2, 4, 7)
    , horzLines = c(4)
    , vertMergeCols = c("variable")
    , round2Cols = c("meanDiff", "T.AD")
    , round1Cols = "AD"
  )
