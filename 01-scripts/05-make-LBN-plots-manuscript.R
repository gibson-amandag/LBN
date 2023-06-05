textSize <- 11
dotSize <- 2
facetMatByLitter <- FALSE

# Dam behavior ------------------------------------------------------------


figDamsB <- damBehavior_byPND %>%
  filter(
    !is.na(Num_exits)
  ) %>%
  plotDamBehavior(
    yVar = Num_exits
    , yLab = "# of exits"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , dotSize = 1
    # , facetByTrt = FALSE
  )



# Dam corticosterone ------------------------------------------------------

figDamsD <- damFiltered %>%
  filter(
    !is.na(Cort_dam_P11)
  ) %>%
  scatterPlotLBN(
    yVar = Cort_dam_P11,
    yLab = "corticosterone (ng/mL)",
    textSize = textSize,
    dotSize = dotSize,
    zoom_y = TRUE,
    ymin = 0,
    ymax = 120
  )

# Dam Mass ------------------------------------------------------


figDamsC <- damFiltered %>%
  plot_dam_mass_lines(
    useLineType = FALSE, # TRUE/FALSE
    lineTypeVar = earlyLifeTrt,
    lineGroupVar = damID,
    xtitle = "PND", #x axis label
    ytitle = "mass (g)", #y axis label
    title = NULL, # plot title
    individualLines = TRUE, # plot individual lines
    meanLines = TRUE, # plot mean lines with SE
    zoom_x = TRUE, # Zoom to part of x axis
    xmin = 0,
    xmax = 21,
    indivLineAlpha = .3,
    indivLineSize = 0.5,
    errorBarWidth = 0,
    meanLineSize = 1,
    meanAlpha = 1,
    errorBarSize = 1,
    # errorBarColor = "grey10",
    errorBarAlpha = 1,
    textSize = textSize,
    axisSize = 0.5,
    legendPosition = c(0.75, 0.2),
    STDColor = "#4D4D4D",
    LBNColor = "#008B8B"
  ) +
  theme(
    legend.key = element_rect(fill = NA)
  )

# Offspring mass ----------------------------------------------------------


## First litter ------------------------------------------------------------

figOffA <- massFiltered %>%
  plot_mass_lines(
    groupByDam = TRUE,
    facetBySex = TRUE,
    useLineType = FALSE,
    lineTypeVar = earlyLifeTrt,
    lineGroupVar = damID,
    xtitle = "postnatal day", #x axis label
    ytitle = "mass (g)", #y axis label
    title = NULL, # plot title
    individualLines = TRUE, # plot individual lines
    meanLines = TRUE, # plot mean lines with SE
    zoom_x = FALSE, # Zoom to part of x axis
    xmin = 0,
    xmax = 72,
    zoom_y = FALSE, # Zoom to part of y axis
    ymin = 0,
    ymax = 35,
    indivLineAlpha = .15,
    indivLineSize = 0.3,
    errorBarWidth = 0,
    meanLineSize = 0.5,
    meanAlpha = 1,
    errorBarSize = .5,
    # errorBarColor = "grey10",
    errorBarAlpha = 1,
    textSize = textSize,
    axisSize = 0.5,
    # legendPosition = "bottom",
    legendPosition = c(0.85, 0.2),
    STDColor = "#4D4D4D",
    LBNColor = "#008B8B"
  ) +
  theme(
    legend.key = element_rect(fill = NA)
  )

# Maturation --------------------------------------------------------------

matVals <- getMaxMatVals(maturation_byDam_f, maturation_byDam_m)

max_mass <- matVals$max_mass
max_age <- matVals$max_age

matVals_indiv <- getMaxMatVals(maturationFiltered, maturationFiltered)
indivMaxAge <- matVals_indiv$max_age

## Vaginal opening ---------------------------------------------------------

figOffB <- maturation_byDam_f %>%
  scatterPlotLBN(
    yVar = VO_age
    , "vaginal opening\nage (days)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = max_age
  )

figOffC <- maturation_byDam_f %>%
  scatterPlotLBN(
    yVar = VO_mass
    , "vaginal opening\nmass (g)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = max_mass
  )



## First estrus ------------------------------------------------------------


figOffD <- maturation_byDam_f %>%
  scatterPlotLBN(
    yVar = Estrus_age
    , "first estrus\nage (days)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = max_age
  )

figOffE <- maturation_byDam_f %>%
  scatterPlotLBN(
    yVar = Estrus_mass
    , "first estrus\nmass (g)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = max_mass
  )


## Preputial separation ----------------------------------------------------

figOffF <- maturation_byDam_m %>%
  scatterPlotLBN(
    yVar = PreputialSep_age
    , "preputial separation\nage (days)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = max_age
  )

figOffG <- maturation_byDam_m %>%
  scatterPlotLBN(
    yVar = PreputialSep_mass
    , "preputial separation\nmass (g)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = max_mass
  )


# Cycles ------------------------------------------------------------------


## Representative Plots ----------------------------------------------------

stdCycles <- cyclesFiltered %>%
  filter(
    earlyLifeTrt == "STD"
  )
lbnCycles <- cyclesFiltered %>%
  filter(
    earlyLifeTrt == "LBN"
  )
stdMice <- stdCycles[sample(nrow(stdCycles))[1:4],]$mouseID
lbnMice <- lbnCycles[sample(nrow(lbnCycles))[1:4],]$mouseID

figCyclesA <-  cyclesFiltered %>%
  filter(
    mouseID %in% stdMice | mouseID %in% lbnMice
  ) %>%
  arrange(
    earlyLifeTrt
  ) %>%
  mutate(
    mouseByRow = row_number()
  ) %>%
  makeCyclesLong() %>%
  addCycleStartCol() %>%
  addPNDForCyles() %>%
  plotCycleTraces(
    colorValues = c("grey30", "cyan4")
    , fontSize = textSize
    , removeFacets = TRUE
    , ncol = 4
    , nrow = 2
    , day = PND
    , breakSeq = seq(70, 90, 5)
    , MouseID = mouseByRow
    , facetDir = "v"
  ) + theme(
    panel.border = element_rect(color = "lightgrey", fill = NA)
  )

## cycle features -------------------------------------------------------------

figCyclesB <- cyclesFiltered %>%
  scatterPlotLBN(
    yVar = numCycles
    , yLab = "# of cycles from P70-90"
    , textSize = textSize
    , dotSize = dotSize
  )

figCyclesC <- cyclesFiltered %>%
  scatterPlotLBN(
    yVar = cycleLength
    , yLab = "cycle length (days)"
    , textSize = textSize
    , dotSize = dotSize
  )


## Percent days in stage ---------------------------------------------------

figCyclesD <- cyclesPercLong %>%
  plotCyclesPercent(
    fontSize = textSize
    , dotSize = dotSize
    , strip.position = "top"
  )

# ALPS --------------------------------------------------------------------


## Cort --------------------------------------------------------------------

plotCort_onlyLBN <- manuscriptCortPlotFunc(
  onlyLBN = TRUE 
  , fontSize = textSize
  , dotSize = dotSize
  , yUnitsNewLine = TRUE
  , jitterPosition = 1.8
  , wrapLegend = TRUE
  , useALPSLineType = FALSE
)

plotCort_onlyLBN_longYLab <- manuscriptCortPlotFunc(
  onlyLBN = TRUE 
  , fontSize = textSize
  , dotSize = dotSize
  , yUnitsNewLine = FALSE
  , jitterPosition = 1.8
  , wrapLegend = FALSE
  , meanWidth = 2.5
  , useALPSLineType = FALSE
)

plotCort_long <- manuscriptCortPlotFunc(
  fontSize = textSize
  , dotSize = dotSize
  , yUnitsNewLine = TRUE
  , jitterPosition = 1.8
)



figCort_opt1A <- cortFilteredMales %>%
  plotCort_onlyLBN()

figCort_opt2A <- cortFilteredMales %>%
  plotCort_long()

figCort_opt3A <- cortFilteredMales %>%
  plotCort_onlyLBN_longYLab()

figCort_opt1B <- cortFilteredDi %>%
  plotCort_onlyLBN()

figCort_opt2B <- cortFilteredDi %>%
  plotCort_long()

figCort_opt3B <- cortFilteredDi %>%
  plotCort_onlyLBN_longYLab()

figCort_opt1C <- cortFilteredPro %>%
  plotCort_onlyLBN()

figCort_opt2C <- cortFilteredPro %>%
  plotCort_long()

figCort_opt3C <- cortFilteredPro %>%
  plotCort_onlyLBN_longYLab()

## Uterine mass ------------------------------------------------------------

plotUterineMassByGroup_byStage_bothL <- plotUterineMassByGroupFunc(
  litterNums = c(1, 2)
  , proUterineMin = proUterineMin # defined in 04-filter-datasets
  , diUterineMax = diUterineMax
  , fontSize = textSize
  , dotSize = dotSize
  , facet = TRUE
  , facetInfo = facetForStage
)

uterineMassByGroup_byStage_bothL <- acuteStressFilteredFemales_all %>%
  plotUterineMassByGroup_byStage_bothL()

plotUterineMassByGroup_byStage_1stL <- plotUterineMassByGroupFunc(
  litterNums = c(1)
  , proUterineMin = proUterineMin # defined in 04-filter-datasets
  , diUterineMax = diUterineMax
  , fontSize = textSize
  , dotSize = dotSize
  , facet = TRUE
  , facetInfo = facetForStage
)

# only show the good ones for 1st and 2nd subsets
uterineMassByGroup_byStage_1stL <- acuteStressFilteredFemales %>%
  plotUterineMassByGroup_byStage_1stL()

plotUterineMassByGroup_byStage_2ndL <- plotUterineMassByGroupFunc(
  litterNums = c(2)
  , proUterineMin = proUterineMin # defined in 04-filter-datasets
  , diUterineMax = diUterineMax
  , fontSize = textSize
  , dotSize = dotSize
  , facet = TRUE
  , facetInfo = facetForStage
)

uterineMassByGroup_byStage_2ndL <- acuteStressFilteredFemales %>%
  plotUterineMassByGroup_byStage_2ndL()


## LH ----------------------------------------------------------------------

plotLH_bothL <- plotLHFunc(
  c(1, 2)
  , fontSize = textSize
  , dotSize = dotSize
  , ymax = 40
)

LHplot_pro_bothL <- LHFilteredPro %>%
  plotLH_bothL()

plotLH_bothL_zoom <- plotLHFunc(
  c(1, 2)
  , fontSize = textSize
  , dotSize = dotSize
  , ymax = 10
  , addSurgeLine = TRUE
  , surgeMin = surgeMin
)

LHplot_pro_bothL_zoom <- LHFilteredPro %>%
  plotLH_bothL_zoom()

plotLH_1stL <- plotLHFunc(
  c(1)
  , fontSize = textSize
  , dotSize = dotSize
  , ymax = 40
)

LHplot_pro_1stL <- LHFilteredPro %>%
  plotLH_1stL()

plotLH_1stL_zoom <- plotLHFunc(
  c(1)
  , fontSize = textSize
  , dotSize = dotSize
  , ymax = 10
  , addSurgeLine = TRUE
  , surgeMin = surgeMin
)

LHplot_pro_1stL_zoom <- LHFilteredPro %>%
  plotLH_1stL_zoom()

plotLH_2ndL <- plotLHFunc(
  c(2)
  , fontSize = textSize
  , dotSize = dotSize
  , ymax = 40
)

LHplot_pro_2ndL <- LHFilteredPro %>%
  plotLH_2ndL()

plotLH_2ndL_zoom <- plotLHFunc(
  c(2)
  , fontSize = textSize
  , dotSize = dotSize
  , ymax = 10
  , addSurgeLine = TRUE
  , surgeMin = surgeMin
)

LHplot_pro_2ndL_zoom <- LHFilteredPro %>%
  plotLH_2ndL_zoom()


## Surge amplitude ---------------------------------------------------------

plotSurgeAmp_bothL <- plotSurgeAmpFunc(
  c(1, 2)
  , surgeMin = surgeMin
  , fontSize = textSize
  , dotSize = dotSize
  , angleX = FALSE
  , addSurgeMinLine = TRUE
  , ymax = 40
)

plotSurgeAmp_1stL <- plotSurgeAmpFunc(
  c(1)
  , surgeMin = surgeMin
  , fontSize = textSize
  , dotSize = dotSize
  , angleX = FALSE
  , addSurgeMinLine = TRUE
  , ymax = 40
)

plotSurgeAmp_2ndL <- plotSurgeAmpFunc(
  c(2)
  , surgeMin = surgeMin
  , fontSize = textSize
  , dotSize = dotSize
  , angleX = FALSE
  , addSurgeMinLine = TRUE
  , ymax = 40
)

LHamp_bothL_plot <- surgedDF %>%
  plotSurgeAmp_bothL()

LHamp_1stL_plot <- surgedDF %>%
  plotSurgeAmp_1stL()

LHamp_2ndL_plot <- surgedDF %>%
  plotSurgeAmp_2ndL()



## % surged ----------------------------------------------------------------

plotPercSurged_bothL <- plotPercSurgedFunc(
  c(1, 2)
  , fontSize = textSize
  , labelFontSize = 10
)

percSurgedPlot_bothL <- surgedDF %>%
  plotPercSurged_bothL()

plotPercSurged_1stL <- plotPercSurgedFunc(
  c(1)
  , fontSize = textSize
  , labelFontSize = 10
)

percSurgedPlot_1stL <- surgedDF %>%
  plotPercSurged_1stL()

plotPercSurged_2ndL <- plotPercSurgedFunc(
  c(2)
  , fontSize = textSize
  , labelFontSize = 10
)

percSurgedPlot_2ndL <- surgedDF %>%
  plotPercSurged_2ndL()


# GABA PSCs ---------------------------------------------------------------

plotCapacitance <- plotCatVarFunc(
  expr(capacitance)
  , fontSize = textSize
  , dotSize = dotSize
)

plotRseries <- plotCatVarFunc(
  expr(Rseries)
  , fontSize = textSize
  , dotSize = dotSize
)

plotRinput <- plotCatVarFunc(
  expr(Rinput)
  , fontSize = textSize
  , dotSize = dotSize
)

plotHoldingCurr <- plotCatVarFunc(
  expr(holdingCurrent)
  , fontSize = textSize
  , dotSize = dotSize
)

plotGABAfreq <- plotCatVarFunc(
  expr(frequency)
  , fontSize = textSize
  , dotSize = dotSize
)

plotGABAamp <- plotCatVarFunc(
  expr(relPeak)
  , fontSize = textSize
  , dotSize = dotSize
)

figGABAa <- GABApscsFilteredFiring %>%
  plotCapacitance()
figGABAc <- GABApscsFilteredFiring %>%
  plotRseries()
figGABAb <- GABApscsFilteredFiring %>%
  plotRinput()
figGABAd <- GABApscsFilteredFiring %>%
  plotHoldingCurr()
figGABAe <- GABApscsFilteredFiring %>%
  plotGABAfreq()
figGABAf <- GABApscsFilteredFiring %>%
  plotGABAamp()



