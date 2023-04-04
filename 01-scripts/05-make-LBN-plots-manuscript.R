textSize <- 11
dotSize <- 2
facetMatByLitter <- FALSE

# Dam behavior ------------------------------------------------------------


damBehavior_byPND_plot <- damBehavior_byPND %>%
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
  )



# Dam corticosterone ------------------------------------------------------

damCort_plot <- damFiltered %>%
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


plotDamMass <- plotDamMass_func(
  c(1)
  , fontSize = textSize
  , indivLineSize = 0.5
)

damMass_plot <- damFiltered %>%
  plotDamMass()


# Offspring mass ----------------------------------------------------------


## First litter ------------------------------------------------------------

plotOffspringMass_1stL <- plotOffspringMass(
  litterNums = c(1)
  , fontSize = textSize
)

mass_plot_1stL <- plotOffspringMass_1stL(massFiltered)

# Maturation --------------------------------------------------------------

matVals <- getMaxMatVals(maturation_byDam_f, maturation_byDam_m)

max_mass <- matVals$max_mass
max_age <- matVals$max_age

matVals_indiv <- getMaxMatVals(maturationFiltered, maturationFiltered)
indivMaxAge <- matVals_indiv$max_age

## Vaginal opening ---------------------------------------------------------

plotVOAge <- plotVaginalOpeningAgeFunc(
  fontSize = textSize
  , dotSize = dotSize
  , facetLitter = facetMatByLitter
)
plotVOMass <- plotVaginalOpeningMassFunc(
  fontSize = textSize
  , dotSize = dotSize
  , facetLitter = facetMatByLitter
)

VO_age_plot <- plotVOAge(maturation_byDam_f) +
  expand_limits(y = max_age)

VO_mass_plot <- plotVOMass(maturation_byDam_f)+
  expand_limits(y = max_mass)

plotVOCumFreq_1stL <- plotVOAgeCumFreqFunc(
  c(1)
  , maxAge = indivMaxAge
  , fontSize = textSize
)

VO_cumFreq_plot_1stL <- plotVOCumFreq_1stL(maturationFiltered)


## First estrus ------------------------------------------------------------

plotEstrusAge <- plotEstrusAgeFunc(
  fontSize = textSize
  , dotSize = dotSize
  , facetLitter = facetMatByLitter
)
plotEstrusMass <- plotEstrusMassFunc(
  fontSize = textSize
  , dotSize = dotSize
  , facetLitter = facetMatByLitter
)

Estrus_age_plot <- plotEstrusAge(maturation_byDam_f) +
  expand_limits(y = max_age)

Estrus_mass_plot <- plotEstrusMass(maturation_byDam_f)+
  expand_limits(y = max_mass)

plotEstrusCumFreq_1stL <- plotEstrusAgeCumFreqFunc(
  c(1)
  , maxAge = indivMaxAge
  , fontSize = textSize
)

Estrus_cumFreq_plot_1stL <- plotEstrusCumFreq_1stL(maturationFiltered)


## Preputial separation ----------------------------------------------------

plotPreputialSepAge <- plotPreputialSepAgeFunc(
  fontSize = textSize
  , dotSize = dotSize
  , facetLitter = facetMatByLitter
)
plotPreputialSepMass <- plotPreputialSepMassFunc(
  fontSize = textSize
  , dotSize = dotSize
  , facetLitter = facetMatByLitter
)

PreputialSep_age_plot <- plotPreputialSepAge(maturation_byDam_m) +
  expand_limits(y = max_age)

PreputialSep_mass_plot <- plotPreputialSepMass(maturation_byDam_m)+
  expand_limits(y = max_mass)

plotPreputialSepCumFreq_1stL <- plotPreputialSepAgeCumFreqFunc(
  c(1)
  , maxAge = indivMaxAge
  , fontSize = textSize
)

PreputialSep_cumFreq_plot_1stL <- plotPreputialSepCumFreq_1stL(maturationFiltered)



# Cycles ------------------------------------------------------------------


## Representative Plots ----------------------------------------------------

plotRepCycles_STD_1stL <- plotRepCyclesFunc(
  c("STD")
  , c(1)
  , fontSize = textSize 
)
plotRepCycles_LBN_1stL <- plotRepCyclesFunc(
  c("LBN")
  , c(1)
  , fontSize = textSize 
)
plotRepCycles_STD_2ndL <- plotRepCyclesFunc(
  c("STD")
  , c(2)
  , fontSize = textSize 
)
plotRepCycles_LBN_2ndL <- plotRepCyclesFunc(
  c("LBN")
  , c(2)
  , fontSize = textSize 
)

repCycles_STD_1stL <- plotRepCycles_STD_1stL(cyclesFiltered)
repCycles_LBN_1stL <- plotRepCycles_LBN_1stL(cyclesFiltered)
repCycles_STD_2ndL <- plotRepCycles_STD_2ndL(cyclesFiltered)
repCycles_LBN_2ndL <- plotRepCycles_LBN_2ndL(cyclesFiltered)


## Percent days in stage ---------------------------------------------------

plotCyclesPerc_1stL <- plotCyclesPercentFunc(
  c(1)
  , fontSize = textSize 
  , dotSize = dotSize
)
plotCyclesPerc_2ndL <- plotCyclesPercentFunc(
  c(2)
  , fontSize = textSize 
  , dotSize = dotSize
)

percDaysStage_1stL <- plotCyclesPerc_1stL(cyclesPercLong)
percDaysStage_2ndL <- plotCyclesPerc_2ndL(cyclesPercLong)


# ALPS --------------------------------------------------------------------


## Cort --------------------------------------------------------------------

plotCort_bothL <- plotCortFunc(c(1, 2))

plotCort_1stL <- plotCortFunc(c(1))
plotCort_2ndL <- plotCortFunc(c(2))

maleCortPlot <- cortFilteredMales %>%
  plotCort_bothL()

maleCortPlot_1stL <- cortFilteredMales %>%
  plotCort_1stL()
maleCortPlot_2ndL <- cortFilteredMales %>%
  plotCort_2ndL()

diCortPlot <- cortFilteredDi %>%
  plotCort_bothL()

diCortPlot_1stL <- cortFilteredDi %>%
  plotCort_1stL()
diCortPlot_2ndL <- cortFilteredDi %>%
  plotCort_2ndL()

proCortPlot <- cortFilteredPro %>%
  plotCort_bothL()

proCortPlot_1stL <- cortFilteredPro %>%
  plotCort_1stL()
proCortPlot_2ndL <- cortFilteredPro %>%
  plotCort_2ndL()


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

capacitancePlot <- GABApscsFilteredFiring %>%
  plotCapacitance()
RseriesPlot <- GABApscsFilteredFiring %>%
  plotRseries()
RinputPlot <- GABApscsFilteredFiring %>%
  plotRinput()
holdingCurrPlot <- GABApscsFilteredFiring %>%
  plotHoldingCurr()
GABAfreqPlot <- GABApscsFilteredFiring %>%
  plotGABAfreq()
GABAampPlot <- GABApscsFilteredFiring %>%
  plotGABAamp()



