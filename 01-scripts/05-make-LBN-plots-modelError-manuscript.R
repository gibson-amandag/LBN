textSize <- 11
dotSize <- 2
facetMatByLitter <- FALSE

# Dam behavior ------------------------------------------------------------

figDams_exits <- damBehavior_byPND %>%
  plotDamBehavior(
    yVar = Num_exits
    , yLab = "mean # of exits"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , dotSize = 1
    , zoom_y = TRUE
    , ymax = 50
    , ymin = 0
    , showMean = FALSE
    , addVertError = FALSE
  ) +
  plotError_LMM(
    numExits_nb.GLMM_errors %>%
      mutate(
        PND = as.numeric(as.character(PND))
      )
    , xVar = PND
    , meanBarWidth = 0.7
    , color = "black"
    , nudgeErrorLine = 0
  )

figDams_meanExits <- damBehavior_byDam %>%
  scatterPlotLBN(
    yVar = Num_exits
    , yLab = "mean # of exits"
    , addMean = FALSE
    , addSEM = FALSE
    , zoom_y = TRUE
    , ymax = 50
    , ymin = 0
    , dotSize = dotSize
    , textSize = textSize
  ) + 
  plotError_LMM(
    numExits_nb.GLMM_errors.earlyLifeEMM
    , xVar = earlyLifeTrt
    , meanBarWidth = 0.7
    , color = "black"
    , nudgeErrorLine = 0
  ) +
  facet_wrap(
    ~earlyLifeTrt
    , scales = "free_x"
  ) +
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  )


figDams_offNest <- damBehavior_byPND %>%
  plotDamBehavior(
    yVar = Perc_off_nest
    , yLab = "mean % off nest"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , dotSize = 1
    , zoom_y = TRUE
    , ymax = 100
    , ymin = 0
    , showMean = FALSE
    , addVertError = FALSE
  ) +
  plotError_LMM(
    percOffNest_lmm_errors %>%
      mutate(
        PND = as.numeric(as.character(PND))
      )
    , xVar = PND
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , color = "black"
  )

figDams_meanOffNest <- damBehavior_byDam %>%
  scatterPlotLBN(
    yVar = Perc_off_nest
    , yLab = "mean % off nest"
    , addMean = FALSE
    , addSEM = FALSE
    , zoom_y = TRUE
    , ymax = 100
    , ymin = 0
    , dotSize = dotSize
    , textSize = textSize
  ) +
  plotError_LMM(
    numExits_nb.GLMM_errors.earlyLifeEMM
    , xVar = earlyLifeTrt
    , meanBarWidth = 0.7
    , color = "black"
    , nudgeErrorLine = 0
  ) +
  facet_wrap(
    ~earlyLifeTrt
    , scales = "free_x"
  ) +
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  )


# Dam corticosterone ------------------------------------------------------

figDamsD <- damFiltered %>%
  filter(
    !is.na(Cort_dam_P11)
  ) %>%
  scatterPlotLBN(
    yVar = Cort_dam_P11,
    yLab = "corticosterone\n(ng/mL)",
    textSize = textSize,
    dotSize = dotSize,
    zoom_y = FALSE,
    # ymin = 0,
    # ymax = 120
  )

# Dam Mass ------------------------------------------------------


figDams_mass <- damFiltered %>%
  plot_dam_mass_lines(
    useLineType = FALSE, # TRUE/FALSE
    lineTypeVar = earlyLifeTrt,
    lineGroupVar = damID,
    xtitle = "PND", #x axis label
    ytitle = "mass (g)", #y axis label
    title = NULL, # plot title
    individualLines = TRUE, # plot individual lines
    meanLines = FALSE, # plot mean lines with SE
    zoom_x = TRUE, # Zoom to part of x axis
    xmin = 0,
    xmax = 21,
    zoom_y = TRUE,
    ymax = 40,
    ymin = 0,
    indivLineAlpha = .2,
    indivLineSize = 0.5,
    errorBarWidth = 0,
    meanLineSize = 1,
    meanAlpha = 1,
    errorBarSize = 1,
    # errorBarColor = "grey10",
    errorBarAlpha = 1,
    textSize = textSize,
    axisSize = 0.5,
    # legendPosition = c(0.75, 0.2),
    legendPosition = c(0.5, 0.2),
    STDColor = "#4D4D4D",
    LBNColor = "#008B8B"
  ) +
  theme(
    legend.key = element_rect(fill = NA)
  ) +
  guides(
    color = guide_legend(nrow = 1)
  ) + 
  plotError_LMM_meanLine(
    damMass_lmm_errors %>%
      mutate(
        PND = as.numeric(as.character(PND))
      )
    , xVar = PND
    , color = earlyLifeTrt
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

figOffAge <- maturationByDamLong %>%
  scatterPlotLBN(
    yVar = age
    , "mean age (days)"
    , textSize = textSize
    , dotSize = dotSize
    , addMean = FALSE
    , addSEM = FALSE
  ) +
  facet_wrap(
    ~ matType
  ) +
  plotError_LMM(
    age_lmm_errors
    , xVar = earlyLifeTrt
    , color = "black"
    , nudgeErrorLine = 0
    , barSize = 0.6
  )

figOffMass <- maturationByDamLong %>%
  scatterPlotLBN(
    yVar = mass
    , "mean mass (g)"
    , textSize = textSize
    , dotSize = dotSize
    , addMean = FALSE
    , addSEM = FALSE
  ) +
  facet_wrap(
    ~ matType
  ) + 
  plotError_LMM(
    mass_lmm_errors
    , xVar = earlyLifeTrt
    , color = "black"
    , nudgeErrorLine = 0
    , barSize = 0.6
  )

figOff_femaleAGD <- maturation_byDam_f %>%
  mutate(
    sex = "F"
  ) %>%
  scatterPlotLBN(
    yVar = AGD_adult
    , yLab = "mean AGD (mm)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = max_AGD
    , addMean = FALSE
    , addSEM = FALSE
  ) +
  facet_wrap(
    ~ sex
    , labeller = labeller(sex = c("F" = "female", "M" = "male"))
  ) +
  plotError_LMM (
    AGD_lmm_errors %>% filter(
      sex == "F"
    )
    , xVar = earlyLifeTrt
    , color = "black"
    , barSize = 0.5
    , meanBarWidth = 0.7
    , nudgeErrorLine = 0
  )

figOff_maleAGD <- maturation_byDam_m %>%
  mutate(
    sex = "M"
  ) %>%
  scatterPlotLBN(
    yVar = AGD_adult
    , yLab = "mean AGD (mm)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = max_AGD
    , addMean = FALSE
    , addSEM = FALSE
  ) +
  facet_wrap(
    ~ sex
    , labeller = labeller(sex = c("F" = "female", "M" = "male"))
  ) +
  plotError_LMM (
    AGD_lmm_errors %>% filter(
      sex == "M"
    )
    , xVar = earlyLifeTrt
    , color = "black"
    , barSize = 0.5
    , meanBarWidth = 0.7
    , nudgeErrorLine = 0
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
  getAvgByDam() %>% # 2023-06-18 - reduce the density, average by litter
  scatterPlotLBN(
    yVar = numCycles
    , yLab = "mean # cycles P70-90"
    , textSize = textSize
    , dotSize = dotSize
  )
figCyclesB_noMean <- cyclesFiltered %>%
  getAvgByDam() %>% # 2023-06-18 - reduce the density, average by litter
  scatterPlotLBN(
    yVar = numCycles
    , yLab = "mean # cycles P70-90"
    , textSize = textSize
    , dotSize = dotSize
    , addMean = FALSE
    , addSEM = FALSE
  )

figCyclesC <- cyclesFiltered %>%
  getAvgByDam() %>%
  scatterPlotLBN(
    yVar = cycleLength
    , yLab = "mean length (days)"
    , textSize = textSize
    , dotSize = dotSize
  ) +
  scale_y_continuous(
    breaks = c(0, 2, 4, 6, 8, 10)
  )

figCyclesC_noMean <- cyclesFiltered %>%
  getAvgByDam() %>%
  scatterPlotLBN(
    yVar = cycleLength
    , yLab = "mean length (days)"
    , textSize = textSize
    , dotSize = dotSize
    , addMean = FALSE
    , addSEM = FALSE
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  ) +
  scale_y_continuous(
    breaks = c(0, 2, 4, 6, 8, 10)
  )


## Percent days in stage ---------------------------------------------------

figCyclesD <- cyclesPercLong %>%
  getAvgByDam(
    byStage = TRUE
  ) %>%
  plotCyclesPercent(
    fontSize = textSize
    , dotSize = dotSize
    , strip.position = "top"
    , ylabel = "mean % days in stage"
    ,
  ) +
  coord_cartesian(ylim = c(0, 80))

# ALPS --------------------------------------------------------------------


## Cort --------------------------------------------------------------------

# plotCort_onlyLBN <- manuscriptCortPlotFunc(
#   onlyLBN = TRUE 
#   , fontSize = textSize
#   , dotSize = dotSize
#   , yUnitsNewLine = TRUE
#   , jitterPosition = 1.8
#   , wrapLegend = TRUE
#   , useALPSLineType = FALSE
# )
# 
# plotCort_onlyLBN_longYLab <- manuscriptCortPlotFunc(
#   onlyLBN = TRUE 
#   , fontSize = textSize
#   , dotSize = dotSize
#   , yUnitsNewLine = FALSE
#   , jitterPosition = 1.8
#   , wrapLegend = FALSE
#   , meanWidth = 2.5
#   , useALPSLineType = FALSE
# )

plotCort_long <- manuscriptCortPlotFunc(
  fontSize = textSize
  , dotSize = dotSize
  , yUnitsNewLine = TRUE
  # , yUnitsNewLine = FALSE
  , jitterPosition = 1.8
)


figCortA <- cortFilteredMales %>%
  plotCort_long()
figCortB <- cortFilteredDi %>%
  plotCort_long()
figCortC <- cortFilteredPro %>%
  plotCort_long()

# figCort_opt2A <- cortFilteredMales %>%
#   plotCort_long()
# figCort_opt2B <- cortFilteredDi %>%
#   plotCort_long()
# figCort_opt2C <- cortFilteredPro %>%
#   plotCort_long()

# figCort_opt1A <- cortFilteredMales %>%
#   plotCort_onlyLBN()


# figCort_opt3A <- cortFilteredMales %>%
#   plotCort_onlyLBN_longYLab()

# figCort_opt1B <- cortFilteredDi %>%
#   plotCort_onlyLBN()


# figCort_opt3B <- cortFilteredDi %>%
#   plotCort_onlyLBN_longYLab()
# 
# figCort_opt1C <- cortFilteredPro %>%
#   plotCort_onlyLBN()

# 
# figCort_opt3C <- cortFilteredPro %>%
#   plotCort_onlyLBN_longYLab()

## Masses -----

### with mean -----
plotBodyMassAM <- plotCatVarFunc(
  expr(Body_mass_AM)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "body mass (g)"
  , addLegend = TRUE
  , removeXTicks = TRUE
)

plotChangeBodyMass <- plotCatVarFunc(
  expr(bodyMass_diff)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "\u0394 body mass (g)"
  , removeXTicks = TRUE
)

plotPercChangeBodyMass <- plotCatVarFunc(
  expr(percChangeBodyMass)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "% change in body mass"
  , removeXTicks = TRUE
)

plotRelAdrenalMass <- plotCatVarFunc(
  expr(Adrenal_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "normalized adrenal\nmass (mg/g)"
  # , thisYLab = "rel. mass (mg/g)"
  , removeXTicks = TRUE
)

plotRelReproTractMass <- plotCatVarFunc(
  expr(ReproTract_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "normalized repro. tract\nmass (mg/g)"
  # , thisYLab = "rel. mass (mg/g)"
  , removeXTicks = TRUE
)

plotRelSeminalVesicleMass <- plotCatVarFunc(
  expr(ReproTract_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "normalized seminal\nvesicle mass (mg/g)"
  # , thisYLab = "rel. mass (mg/g)"
  , removeXTicks = TRUE
)

plotRelUterineMass <- plotCatVarFunc(
  expr(ReproTract_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "normalized uterine\nmass (mg/g)"
  # , thisYLab = "rel. mass (mg/g)"
  , removeXTicks = TRUE
)

plotRelTesticularMass <- plotCatVarFunc(
  expr(Gonad_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  # , thisYLab = "rel. testicular mass\n(mg/g)"
  , thisYLab = "rel. mass (mg/g)"
)

### without mean -----
plotBodyMassAM_noMean <- plotCatVarFunc(
  expr(Body_mass_AM)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "body mass (g)"
  , addLegend = TRUE
  , removeXTicks = TRUE
  , addMeanSE = FALSE
)

plotChangeBodyMass_noMean <- plotCatVarFunc(
  expr(bodyMass_diff)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "\u0394 body mass (g)"
  , removeXTicks = TRUE
  , addMeanSE = FALSE
)

plotPercChangeBodyMass_noMean <- plotCatVarFunc(
  expr(percChangeBodyMass)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "% change in body mass"
  , removeXTicks = TRUE
  , addMeanSE = FALSE
)

plotRelAdrenalMass_noMean <- plotCatVarFunc(
  expr(Adrenal_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "normalized adrenal\nmass (mg/g)"
  # , thisYLab = "rel. mass (mg/g)"
  , removeXTicks = TRUE
  , addMeanSE = FALSE
)

plotRelReproTractMass_noMean <- plotCatVarFunc(
  expr(ReproTract_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "normalized repro. tract\nmass (mg/g)"
  # , thisYLab = "rel. mass (mg/g)"
  , removeXTicks = TRUE
  , addMeanSE = FALSE
)

plotRelSeminalVesicleMass_noMean <- plotCatVarFunc(
  expr(ReproTract_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "normalized seminal\nvesicle mass (mg/g)"
  # , thisYLab = "rel. mass (mg/g)"
  , removeXTicks = TRUE
  , addMeanSE = FALSE
)

plotRelUterineMass_noMean <- plotCatVarFunc(
  expr(ReproTract_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "normalized uterine\nmass (mg/g)"
  # , thisYLab = "rel. mass (mg/g)"
  , removeXTicks = TRUE
  , addMeanSE = FALSE
)

plotRelTesticularMass_noMean <- plotCatVarFunc(
  expr(Gonad_mass_perBodyAM_g)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , useSpecYLab = TRUE
  # , thisYLab = "rel. testicular mass\n(mg/g)"
  , thisYLab = "normalized testicular mass (mg/g)"
  , removeXTicks = TRUE
  , addMeanSE = FALSE
)

figMassA <- acuteStressFilteredMales %>%
  plotBodyMassAM()

figMassB <- acuteStressFilteredDi %>%
  plotBodyMassAM()

figMassC <- acuteStressFilteredPro %>%
  plotBodyMassAM()

figMassD <- acuteStressFilteredMales %>%
  plotChangeBodyMass()

figMassE <- acuteStressFilteredDi %>%
  plotChangeBodyMass()

figMassF <- acuteStressFilteredPro %>%
  plotChangeBodyMass()

figMassG <- acuteStressFilteredMales %>%
  plotRelAdrenalMass()

figMassH <- acuteStressFilteredDi %>%
  plotRelAdrenalMass()

figMassI <- acuteStressFilteredPro %>%
  plotRelAdrenalMass()

figMassJ <- acuteStressFilteredMales %>%
  plotRelSeminalVesicleMass(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 9
  )

figMassK <- acuteStressFilteredDi %>%
  plotRelUterineMass(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 9
  )

figMassL <- acuteStressFilteredPro %>%
  plotRelUterineMass(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 9
  )

### Faceted rows ----
acuteStressForMasses <- acuteStressFiltered_M_DiPro %>%
  mutate(
    hormoneStatus = 
      ifelse(
        sex == "M"
        , "male"
        , Sac_cycle
      )
  ) %>%
  mutate(
    hormoneStatus = factor(hormoneStatus, c("male", "diestrus", "proestrus"))
  )

facetByHormoneStatus <- facet_wrap(
  ~ hormoneStatus
  , nrow = 1
)

#### with mean ----
figMassFacetA <- acuteStressForMasses %>%
  plotBodyMassAM() +
  facetByHormoneStatus

figMassFacetB <- acuteStressForMasses %>%
  plotChangeBodyMass() +
  facetByHormoneStatus

figMassFacetB_perc <- acuteStressForMasses %>%
  plotPercChangeBodyMass() +
  facetByHormoneStatus

figMassFacetC <- acuteStressForMasses %>%
  plotRelAdrenalMass(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 0.4
  ) +
  facetByHormoneStatus

figMassFacetD_male <- acuteStressForMasses %>%
  filter(
    sex == "M"
  ) %>%
  plotRelSeminalVesicleMass(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  ) +
  facetByHormoneStatus

figMassFacetD_female <- acuteStressForMasses %>%
  filter(
    sex == "F"
  ) %>%
  plotRelUterineMass(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  ) +
  facetByHormoneStatus

#### without mean -----
figMassFacetA_noMean <- acuteStressForMasses %>%
  plotBodyMassAM_noMean() +
  facetByHormoneStatus

figMassFacetB_noMean <- acuteStressForMasses %>%
  plotChangeBodyMass_noMean() +
  facetByHormoneStatus

figMassFacetB_perc_noMean <- acuteStressForMasses %>%
  plotPercChangeBodyMass_noMean() +
  facetByHormoneStatus

figMassFacetC_noMean <- acuteStressForMasses %>%
  plotRelAdrenalMass_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 0.4
  ) +
  facetByHormoneStatus

figMassFacetD_male_noMean <- acuteStressForMasses %>%
  filter(
    sex == "M"
  ) %>%
  plotRelSeminalVesicleMass_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  ) +
  facetByHormoneStatus

figMassFacetD_female_noMean <- acuteStressForMasses %>%
  filter(
    sex == "F"
  ) %>%
  plotRelUterineMass_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  ) +
  facetByHormoneStatus
  



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
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
)

plotRseries <- plotCatVarFunc(
  expr(Rseries)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
)

plotRinput <- plotCatVarFunc(
  expr(Rinput)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
)

plotHoldingCurr <- plotCatVarFunc(
  expr(holdingCurrent)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
)

plotGABAfreq <- plotCatVarFunc(
  expr(frequency)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
)

plotGABAamp <- plotCatVarFunc(
  expr(relPeak)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
)

figGABAa <- GABApscs_240FilteredFiring %>%
  plotCapacitance()
figGABAc <- GABApscs_240FilteredFiring %>%
  plotRseries()
figGABAb <- GABApscs_240FilteredFiring %>%
  plotRinput()
figGABAd <- GABApscs_240FilteredFiring %>%
  plotHoldingCurr()
figGABAe <- GABApscs_240FilteredFiring %>%
  plotGABAfreq()
figGABAf <- GABApscs_240FilteredFiring %>%
  plotGABAamp()

## no mean -----

plotCapacitance_noMean <- plotCatVarFunc(
  expr(capacitance)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotRseries_noMean <- plotCatVarFunc(
  expr(Rseries)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotRinput_noMean <- plotCatVarFunc(
  expr(Rinput)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotHoldingCurr_noMean <- plotCatVarFunc(
  expr(holdingCurrent)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAfreq_noMean <- plotCatVarFunc(
  expr(frequency)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAamp_noMean <- plotCatVarFunc(
  expr(relPeak)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

figGABAa_noMean <- GABApscs_240FilteredFiring %>%
  plotCapacitance_noMean()
figGABAc_noMean <- GABApscs_240FilteredFiring %>%
  plotRseries_noMean()
figGABAb_noMean <- GABApscs_240FilteredFiring %>%
  plotRinput_noMean()
figGABAd_noMean <- GABApscs_240FilteredFiring %>%
  plotHoldingCurr_noMean()
figGABAe_noMean <- GABApscs_240FilteredFiring %>%
  plotGABAfreq_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  )
figGABAf_noMean <- GABApscs_240FilteredFiring %>%
  plotGABAamp_noMean()

# figGABAa <- GABApscsFilteredFiring %>%
#   plotCapacitance()
# figGABAc <- GABApscsFilteredFiring %>%
#   plotRseries()
# figGABAb <- GABApscsFilteredFiring %>%
#   plotRinput()
# figGABAd <- GABApscsFilteredFiring %>%
#   plotHoldingCurr()
# figGABAe <- GABApscsFilteredFiring %>%
#   plotGABAfreq()
# figGABAf <- GABApscsFilteredFiring %>%
#   plotGABAamp()



