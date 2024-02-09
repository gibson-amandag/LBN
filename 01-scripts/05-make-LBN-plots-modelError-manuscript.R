textSize <- 11
dotSize <- 1.5
# textSize <- 24
# dotSize <- 3
facetMatByLitter <- FALSE

# Dam behavior ------------------------------------------------------------

figDams_exits <- damBehavior_byPND %>%
  plotDamBehavior(
    yVar = Num_exits
    , yLab = "mean # of exits / hr"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , dotSize = .8
    , zoom_y = TRUE
    , ymax = 60
    , ymin = 0
    , showMean = TRUE
    , addVertError = TRUE
    , lineAlpha = 0.2
    , dotAlpha = 0.4 
  # ) +
  # plotError_LMM(
  #   numExits_nb.GLMM_errors %>%
  #     mutate(
  #       PND = as.numeric(as.character(PND))
  #     )
  #   , xVar = PND
  #   , meanBarWidth = 0.7
  #   , color = "black"
  #   , nudgeErrorLine = 0
  )

figDams_meanExits <- damBehavior_byDam %>%
  # unite(
  #   fullRec,
  #   earlyLifeTrt,
  #   cohort,
  #   sep = "-",
  #   remove = FALSE
  # ) %>%
  # scatterPlot_general(
  #   xVar = earlyLifeTrt
  #   , xLab = NULL
  #   , yVar = Num_exits
  #   , yLab = "mean # exits"
  #   , fillVar = fullRec
  #   , fillLimits = c("STD-7", "STD-9", "LBN-7", "LBN-9")
  #   , fillValues = c("grey", "white", "darkcyan", "lightblue1")
  #   , textSize = textSize
  #   , dotSize = dotSize
  #   , addMean = TRUE
  #   , addSE = TRUE
  #   , zoom_y = TRUE
  #   , ymax = 50
  #   , ymin = 0
  # ) +
  scatterPlotLBN(
    yVar = Num_exits
    , yLab = "mean # of exits / hr"
    , addMean = TRUE
    , addSEM = TRUE
    , zoom_y = TRUE
    , ymax = 60
    , ymin = 0
    , dotSize = dotSize
    , textSize = textSize
    , meanColor = "magenta"
    , barColor = "magenta"
  ) +
  # plotError_LMM(
  #   numExits_nb.GLMM_errors.earlyLifeEMM
  #   , xVar = earlyLifeTrt
  #   , meanBarWidth = 0.7
  #   , color = "black"
  #   , nudgeErrorLine = 0
  # ) +
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
    , dotSize = 0.8
    , zoom_y = TRUE
    , ymax = 100
    , ymin = 0
    , showMean = TRUE
    , addVertError = TRUE
    , lineAlpha = 0.2
    , dotAlpha = 0.4
  # ) +
  # plotError_LMM(
  #   percOffNest_lmm_errors %>%
  #     mutate(
  #       PND = as.numeric(as.character(PND))
  #     )
  #   , xVar = PND
  #   , nudgeErrorLine = 0
  #   , nudgeMeanLine = 0
  #   , color = "black"
  )

figDams_meanOffNest <- damBehavior_byDam %>%
  scatterPlotLBN(
    yVar = Perc_off_nest
    , yLab = "mean % off nest"
    , addMean = TRUE
    , addSEM = TRUE
    , zoom_y = TRUE
    , ymax = 100
    , ymin = 0
    , dotSize = dotSize
    , textSize = textSize
    , meanColor = "magenta"
    , barColor = "magenta"
  ) +
  # plotError_LMM(
  #   percOffNest_lmm_errors.earlyLifeEMM
  #   , xVar = earlyLifeTrt
  #   , meanBarWidth = 0.7
  #   , color = "black"
  #   , nudgeErrorLine = 0
  # ) +
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
    zoom_y = FALSE
    # ymin = 0,
    # ymax = 120
    , meanColor = "magenta"
    , barColor = "magenta"
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
    meanLines = FALSE, # plot mean lines with SE #2023-11-22 to add model error
    zoom_x = TRUE, # Zoom to part of x axis
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
  ) + 
  plotError_LMM_meanLine_mass(
    mass_lmm_errors
    , xVar = day
    , fill = earlyLifeTrt
    , barSize = .6
    , ribbonAlpha = .4
  ) +
  scale_fill_manual(
    values = c("STD" = "grey30", "LBN" = "cyan4")
  )

# Maturation --------------------------------------------------------------

figOffAge_model <- maturationByDamLong %>%
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
    , color = "magenta"
    , nudgeErrorLine = 0
    , barSize = 0.6
  )

figOffMass_model <- maturationByDamLong %>%
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
    matMass_lmm_errors
    , xVar = earlyLifeTrt
    , color = "magenta"
    , nudgeErrorLine = 0
    , barSize = 0.6
  )

matVals <- getMaxMatVals(maturation_byDam_f, maturation_byDam_m)

max_mass <- matVals$max_mass
max_age <- matVals$max_age
max_AGD <- matVals$max_AGD

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
    , color = "magenta"
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
    , color = "magenta"
    , barSize = 0.5
    , meanBarWidth = 0.7
    , nudgeErrorLine = 0
  )

# Cycles ------------------------------------------------------------------


## Representative Plots ----------------------------------------------------

## replaced with getRandomSubjects 2023-11-11
# stdCycles <- cyclesFiltered %>%
#   filter(
#     earlyLifeTrt == "STD"
#   )
# lbnCycles <- cyclesFiltered %>%
#   filter(
#     earlyLifeTrt == "LBN"
#   )
# stdMice <- stdCycles[sample(nrow(stdCycles))[1:4],]$mouseID
# lbnMice <- lbnCycles[sample(nrow(lbnCycles))[1:4],]$mouseID

figCyclesA <-  cyclesFiltered %>%
  # filter(
  #   mouseID %in% stdMice | mouseID %in% lbnMice
  # ) %>%
  group_by(earlyLifeTrt) %>%
  getRandomSubjects(mouseID, 4, seed = 42) %>%
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

figCycles_numCycles_model <- cyclesFiltered %>%
  getAvgByDam() %>% # 2023-06-18 - reduce the density, average by litter
  scatterPlotLBN(
    yVar = numCycles
    , yLab = "mean # cycles P70-90"
    , textSize = textSize
    , dotSize = dotSize
    , addMean = FALSE
    , addSEM = FALSE
  ) + 
  plotError_LMM(
    numCycles_lmm_error
    , xVar = earlyLifeTrt
    , meanBarWidth = 0.7
    , barSize = 0.4
    , color = "magenta"
    , nudgeErrorLine = 0
  )

figCycles_lengthLog_model <- cyclesFiltered %>%
  getAvgByDam() %>%
  scatterPlotLBN(
    yVar = cycleLength
    , yLab = "mean length (days)"
    , textSize = textSize
    , dotSize = dotSize
    , addMean = FALSE
    , addSEM = FALSE
    , zoom_y = TRUE # make false if log
    , ymin = 0
    , ymax = 10
  ) +
  # scale_y_continuous(
  #   trans = "log"
  #   , limits = c(1, 10)
  #   , breaks = c(2, 4, 6, 8, 10)
  # ) +
  plotError_LMM(
    lengthCycles_log_lmm_error
    , xVar = earlyLifeTrt
    , meanBarWidth = 0.7
    , color = "magenta"
    , nudgeErrorLine = 0
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
    , meanColor = "magenta"
    , barColor = "magenta"
    ,
  ) +
  coord_cartesian(ylim = c(0, 80))

# ALPS --------------------------------------------------------------------


## Cort --------------------------------------------------------------------

plotCort_long <- manuscriptCortPlotFunc(
  fontSize = textSize
  , dotSize = dotSize
  # , zoom_y = FALSE
  , zoom_y = TRUE
  , ymin = 0
  , ymax = 750
  , plotMean = FALSE
  , plotSE = FALSE
)

# cortScale <- scale_y_continuous(
#   trans = "log"
#   , limits = c(1, 700)
#   , breaks = c(1.5625, 3.125, 6.25, 12.5, 25, 50, 100, 200, 400, 800)
#   , labels = c("1.56", "3.13", "6.25", "12.5", "25", "50", "100", "200", "400", "800")
# )

cortScale <- scale_y_continuous(
  breaks = c(0, 150, 300, 450, 600, 750)
)


figCortA <- cortFilteredMales %>%
  plotCort_long() +
  cortScale +
  plotError_LMM_aes(
    male_cort_lmm_error %>%
      mutate(
        time = ifelse(
          time == 0
          , time - 1.5
          , time + 1.5
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 1
    , color = comboTrt
  )

figCortB <- cortFilteredDi %>%
  plotCort_long() +
  cortScale +
  plotError_LMM_aes(
    female_cort_lmm_error %>%
      filter(
        Sac_cycle == "diestrus"
      ) %>%
      mutate(
        time = ifelse(
          time == 0
          , time - 1.5
          , time + 1.5
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 1
    , color = comboTrt
  )

figCortC <- cortFilteredPro %>%
  plotCort_long() +
  cortScale + 
  plotError_LMM_aes(
    female_cort_lmm_error %>%
      filter(
        Sac_cycle == "proestrus"
      ) %>%
      mutate(
        time = ifelse(
          time == 0
          , time - 1.5
          , time + 1.5
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 1
    , color = comboTrt
  )

## cort administration cort ------
cortAdmin_cort <- maleCortAdmin_cort_filtered %>%
  filter(
    # time %in% c(0, 5)
  ) %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    groupVar = dosage,
    lineTypeGuide = c("solid", "dotted"),
    positionDodge = 0.2, #this controls the spread of the data visually
    zoom_y = TRUE,
    ymax = 750,
    ymin = 0
    , plotMean = FALSE
    , plotSE = FALSE
    , xBreaks = c(0, 1, 2, 3, 4, 5)
    # , xLabels = c("0h", "1h", "2h", "3h", "4h", "5h")
    , xLabels = c(0, 1, 2, 3, 4, 5)
    , pointAlpha = 0.7
    , yUnitsNewLine = TRUE
  ) +
  # facet_wrap(
  #   ~ dosage,
  #   labeller = labeller(dosage = c("0" = "0mg/kg", "2" = "2mg/kg"))
  #   , strip.position = "bottom"
  # ) +
  # labs(
  #   x = "time since 1st administration (h)" # add a title to the x axis
  # ) +
  theme(
    legend.position = "bottom"
    , legend.margin = margin(-10,0,0,0)
  )  +
  dosageFillShape()+
  plotError_LMM(
    maleCortAdmin_cort_lmm_error %>%
      mutate(
        time = as.numeric(as.character(time))
      )
    , xVar = time
    , meanBarWidth = 0.7
    , color = "magenta"
    , nudgeErrorLine = 0
  ) +
  labs(x = "time (hr)"
       ) +
  scale_y_continuous(
    breaks = seq(0, 750, 150)
  ) +
  scale_linetype_manual(
    values = c("0" = "solid", 
               "2" = "dotted"),
    labels = c("0" = "0mg/kg",
               "2" = "2mg/kg")
  )


  
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

source("./01-scripts/05.5-run-LBN-masses-stats-plots.R")
source("./01-scripts/05.5-run-LBN-female-masses-stats-plots.R")

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

plotCapacitance_noMean <- plotCatVarFunc(
  expr(capacitance)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotRseries_noMean <- plotCatVarFunc(
  expr(Rseries)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotRinput_noMean <- plotCatVarFunc(
  expr(Rinput)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotHoldingCurr_noMean <- plotCatVarFunc(
  expr(holdingCurrent)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAfreq_noMean <- plotCatVarFunc(
  expr(frequency)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAamp_noMean <- plotCatVarFunc(
  expr(relPeak)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

figGABAa_model <- GABApscs_240FilteredFiring %>%
  plotCapacitance_noMean() +
  plotError_LMM(
    capacitance_lmm_errors
    , xVar = comboTrt
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = "magenta"
  )

figGABAc_model <- GABApscs_240FilteredFiring %>%
  plotRseries_noMean() +
  plotError_LMM(
    seriesResistance_lmm_errors
    , xVar = comboTrt
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = "magenta"
  )


figGABAb_model <- GABApscs_240FilteredFiring %>%
  plotRinput_noMean() +
  plotError_LMM(
    inputResistance_lmm_errors
    , xVar = comboTrt
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = "magenta"
  )


figGABAd_model <- GABApscs_240FilteredFiring %>%
  plotHoldingCurr_noMean() +
  plotError_LMM(
    holdingCurrent_lmm_errors
    , xVar = comboTrt
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = "magenta"
  )

figGABAe_model <- GABApscs_240FilteredFiring %>%
  plotGABAfreq_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  ) +
  plotError_LMM(
    numEvents_nb.GLMM_errors
    , xVar = comboTrt
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = "magenta"
  )

figGABAf_model <- GABApscs_240FilteredFiring %>%
  plotGABAamp_noMean() +
  plotError_LMM(
    relAmplitude_lmm_errors
    , xVar = comboTrt
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = "magenta"
  )




# # OLD -----------------
# 
# ## Masses -----
# 
# ### Functions ----
# 
# plotBodyMassAM_noMean <- plotCatVarFunc(
#   expr(Body_mass_AM)
#   , fontSize = textSize
#   , dotSize = dotSize
#   , twoLineXLabs = TRUE
#   , useFacetLabels = FALSE
#   , useSpecYLab = TRUE
#   , thisYLab = "body mass (g)"
#   , addLegend = TRUE
#   , removeXTicks = TRUE
#   , addMeanSE = FALSE
# )
# 
# plotChangeBodyMass_noMean <- plotCatVarFunc(
#   expr(bodyMass_diff)
#   , fontSize = textSize
#   , dotSize = dotSize
#   , twoLineXLabs = TRUE
#   , useFacetLabels = FALSE
#   , useSpecYLab = TRUE
#   , thisYLab = "\u0394 body mass (g)"
#   , removeXTicks = TRUE
#   , addMeanSE = FALSE
# )
# 
# plotPercChangeBodyMass_noMean <- plotCatVarFunc(
#   expr(percChangeBodyMass)
#   , fontSize = textSize
#   , dotSize = dotSize
#   , twoLineXLabs = TRUE
#   , useFacetLabels = FALSE
#   , useSpecYLab = TRUE
#   , thisYLab = "% change in body mass"
#   , removeXTicks = TRUE
#   , addMeanSE = FALSE
# )
# 
# plotRelAdrenalMass_noMean <- plotCatVarFunc(
#   expr(Adrenal_mass_perBodyAM_g)
#   , fontSize = textSize
#   , dotSize = dotSize
#   , twoLineXLabs = TRUE
#   , useFacetLabels = FALSE
#   , useSpecYLab = TRUE
#   , thisYLab = "normalized adrenal\nmass (mg/g)"
#   # , thisYLab = "rel. mass (mg/g)"
#   , removeXTicks = TRUE
#   , addMeanSE = FALSE
# )
# 
# plotRelReproTractMass_noMean <- plotCatVarFunc(
#   expr(ReproTract_mass_perBodyAM_g)
#   , fontSize = textSize
#   , dotSize = dotSize
#   , twoLineXLabs = TRUE
#   , useFacetLabels = FALSE
#   , useSpecYLab = TRUE
#   , thisYLab = "normalized repro. tract\nmass (mg/g)"
#   # , thisYLab = "rel. mass (mg/g)"
#   , removeXTicks = TRUE
#   , addMeanSE = FALSE
# )
# 
# plotRelSeminalVesicleMass_noMean <- plotCatVarFunc(
#   expr(ReproTract_mass_perBodyAM_g)
#   , fontSize = textSize
#   , dotSize = dotSize
#   , twoLineXLabs = TRUE
#   , useFacetLabels = FALSE
#   , useSpecYLab = TRUE
#   , thisYLab = "normalized seminal\nvesicle mass (mg/g)"
#   # , thisYLab = "rel. mass (mg/g)"
#   , removeXTicks = TRUE
#   , addMeanSE = FALSE
# )
# 
# plotRelUterineMass_noMean <- plotCatVarFunc(
#   expr(ReproTract_mass_perBodyAM_g)
#   , fontSize = textSize
#   , dotSize = dotSize
#   , twoLineXLabs = TRUE
#   , useFacetLabels = FALSE
#   , useSpecYLab = TRUE
#   , thisYLab = "normalized uterine\nmass (mg/g)"
#   # , thisYLab = "rel. mass (mg/g)"
#   , removeXTicks = TRUE
#   , addMeanSE = FALSE
# )
# 
# plotRelTesticularMass_noMean <- plotCatVarFunc(
#   expr(Gonad_mass_perBodyAM_g)
#   , fontSize = textSize
#   , dotSize = dotSize
#   , twoLineXLabs = TRUE
#   , useFacetLabels = FALSE
#   , useSpecYLab = TRUE
#   # , thisYLab = "rel. testicular mass\n(mg/g)"
#   , thisYLab = "normalized testicular mass (mg/g)"
#   , removeXTicks = TRUE
#   , addMeanSE = FALSE
# )
# 
# facetByHormoneStatus <- facet_wrap(
#   ~ hormoneStatus
#   , nrow = 1
# )
# 
# ### Graphs ----
# 
# #### Males --------
# 
# # Body mass AM
# figMaleMassA
# 
# # % change in body mass
# 
# # noramlized adrenal mass
# 
# # normalized seminal vesicle mass
# 
# # normalized testicular mass
# 
# # Cort admin
# 
# maleCortAdmin_plotByConsumption <- maleCortAdmin_cort %>%
#   plotCortByNutellaConsumption(
#     fontSize = 16
#   )
# 
# latterCortAdmin_plotByConsumption <- latterCortAdmin_cort %>%
#   plotCortByNutellaConsumption(
#     fontSize = 16
#   )
# 
# plotCortAdminAMBodyMass <- plotMaleCortAdminFunc(
#   expr(Body_mass_AM)
#   , thisYLab = "body mass (g)"
# )
# 
# plotCortAdminPercChangeBodyMass <- plotMaleCortAdminFunc(
#   expr(percChangeBodyMass)
#   , thisYLab = "% change in body mass"
# )
# 
# plotCortAdminTesticularMass <- plotMaleCortAdminFunc(
#   expr(Gonad_mass_perBodyAM_g)
#   , thisYLab = "rel. testicular mass (mg/g)"
# )
# 
# plotCortAdminSeminalVesicleMass <- plotMaleCortAdminFunc(
#   expr(ReproTract_mass_perBodyAM_g)
#   , thisYLab = "rel. seminal vesicle mass (mg/g)"
# )
# 
# plotCortAdminAdrenalMass <- plotMaleCortAdminFunc(
#   expr(Adrenal_mass_perBodyAM_g)
#   , thisYLab = "rel. adrenal vesicle mass (mg/g)"
# )
# 
# 
# maleCortAdmin_AMBodyMass <- plotCortAdminAMBodyMass(latterCortAdmin)
# maleCortAdmin_PercChangeBodyMass <- plotCortAdminPercChangeBodyMass(latterCortAdmin)
# maleCortAdmin_TesticularMass <- plotCortAdminTesticularMass(latterCortAdmin)
# maleCortAdmin_SeminalVesicleMass <- plotCortAdminSeminalVesicleMass(latterCortAdmin)
# maleCortAdmin_AdrenalMass <- plotCortAdminAdrenalMass(latterCortAdmin)
# 
# 
# figMassFacetA_model <- acuteStressFiltered_M_DiPro %>%
#   plotBodyMassAM_noMean() +
#   facetByHormoneStatus +
#   plotError_LMM(
#     bodyMassAM_lmm_error
#     , xVar = comboTrt
#     , nudgeErrorLine = 0
#     , nudgeMeanLine = 0
#     , meanBarWidth = 0.7
#     , color = "magenta"
#   )
# 
# figMassFacetB_perc_model <- acuteStressFiltered_M_DiPro %>%
#   plotPercChangeBodyMass_noMean() +
#   facetByHormoneStatus +
#   plotError_LMM(
#     percChangeBodyMass_lmm_error
#     , xVar = comboTrt
#     , nudgeErrorLine = 0
#     , nudgeMeanLine = 0
#     , meanBarWidth = 0.7
#     , color = "magenta"
#   )
# 
# figMassFacetC_model <- acuteStressFiltered_M_DiPro %>%
#   plotRelAdrenalMass_noMean(
#     zoom_y = TRUE
#     , ymin = 0
#     , ymax = 0.4
#   ) +
#   facetByHormoneStatus +
#   plotError_LMM(
#     adrenalMass_lmm_error
#     , xVar = comboTrt
#     , nudgeErrorLine = 0
#     , nudgeMeanLine = 0
#     , meanBarWidth = 0.7
#     , color = "magenta"
#   )
# 
# figMassFacetD_male_model <- acuteStressFiltered_M_DiPro %>%
#   filter(
#     sex == "M"
#   ) %>%
#   plotRelSeminalVesicleMass_noMean(
#     zoom_y = TRUE
#     , ymin = 0
#     , ymax = 10
#   ) +
#   facetByHormoneStatus +
#   plotError_LMM(
#     seminalVesicle_lmm_error
#     , xVar = comboTrt
#     , nudgeErrorLine = 0
#     , nudgeMeanLine = 0
#     , meanBarWidth = 0.7
#     , color = "magenta"
#   )
# 
# figMassFacetD_female_model <- acuteStressFiltered_M_DiPro %>%
#   filter(
#     sex == "F"
#   ) %>%
#   plotRelUterineMass_noMean(
#     zoom_y = TRUE
#     , ymin = 0
#     , ymax = 10
#   ) +
#   facetByHormoneStatus +
#   plotError_LMM(
#     uterineMass_lmm_error
#     , xVar = comboTrt
#     , nudgeErrorLine = 0
#     , nudgeMeanLine = 0
#     , meanBarWidth = 0.7
#     , color = "magenta"
#   )
# 
# figMass_testicular_model <- acuteStressFiltered_M_DiPro %>%
#   filter(
#     sex == "M"
#   ) %>%
#   plotRelTesticularMass_noMean()+
#   plotError_LMM(
#     testicularMass_lmm_error
#     , xVar = comboTrt
#     , nudgeErrorLine = 0
#     , nudgeMeanLine = 0
#     , meanBarWidth = 0.7
#     , color = "magenta"
#   )
