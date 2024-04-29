textSize <- 11
dotSize <- 1.5
# textSize <- 24
# dotSize <- 3
facetMatByLitter <- FALSE
isManuscript <- TRUE

# Dam behavior ------------------------------------------------------------

figDams_exits <- damBehavior_byPND %>%
  plotDamBehavior(
    yVar = Num_exits
    , yLab = "mean # of exits / h"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , dotSize = 1
    , zoom_y = TRUE
    , ymax = 60
    , ymin = 0
    , showDots = FALSE
    , showMean = TRUE
    , addVertError = TRUE
    , lineAlpha = 1
    , dotAlpha = 1
    , lineSize = 0.25
  ) + 
  theme(
    strip.text.x.top = element_text(margin = margin(b=-10))
  )

figDams_meanExits <- damBehavior_byDam %>%
  plotDamBehavior(
    yVar = Num_exits
    , yLab = "mean # of exits / h"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymax = 60
    , ymin = 0
    , showMean = TRUE
    , addVertError = TRUE
    , dotAlpha = 1
  ) +
  theme(
    axis.text.x = element_text(face = "bold")
    , axis.title.x = element_blank()
  )

figDams_offNest <- damBehavior_byPND %>%
  plotDamBehavior(
    yVar = Perc_off_nest
    , yLab = "mean % time off nest"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , showDots = FALSE
    , dotSize = 1
    , zoom_y = TRUE
    , ymax = 100
    , ymin = 0
    , showMean = TRUE
    , addVertError = TRUE
    , lineAlpha = 1
    , dotAlpha = 1
    , lineSize = 0.25
  ) +
  theme(
    strip.text.x.top = element_text(margin = margin(b=-10))
  )

figDams_meanOffNest <- damBehavior_byDam %>%
  plotDamBehavior(
    yVar = Perc_off_nest
    , yLab = "mean % time off nest"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymax = 100
    , ymin = 0
    , showMean = TRUE
    , addVertError = TRUE
    , dotAlpha = 1
  ) +
  theme(
    axis.text.x = element_text()
    , axis.title.x = element_blank()
  )

# Dam corticosterone ------------------------------------------------------

figDamsD <- damFiltered %>%
  filter(
    !is.na(Cort_dam_P11)
  ) %>%
  scatterPlotLBN(
    yVar = Cort_dam_P11,
    yLab = "dam corticosterone\nAM (ng/mL)",
    textSize = textSize,
    dotSize = dotSize,
    zoom_y = FALSE
    , addMean = FALSE
    , addSEM = FALSE
  ) +
  addMeanHorizontalBar(
    width = .95
    , size = 0.6
    , meanColor = "black"
  ) +
  addMeanSE_vertBar(
    size = 0.6
    , barColor = "black"
  )

# Dam Mass ------------------------------------------------------


figDams_mass <- damFiltered %>%
  plot_dam_mass_lines(
    useLineType = FALSE, # TRUE/FALSE
    lineTypeVar = earlyLifeTrt,
    lineGroupVar = damID,
    xtitle = "postnatal day", #x axis label
    ytitle = "dam mass (g)", #y axis label
    title = NULL, # plot title
    individualLines = TRUE, # plot individual lines
    meanLines = FALSE, # plot mean lines with SE
    zoom_x = TRUE, # Zoom to part of x axis
    xmin = 0,
    xmax = 21,
    zoom_y = TRUE,
    ymax = 40,
    ymin = 0,
    indivLineAlpha = .15,
    indivLineSize = 0.3,
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
    # LBNColor = "#04b5b5"
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
    , barSize = 0.8
  )

# Offspring mass ----------------------------------------------------------

figOff_PND4 <- damFiltered %>%
  mutate(
    dayForFacet = "PND4"
  ) %>%
  scatterPlotLBN(
    yVar = Mass_P4
    , yLab = "mean mass (g)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 10
    , addMean = TRUE
    , addSEM = TRUE
  ) +
  facet_wrap(
    ~ dayForFacet
  ) +
  scale_y_continuous(
    breaks = c(0, 2, 4, 6, 8, 10)
  )

figOff_PND11 <- massFiltered %>%
  getAvgByDam() %>%
  mutate(
    dayForFacet = "PND11"
  ) %>%
  scatterPlotLBN(
    yVar = Mass_P11
    , yLab = "mean mass (g)"
    , textSize = textSize
    , dotSize = dotSize
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 10
    , addMean = FALSE
    , addSEM = FALSE
  ) +
  facet_wrap(
    ~ dayForFacet
  ) +
  scale_y_continuous(
    breaks = c(0, 2, 4, 6, 8, 10)
  ) +
  plotError_LMM (
    mass_PND11_lmm_errors_earlyLife
    , xVar = earlyLifeTrt
  )

figOffA_indiv <- massFiltered %>%
  plot_mass_lines(
    groupByDam = TRUE,
    facetBySex = TRUE,
    useLineType = FALSE,
    lineTypeVar = earlyLifeTrt,
    lineGroupVar = damID,
    xtitle = "postnatal day", #x axis label
    ytitle = "mean mass (g)", #y axis label
    title = NULL, # plot title
    individualLines = TRUE, # plot individual lines
    meanLines = FALSE, # plot mean lines with SE #2023-11-22 to add model error
    zoom_x = TRUE, # Zoom to part of x axis
    xmin = 0,
    xmax = 72,
    zoom_y = TRUE, # Zoom to part of y axis
    ymin = 0,
    ymax = 32,
    indivLineAlpha = 0.9,
    indivLineSize = 0.2,
    errorBarWidth = 0,
    meanLineSize = 0.5,
    meanAlpha = 1,
    errorBarSize = .5,
    # errorBarColor = "grey10",
    errorBarAlpha = 1,
    textSize = textSize,
    axisSize = 0.5,
    legendPosition = "none",
    STDColor = "grey80",
    LBNColor = "#66CCCC"
  ) +
  theme(
    legend.key = element_rect(fill = NA)
    , strip.text = element_blank()
    , axis.title.x = element_blank()
  )

figOffA_group <- massFiltered %>%
  plot_mass_lines(
    groupByDam = TRUE,
    facetBySex = TRUE,
    useLineType = FALSE,
    lineTypeVar = earlyLifeTrt,
    lineGroupVar = damID,
    xtitle = "postnatal day", #x axis label
    ytitle = "mean mass (g)", #y axis label
    title = NULL, # plot title
    individualLines = FALSE, # plot individual lines
    meanLines = FALSE, # plot mean lines with SE #2023-11-22 to add model error
    zoom_x = TRUE, # Zoom to part of x axis
    xmin = 0,
    xmax = 72,
    zoom_y = TRUE, # Zoom to part of y axis
    ymin = 0,
    ymax = 30,
    indivLineAlpha = 1,
    indivLineSize = 0.2,
    errorBarWidth = 0,
    meanLineSize = 0.5,
    meanAlpha = 1,
    errorBarSize = .5,
    # errorBarColor = "grey10",
    errorBarAlpha = 1,
    textSize = textSize,
    axisSize = 0.5,
    # legendPosition = "bottom",
    legendPosition = c(0.1, 0.8),
    STDColor = "grey20",
    LBNColor = "darkcyan"
  ) +
  theme(
    legend.key = element_rect(fill = NA, colour = NA)
  ) + 
  plotError_LMM_meanLine_mass(
    mass_lmm_errors
    , xVar = day
    , fill = earlyLifeTrt
    , barSize = .4
    , ribbonAlpha = 0.7
  ) +
  scale_fill_manual(
    values = c("STD" = "grey70", "LBN" = "cyan3") # might need to be different
    , "early-life trt"
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
  )

# Cycles ------------------------------------------------------------------


## Representative Plots ----------------------------------------------------

figCyclesA <-  cyclesFiltered %>%
  filter(
    numCycles < 3.1 & numCycles > 2.9
    , cycleLength <= 6 & cycleLength >= 5
  ) %>%
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
    colorValues = c("grey30", "cyan3")
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
    # , axis.title.x = element_text(margin = margin(t = -2))
  ) +
  xlab("postnatal day")

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
    , forManuscript = FALSE
  ) + 
  plotError_LMM(
    numCycles_lmm_error
    , xVar = earlyLifeTrt
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
    , meanColor = "black"
    , barColor = "black"
    ,
  ) +
  coord_cartesian(ylim = c(0, 80), clip = "off") +
  theme(
    strip.text.x.top = element_text(margin = margin(b = -5))
  )

# ALPS --------------------------------------------------------------------

source("./01-scripts/05.5-run-LBN-masses-stats-plots.R")
source("./01-scripts/05.5-run-LBN-female-masses-stats-plots.R")

## Cort --------------------------------------------------------------------

plotCort_long <- manuscriptCortPlotFunc(
  fontSize = textSize
  , dotSize = dotSize
  # , zoom_y = FALSE
  , zoom_y = TRUE
  , ymin = 0
  , ymax = ifelse(isManuscript, 450, 500)
  , plotMean = FALSE
  , plotSE = FALSE
)

plotCort_adultSep <- manuscriptCortPlotFunc(
  fontSize = textSize
  , dotSize = dotSize
  # , zoom_y = FALSE
  , zoom_y = TRUE
  , ymin = 0
  , ymax = 450
  , plotMean = FALSE
  , plotSE = FALSE
  , facetByAdultOnly = TRUE
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
          , time - 2
          , time + 2
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 1
    , color = comboTrt
  ) +
  labs(title = "males")

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
          , time - 2
          , time + 2
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 1
    , color = comboTrt
  ) +
  labs(title = "diestrous females")

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
          , time - 2
          , time + 2
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 1
    , color = comboTrt
  ) +
  labs(title = "proestrous females")

figCortA_adult <- cortFilteredMales %>%
  plotCort_adultSep() +
  cortScale +
  plotError_LMM_aes(
    male_cort_lmm_error %>%
      mutate(
        time = ifelse(
          (time == 0 & earlyLifeTrt == "STD")
          , time - 2.0
          , ifelse(
            (time == 0 & earlyLifeTrt == "LBN")
            , time - 1.5
            , ifelse(
              (time == 5 & earlyLifeTrt == "STD")
              , time + 1.4
              , time + 1.9
            )
        )
      )
    )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = comboTrt
  ) +
  labs(title = "males")

figCortB_adult <- cortFilteredDi %>%
  plotCort_adultSep() +
  cortScale +
  plotError_LMM_aes(
    female_cort_lmm_error %>%
      filter(
        Sac_cycle == "diestrus"
      ) %>%
      mutate(
        time = ifelse(
          (time == 0 & earlyLifeTrt == "STD")
          , time - 2.0
          , ifelse(
            (time == 0 & earlyLifeTrt == "LBN")
            , time - 1.5
            , ifelse(
              (time == 5 & earlyLifeTrt == "STD")
              , time + 1.4
              , time + 1.9
            )
          )
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = .7
    , color = comboTrt
  ) +
  labs(title = "diestrous females")

figCortC_adult <- cortFilteredPro %>%
  plotCort_adultSep() +
  cortScale + 
  plotError_LMM_aes(
    female_cort_lmm_error %>%
      filter(
        Sac_cycle == "proestrus"
      ) %>%
      mutate(
        time = ifelse(
          (time == 0 & earlyLifeTrt == "STD")
          , time - 2.0
          , ifelse(
            (time == 0 & earlyLifeTrt == "LBN")
            , time - 1.5
            , ifelse(
              (time == 5 & earlyLifeTrt == "STD")
              , time + 1.4
              , time + 1.9
            )
          )
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = .7
    , color = comboTrt
  ) +
  labs(title = "proestrous females")

figCort_females_adult <- cortFilteredFemales %>%
  plotCort_adultSep() +
  cortScale +
  facet_wrap(
    ~ Sac_cycle + adultTrt
    , nrow = 1
  ) +
  plotError_LMM_aes(
    female_cort_lmm_error %>%
      mutate(
        time = ifelse(
          (time == 0 & earlyLifeTrt == "STD")
          , time - 2.0
          , ifelse(
            (time == 0 & earlyLifeTrt == "LBN")
            , time - 1.5
            , ifelse(
              (time == 5 & earlyLifeTrt == "STD")
              , time + 1.4
              , time + 1.9
            )
          )
        )
      )
    , xVar = time
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = .7
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
    lineTypeGuide = c("dotted", "solid"),
    positionDodge = 0.2, #this controls the spread of the data visually
    zoom_y = TRUE,
    ymax = 1000,
    ymin = 0
    , plotMean = FALSE
    , plotSE = FALSE
    , xBreaks = c(0, 1, 2, 3, 4, 5)
    # , xLabels = c("0h", "1h", "2h", "3h", "4h", "5h")
    , xLabels = c(0, 1, 2, 3, 4, 5)
    , pointAlpha = 1
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
    legend.position = c(0.15, .95)
    , legend.text = element_text(size = textSize)
    , legend.title = element_blank()
    , axis.title.x = element_text(margin = margin(t = -1))
  )  +
  dosageFillShape(fillAlpha = 1)+
  plotError_LMM(
    maleCortAdmin_cort_lmm_error %>%
      mutate(
        time = as.numeric(as.character(time))
      )
    , xVar = time
  ) +
  labs(x = "time (hr)"
       ) +
  scale_y_continuous(
    breaks = seq(0, 1100, 150)
  ) +
  scale_linetype_manual(
    values = c("0" = "dotted", 
               "2" = "solid"),
    labels = c("0" = "0mg/kg",
               "2" = "2mg/kg")
  )

## LH ----------------------------------------------------------------------

### Diestrus -----------------

figLH_diAfternoon <- acuteStressFilteredDi %>%
  plotCatVarFunc(
    expr(avgLH)
    , fontSize = textSize
    , dotSize = dotSize
    , twoLineXLabs = TRUE
    , useFacetLabels = FALSE
    , addMeanSE = FALSE
    , useSpecYLab = TRUE
    , thisYLab = "avg. evening LH (ng/mL)"
  )(
    zoom_y = FALSE
    , ymin = 0
    , ymax = 40
  ) +
  plotError_LMM(
    LH_diAfternoon_lmm_errors
    , xVar = comboTrt
  ) +
  labs(title = "diestrus")

### Pro - ephys -----------
# 
# figLH_ephysMax <- acuteStressFilteredPro_ephys %>%
#   plotCatVarFunc(
#     expr(maxLH)
#     , thisYLab = "max evening LH (ng/mL)"
#     , fontSize = textSize
#     , dotSize = dotSize
#     , twoLineXLabs = TRUE
#     , useFacetLabels = FALSE
#     , addMeanSE = FALSE
#     , useSpecYLab = TRUE
#   )(
#     zoom_y = TRUE
#     , ymin = 0
#     , ymax = 40
#   ) +
#   geom_hline(yintercept = surgeMin, color = "grey") +
#   addMedianHorizontalBar(width = 0.9, size = 0.6, color = "black")+
#   stat_summary(fun.min = function(z) { quantile(z,0.25) },
#                fun.max = function(z) { quantile(z,0.75) }
#                , geom = "linerange") +
#   labs(title = "limited sampling")

### Pro - ephys - surged ---------------


# figLH_ephysSurged <- acuteStressFilteredPro_ephys %>%
#   propSurgedPlotCombo_forSBN(
#     fontSize = textSize
#     , labelSize = 5
#   ) +
#   labs(title = "limited sampling")

### Pro - sampling - over time -----------

LHSamplingDF_color <- acuteStressFilteredPro_sampling %>%
  filter(
    maxLH >= surgeMin
  ) %>%
  group_by(earlyLifeTrt, adultTrt) %>%
  mutate(
    # rowNum = row_number()
    rank = rank(
      row_number()
      , ties.method = "first"
    )
    , color = viridis::viridis(max(rank))[rank]
  ) %>%
  ungroup() %>%
  select(
    mouseID
    , color
  ) %>%
  full_join(
    LHFilteredPro_sampling %>%
      filter(
        time > 0
      )
    , by = c("mouseID")
  ) %>%
  mutate(
    color = ifelse(
      maxLH < surgeMin
      , "#C0C0C0"
      , color
    )
  )
  

# LHSamplingDF_color <- LHFilteredPro_sampling %>%
#   filter(
#     time > 0
#     , maxLH >= surgeMin
#   ) %>%
#   addOrderedColors(
#     maxLH
#     , mouseID
#     , colorByGroups = TRUE
#     , pkg = "viridis"
#     , byMax = FALSE
#     , revOrder = TRUE
#     , comboTrt
#   ) %>%
#   select(
#     mouseID
#     , time
#     , color
#   ) %>%
#   full_join(
#     LHFilteredPro_sampling %>%
#       filter(
#         time > 0
#       )
#     , by = c("mouseID", "time")
#   ) %>%
#   mutate(
#     color = ifelse(
#       maxLH < surgeMin
#       , "#C0C0C0"
#       , color
#     )
#   )

plotLH_overTime_color <-  function(df){
  df %>%
    LHPlot_noMean_lineColor(
      fontSize = textSize
      , zoom_y = TRUE
      , ymin = 0
      , ymax = 40
      , addPoint = FALSE
    ) + theme(
      legend.position = "none"
    ) + facet_wrap(
      ~comboTrt,
      scales = "free"
      , nrow = 1
    ) +
    theme(
      # strip.text.x.top = element_text(margin = margin(b = -2))
    )
}

figLH_samplingTime <- LHSamplingDF_color %>%
  plotLH_overTime_color()

figLH_samplingTime_STDCON <- LHSamplingDF_color %>%
  filter(
    comboTrt == "STD-CON"
  ) %>%
  plotLH_overTime_color()

figLH_samplingTime_LBNCON <- LHSamplingDF_color %>%
  filter(
    comboTrt == "LBN-CON"
  ) %>%
  plotLH_overTime_color()

figLH_samplingTime_STDALPS <- LHSamplingDF_color %>%
  filter(
    comboTrt == "STD-ALPS"
  ) %>%
  plotLH_overTime_color()

figLH_samplingTime_LBNALPS <- LHSamplingDF_color %>%
  filter(
    comboTrt == "LBN-ALPS"
  ) %>%
  plotLH_overTime_color()


### Pro - sampling max -----------

figLH_samplingMax <- acuteStressFilteredPro_sampling %>%
  plotCatVarFunc(
    expr(maxLH)
    , thisYLab = "max evening LH (ng/mL)"
    , fontSize = textSize
    , dotSize = dotSize
    , twoLineXLabs = TRUE
    , useFacetLabels = FALSE
    , addMeanSE = FALSE
    , useSpecYLab = TRUE
  )(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 40
  ) +
  geom_hline(yintercept = surgeMin, color = "grey", linewidth = ifelse(isManuscript, 1, 1.5))+
  addMedianHorizontalBar(width = 0.9, size = ifelse(isManuscript, 0.6, 1), color = "black")+
  stat_summary(fun.min = function(z) { quantile(z,0.25) },
               fun.max = function(z) { quantile(z,0.75) }
               , geom = "linerange", color = "black"
               , linewidth = ifelse(isManuscript, 0.6, 1)
               )

if(isManuscript){
  figLH_samplingMax <- figLH_samplingMax +
    labs(title = "proestrus")
}


### Pro - sampling - surged ---------------

figLH_samplingSurged <- acuteStressFilteredPro_sampling %>%
  filter(
    !is.na(LH_hr5)
  ) %>%
  propSurgedPlotCombo_forSBN(
    fontSize = textSize
    , labelSize = 5
  )

if(isManuscript){
  figLH_samplingSurged <- figLH_samplingSurged +
    labs(title = "proestrus")
}

# GABA PSCs ---------------------------------------------------------------

plotCapacitance_noMean <- plotCatVarFunc(
  expr(capacitance)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotRseries_noMean <- plotCatVarFunc(
  expr(Rseries)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotRinput_noMean <- plotCatVarFunc(
  expr(Rinput)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotHoldingCurr_noMean <- plotCatVarFunc(
  expr(holdingCurrent)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAfreq_noMean <- plotCatVarFunc(
  expr(frequency)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAfreq_noMean_Pres <- plotCatVarFunc(
  expr(frequency)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAamp_noMean_Pres <- plotCatVarFunc(
  expr(amplitude)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAamp_noMean <- plotCatVarFunc(
  # expr(relPeak)
  expr(amplitude)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "amplitude (pA)"
)

plotGABAriseTime_noMean <- plotCatVarFunc(
  expr(riseTime)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
)

plotGABAdecayTime_noMean <- plotCatVarFunc(
  expr(decayTimes_8020)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "decay time (ms)"
)

plotGABAfwhm_noMean <- plotCatVarFunc(
  expr(fwhm)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = FALSE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "fwhm (ms)"
)

plotGABAinterval_noMean <- plotCatVarFunc(
  expr(interval)
  , fontSize = textSize
  , dotSize = dotSize
  , twoLineXLabs = TRUE
  , tiltedXLabs = TRUE
  , useFacetLabels = FALSE
  , addMeanSE = FALSE
  , useSpecYLab = TRUE
  , thisYLab = "interevent interval (s)"
)

figGABAa_model <- GABApscs_240FilteredPropsFreq %>%
  plotCapacitance_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 25
  ) +
  plotError_LMM(
    capacitance_lmm_errors
    , xVar = comboTrt
  )

figGABAc_model <- GABApscs_240FilteredPropsFreq %>%
  plotRseries_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 20
  ) +
  plotError_LMM(
    seriesResistance_lmm_errors
    , xVar = comboTrt
  )


figGABAb_model <- GABApscs_240FilteredPropsFreq %>%
  plotRinput_noMean() +
  plotError_LMM(
    inputResistance_lmm_errors
    , xVar = comboTrt
  )


figGABAd_model <- GABApscs_240FilteredPropsFreq %>%
  plotHoldingCurr_noMean(
    zoom_y = TRUE
    , ymin = -100
    , ymax = 25
  ) +
  plotError_LMM(
    holdingCurrent_lmm_errors
    , xVar = comboTrt
  ) +
  scale_y_continuous(
    breaks = c(-100, -75, -50, -25, 0, 25)
  )

figGABA_freq_model <- GABApscs_240FilteredPropsFreq %>%
  plotGABAfreq_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  ) +
  plotError_LMM(
    numEvents_nb.GLMM_errors
    , xVar = comboTrt
  )

figGABA_freq_model_Pres <- GABApscs_240FilteredPropsFreq %>%
  plotGABAfreq_noMean_Pres(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  ) +
  plotError_LMM(
    numEvents_nb.GLMM_errors
    , xVar = comboTrt
  )

figGABA_int_model <- GABApscs_240FilteredPropsFreq %>%
  plotGABAinterval_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 30
  ) +
  plotError_LMM(
    interval_lmm_error
    , xVar = comboTrt
  ) +
  scale_y_continuous(
    breaks = c(seq(0, 30, by = 5))
  )

figGABA_amp_model <- GABApscs_240FilteredPropsFreq %>%
  plotGABAamp_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 90
  ) +
  plotError_LMM(
    relAmplitude_lmm_errors
    , xVar = comboTrt
  ) +
  scale_y_continuous(
    breaks = c(seq(0, 90, by = 15))
  )

figGABA_amp_model_Pres <- GABApscs_240FilteredPropsFreq %>%
  plotGABAamp_noMean_Pres(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 90
  ) +
  plotError_LMM(
    relAmplitude_lmm_errors
    , xVar = comboTrt
  ) +
  scale_y_continuous(
    breaks = c(seq(0, 90, by = 15))
  )

figGABA_decayTime_model <- GABApscs_240FilteredPropsFreq %>%
  plotGABAdecayTime_noMean(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 90
  ) +
  plotError_LMM(
    decayTime_lmm_errors
    , xVar = comboTrt
  ) +
  scale_y_continuous(
    breaks = c(seq(0, 90, by = 15))
  )
