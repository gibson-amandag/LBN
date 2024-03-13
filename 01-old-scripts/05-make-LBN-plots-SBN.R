textSize <- 32
dotSize <- 4
facetMatByLitter <- FALSE

# Dam behavior ------------------------------------------------------------


figDamsA <- damBehavior_byPND %>%
  filter(
    !is.na(Num_exits)
  ) %>%
  plotDamBehavior(
    yVar = Num_exits
    , yLab = "# of exits"
    , fontSize = textSize
    , addTriangleForMean = FALSE
    , colorByDam = TRUE
    , dotSize = 1.5
    # , facetByTrt = FALSE
  ) +
  theme(
    strip.text = element_blank()
  )



# Dam corticosterone ------------------------------------------------------

figDamsB <- damFiltered %>%
  filter(
    !is.na(Cort_dam_P11)
  ) %>%
  scatterPlotLBN(
    yVar = Cort_dam_P11,
    yLab = "corticosterone (ng/mL)",
    textSize = textSize,
    dotSize = dotSize
  )

# ALPS --------------------------------------------------------------------


## Cort --------------------------------------------------------------------

plotCort_long <- manuscriptCortPlotFunc(
  fontSize = textSize
  , dotSize = dotSize
  , yUnitsNewLine = FALSE
  , jitterPosition = 1.8
  , stripPosition = "top"
  , xmin = -1
  , xmax = 6
)

figCort <- cortFilteredPro %>%
  plotCort_long() +
  theme(
    strip.text = element_blank()
  )

## % surged ----------------------------------------------------------------


# have to remove the top and side lines for the LBN-ALPS
# made it dark cyan for the no surge to be able to have line at bottom
ephys_percSurgedPlot <- surgedDF %>%
  filter(
    is.na(LH_hr7.5) & !is.na(LH_hr5.5)
  ) %>%
  propSurgedPlotCombo_forSBN(
    fontSize = textSize
  )


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

figGABAa <- GABApscsFilteredPropsFreq %>%
  plotCapacitance()
figGABAc <- GABApscsFilteredPropsFreq %>%
  plotRseries()
figGABAb <- GABApscsFilteredPropsFreq %>%
  plotRinput()
figGABAd <- GABApscsFilteredPropsFreq %>%
  plotHoldingCurr()
figGABAe <- GABApscsFilteredPropsFreq %>%
  plotGABAfreq(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 6
  ) +
  scale_y_continuous(
    breaks = c(0, 1, 2, 3, 4, 5)
    , labels = c("0", "1.0", "2.0", "3.0", "4.0", "5.0")
  )
figGABAf <- GABApscsFilteredPropsFreq %>%
  plotGABAamp() +
  ylab("amplitude (pA)")

# Cort admin ---------------------

source(file.path(scriptsFolder, "cortAdmin-get-datasets.R"))


## pilot -----
BD_comboNutALPS <- rbind(
  BD_cortALPS %>%
    mutate(
      atePrevNutella = NA,
      ateNutella = NA,
      cortNutTrt = adultTrt
    ), 
  BD_cort4 %>% 
    filter(Sac_date == date_parse("2022-05-27")) %>%
    mutate(
      cortNutTrt = dosage
    )
)

nutellaALPS_cortPlot <- BD_comboNutALPS %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    plotMean = FALSE, # we don't have enough data yet for mean/se to be helpful
    plotSE = FALSE,
    groupVar = cortNutTrt, # group by the dosage; this is why it needs to be a factor
    xBreaks = c(0, 0.5, 1, 3, 5), # set the axis ticks at these values
    xLabels = c("0", "0.5", "1", "3", "5"),
    lineTypeGuide = "",
    positionDodge = 0.2, #this controls the spread of the data visually
    zoom_y = TRUE,
    ymax = 1000,
    ymin = 0
  ) +
  # facet_wrap(
  #   ~ateAllNutella # this splits the graph by those that did or did not each Nutella
  # ) +
  labs(
    x = "time (h)" # add a title to the x axis
  ) + 
  theme(
    legend.position = "top"
  )  +
  scale_color_manual(
    "treatment",
    labels = c(
      "ALPS" = "ALPS",
      "0" = "0mg/kg",
      "2" = "2mg/kg"
    ),
    values = c(
      "ALPS"="black",
      "0" = "black",
      "2" = "black"
    )
  )+
  scale_fill_manual(
    "treatment",
    labels = c(
      "ALPS" = "ALPS",
      "0" = "0mg/kg",
      "2" = "2mg/kg"
    ),
    values = c(
      "ALPS"="grey80",
      "0" = "white",
      "2" = "black"
    )
  )+
  scale_shape_manual(
    "treatment",
    labels = c(
      "ALPS" = "ALPS",
      "0" = "0mg/kg",
      "2" = "2mg/kg"
    ),
    values = c(
      "ALPS"=23,
      "0" = 21,
      "2" = 21
    )
  ) + scale_linetype_manual(
    "treatment",
    labels = c(
      "ALPS" = "ALPS",
      "0" = "0mg/kg",
      "2" = "2mg/kg"
    ),
    values = c(
      "ALPS"="dotted",
      "0" = "solid",
      "2" = "solid"
    )
  )


## serum cort - final dataset ------
cortAdminCortDF <- BD_cort %>%
  filter(
    time %in% c(0, 5)
    , is.na(exclude) | exclude == FALSE
    , trust == TRUE
  )

cortAdmin_cort <- cortAdminCortDF %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    groupVar = dosage, # group by the dosage; this is why it needs to be a factor
    lineTypeGuide = c("solid", "dotted"),
    positionDodge = 1.8, #this controls the spread of the data visually
    zoom_y = TRUE,
    ymax = 300,
    ymin = 0,
    zoom_x = TRUE
    , xmin = -1.5
    , xmax = 6.5
  ) +
  facet_wrap(
    ~ dosage,
    labeller = labeller(dosage = c("0" = "0mg/kg", "2" = "2mg/kg"))
  ) +
  # labs(
  #   x = "time since 1st administration (h)" # add a title to the x axis
  # ) + 
  theme(
    legend.position = "none"
  )  + 
  dosageFillShape()

## Max LH -----

cortAdmin_sampling_trust <- BD_sampling %>%
  filter(
    trust == TRUE
    , !is.na(maxLH)
  )

cortAdmin_LHamp <- cortAdmin_sampling_trust %>%
  plotLHAmp_dosage(
    surgeMin = 3
    , textSize = textSize
    , dotSize = dotSize
    , textAngle = 0
  )

## LH plot -----

cortAdmin_LH_trust <- BD_LH %>%
  filter(
    trust == TRUE
    , !is.na(LH)
  )
cortAdmin_LHplot <- cortAdmin_LH_trust %>%
  LHPlot_adultTrt_color(
    trtVar = dosage
    , trtName = "dosage (mg/kg)"
    , trtLineGuide = c("0" = "solid", "2" = "solid")
    , fontSize = textSize
    , dotSize = 1.5
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 45
  ) + facet_wrap(
    ~ dosage
    , ncol = 2
    , labeller = labeller(
      dosage = c("0" = "0 mg/kg", "2" = "2 mg/kg")
    )
  ) + 
  rremove("legend")

## Ovulation ------
BD_ovulation_plot <- BD_sampling %>%
  filter(
    trust == TRUE
  ) %>%
  propOvulatedPlot(
    xVar = dosage,
    fontSize = textSize
    , labelFontSize = 16
  ) +
  xlab("dosage (mg/kg)")
