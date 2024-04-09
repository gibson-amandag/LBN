# Cort plots --------------

## pilot 1 ------------
nutellaCortPlot1 <- BD_cort1 %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    plotMean = FALSE, # we don't have enough data yet for mean/se to be helpful
    plotSE = FALSE,
    groupVar = dosage, # group by the dosage; this is why it needs to be a factor
    xBreaks = c(0, 0.5, 1, 3, 5), # set the axis ticks at these values
    xLabels = c("0", "0.5", "1", "3", "5"), # label the axis with the actual time values
    lineTypeGuide = "",
    positionDodge = 0.2 #this controls the spread of the data visually
  ) +
  facet_wrap(
    ~ateAllNutella # this splits the graph by those that did or did not eat Nutella
  ) +
  labs(
    x = "time since administration (h)" # add a title to the x axis
  ) +
  pilotDosageFillShape(c(0, 10, 20, 50))

nutellaCortPlot1_ateAll <- BD_cort1 %>%
  filter(
    ateAllNutella == "ate all Nutella"
  ) %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    plotMean = FALSE, # we don't have enough data yet for mean/se to be helpful
    plotSE = FALSE,
    groupVar = dosage, # group by the dosage; this is why it needs to be a factor
    xBreaks = c(0, 0.5, 1, 3, 5), # set the axis ticks at these values
    xLabels = c("0", "0.5", "1", "3", "5"), # label the axis with the actual time values
    lineTypeGuide = "",
    positionDodge = 0.2 #this controls the spread of the data visually
  ) +
  facet_wrap(
    ~ateAllNutella # this splits the graph by those that did or did not eat Nutella
  ) +
  labs(
    x = "time since administration (h)" # add a title to the x axis
  ) +
  pilotDosageFillShape(c(0, 10, 20, 50))

## pilot 2 ---------------
nutellaCortPlot2 <- BD_cort2 %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    plotMean = FALSE, # we don't have enough data yet for mean/se to be helpful
    plotSE = FALSE,
    groupVar = dosage, # group by the dosage; this is why it needs to be a factor
    xBreaks = c(0, 0.5, 1, 3, 5), # set the axis ticks at these values
    xLabels = c("0", "0.5", "1", "3", "5"), # label the axis with the actual time values
    lineTypeGuide = "",
    positionDodge = 0.2 #this controls the spread of the data visually
  ) +
  facet_wrap(
    ~ateAllNutella # this splits the graph by those that did or did not eat Nutella
  ) +
  labs(
    x = "time since administration (h)" # add a title to the x axis
  ) +
  pilotDosageFillShape(c(0, 3, 10))

## pilot 3 ---------------
nutellaCortPlot3<- BD_cort3 %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    plotMean = FALSE, # we don't have enough data yet for mean/se to be helpful
    plotSE = FALSE,
    groupVar = dosage, # group by the dosage; this is why it needs to be a factor
    xBreaks = c(0, 0.5, 1, 3, 5), # set the axis ticks at these values
    xLabels = c("0", "0.5", "1", "3", "5"), # label the axis with the actual time values
    lineTypeGuide = "",
    positionDodge = 0.2 #this controls the spread of the data visually
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 1000
  ) +
  facet_wrap(
    ~ateAllNutella # this splits the graph by those that did or did not eat Nutella
  ) +
  labs(
    x = "time since administration (h)" # add a title to the x axis
  ) +
  pilotDosageFillShape(c(0, 0.5, 1))

## comp to ALPS -------------

nutellaALPS_cortPlot <- BD_comboNutALPS %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    plotMean = FALSE,
    plotSE = FALSE,
    groupVar = cortNutTrt, 
    xBreaks = c(0, 0.5, 1, 3, 5),
    xLabels = c("0", "0.5", "1", "3", "5"),
    lineTypeGuide = "",
    positionDodge = 0.2,
    zoom_y = TRUE,
    ymax = 720,
    ymin = 0
  ) +
  labs(
    x = "time since 1st administration (h)" # add a title to the x axis
  ) + 
  theme(
    legend.position = c(c(0.3, 1))
    , legend.direction = "horizontal"
  ) +
  scale_y_continuous(
    breaks = seq(0, 750, 125)
  )

scaleLabels <- c(
  "ALPS" = "ALPS",
  "0" = "0mg/kg",
  "2" = "2mg/kg"
)

if(isManuscript){
  nutellaALPS_cortPlot <- nutellaALPS_cortPlot +
    scale_color_manual(
      "treatment",
      labels = scaleLabels,
      values = c(
        "ALPS"="cyan3",
        "0" = "#CCCCCC",
        "2" = "#666666"
      )
    ) +
    scale_shape_manual(
      "treatment",
      labels = scaleLabels,
      values = c(
        "ALPS"=18,
        "0" = 16,
        "2" = 16
      )
    )
} else{
  nutellaALPS_cortPlot <- nutellaALPS_cortPlot +
    scale_color_manual(
      "treatment",
      labels = scaleLabels,
      values = c(
        "ALPS"="black",
        "0" = "black",
        "2" = "black"
      )
    ) +
    scale_fill_manual(
      "treatment",
      labels = scaleLabels,
      values = c(
        "ALPS"="cyan3",
        "0" = "white",
        "2" = "#666666"
      )
    )+
    scale_shape_manual(
      "treatment",
      labels = scaleLabels,
      values = c(
        "ALPS"=23,
        "0" = 21,
        "2" = 21
      )
    )
}

nutellaALPS_cortPlot <- nutellaALPS_cortPlot +
  scale_linetype_manual(
    "treatment",
    labels = scaleLabels,
    values = c(
      "ALPS"="dotted",
      "0" = "solid",
      "2" = "solid"
    )
  )

# Cort LMM ----------

cortAdmin_cort_lmm <- mixed(
  log10(cort) ~ dosage * time + (1|mouseID) + (1|damID)
  , cortAdminCortDF
  , method = "KR"
)


cortAdmin_cort_lmm_emm <- emmeans(
  cortAdmin_cort_lmm
  , "dosage"
  , by = "time"
  , type = "response"
)

cortAdmin_cort_lmm_emm.pairs <- contrast(
  cortAdmin_cort_lmm_emm
  , "pairwise"
  , simple = "each"
  , combine = TRUE
  , adjust = "holm"
)

cortAdmin_cort_lmm_emm.pairs %>%
  simplifyEMMPairsOutput() %>%
  makeManuscriptFlexTable(
    round2Cols = c("ratio", "t ratio")
    , round3Cols = c("SEM")
    , round1Cols = c("df")
  )

## errors for graph ---------------
cortAdmin_cort_lmm_error <- cortAdmin_cort_lmm_emm %>%
  as_data_frame() %>%
  rename(
    y = response
  ) %>%
  mutate(
    lower = y - SE
    , upper = y + SE
  )

# Final dataset ------------


cortAdmin_cort <- cortAdminCortDF %>%
  left_join(
    cortAdmin_sampling_trust %>%
      select(
        mouseID
        , maxLH
      )
    , by = "mouseID"
  ) %>%
  mutate(
    surgeStatus = 
      case_when(
        dosage == "0" & maxLH > surgeMin ~ "0surge",
        dosage == "0" & maxLH <= surgeMin ~ "0noSurge",
        dosage == "2" & maxLH > surgeMin ~ "2surge",
        TRUE ~ "2noSurge"
      )
  ) %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    groupVar = surgeStatus, # group by the dosage; this is why it needs to be a factor
    lineTypeGuide = c("solid", "solid", "solid", "solid"),
    positionDodge = 2, #this controls the spread of the data visually
    zoom_y = TRUE,
    ymax = 300,
    ymin = 0,
    zoom_x = TRUE
    , xmin = -1.5
    , xmax = 6.5
    , plotMean = FALSE
    , plotSE = FALSE
  ) +
  facet_wrap(
    ~ dosage,
    labeller = labeller(dosage = c("0" = "0mg/kg", "2" = "2mg/kg"))
    # , strip.position = "bottom"
  ) +
  # labs(
  #   x = "time since 1st administration (h)" # add a title to the x axis
  # ) + 
  theme(
    legend.position = "none"
  )  + 
  scale_color_manual(
    "treatment", 
    values = c("0surge"=ifelse(isManuscript, "#CCCCCC", "black")
               , "2surge"=ifelse(isManuscript, "#666666", "black")
               , "0noSurge" = ifelse(isManuscript, "#FF0099", "black")
               , "2noSurge" = ifelse(isManuscript, "#FF0099", "black")
               ),
    labels = c("0surge" = "0mg/kg surged"
               , "2surge" = "2mg/kg surged"
               , "0noSurge" = "0mg/kg no surge"
               , "2noSurge" = "2mg/kg no surge"
               )
  ) +
  scale_fill_manual(
    "treatment", 
    values = c("0surge"=ifelse(isManuscript, "#CCCCCC", "white")
               , "2surge"="#666666"
               , "0noSurge" = "#FF0099"
               , "2noSurge" = "#FF0099"
    ),
    labels = c("0surge" = "0mg/kg surged"
               , "2surge" = "2mg/kg surged"
               , "0noSurge" = "0mg/kg no surge"
               , "2noSurge" = "2mg/kg no surge"
    )
  ) +
  scale_shape_manual(
    "treatment", 
    values = c("0surge"=ifelse(isManuscript, 16, 21)
               , "2surge"=ifelse(isManuscript, 16, 21)
               , "0noSurge"=ifelse(isManuscript, 16, 21)
               , "2noSurge"=ifelse(isManuscript, 16, 21)
               ),
    labels = c("0surge" = "0mg/kg surged"
               , "2surge" = "2mg/kg surged"
               , "0noSurge" = "0mg/kg no surge"
               , "2noSurge" = "2mg/kg no surge"
    )
  ) +
  plotError_LMM(
    cortAdmin_cort_lmm_error
    , xVar = time
    , meanBarWidth = 2.8
  ) 

## Max LH -----

cortAdmin_sampling_trust <- BD_sampling %>%
  filter(
    trust == TRUE
    , !is.na(maxLH)
  )

cortAdmin_LHamp <- cortAdmin_sampling_trust %>%
  plotLHAmp_dosage(
    textSize = textSize
    , dotSize = dotSize
  )

## LH plot -----

cortAdmin_LH_trust <- BD_LH %>%
  filter(
    trust == TRUE
    , !is.na(LH)
  )


cortAdmin_LHSamplingDF_color <- cortAdmin_sampling_trust %>%
  filter(
    maxLH >= surgeMin
  ) %>%
  group_by(dosage) %>%
  mutate(
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
    cortAdmin_LH_trust %>%
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

cortAdmin_LH_overTime <- cortAdmin_LHSamplingDF_color %>%
  plotLH_overTime_color() +
  facet_wrap(
    ~ dosage
    , scales = "free"
    , labeller = labeller(
      dosage = c("0" = "0mg/kg", "2" = "2mg/kg")
    )
  )

## Ovulation ------
cortAdmin_ovulation_plot <- cortAdmin_sampling_trust %>%
  propOvulatedPlot(
    xVar = dosage,
    fontSize = textSize
    , labelFontSize = 4
  ) +
  scale_x_discrete(
    labels = c("0mg/kg", "2mg/kg")
  )

### prop chi-square ---------

contTable <- table(
  cortAdmin_sampling_trust$dosage
  , cortAdmin_sampling_trust$ovulated
)

contTable

propSurged.Chi.Sq.res <- chisq_test(contTable) 

propSurged.Chi.Sq.descriptives <-  chisq_descriptives(propSurged.Chi.Sq.res)

# Combined plot -----------

cortAdminLeft <- align_plots(
  nutellaALPS_cortPlot
  , cortAdmin_cort
  , align = "v"
  , axis = "l"
)

cortAdminPlot_row1 <- plot_grid(
  cortAdminLeft[[1]]
  , cortAdmin_LH_overTime
  , nrow = 1
  , align = "h"
  , axis = "tb"
  , rel_widths = c(1, 2)
  , labels = c("A", "C")
  , label_fontfamily = "Arial"
  , label_size = textSize
)

cortAdminPlot_row2 <- plot_grid(
  cortAdminLeft[[2]]
  , cortAdmin_LHamp
  , cortAdmin_ovulation_plot
  , nrow = 1
  # , rel_widths = c(2, 1, 1)
  , labels = c("B", "D", "E")
  , align = "h"
  , axis = "tb"
  , label_fontfamily = "Arial"
  , label_size = textSize
)

cortAdminPlot <- plot_grid(
  cortAdminPlot_row1
  , cortAdminPlot_row2
  , nrow = 2
)

plotFolder <- file.path(plotOutputFolder, "dissertation")
flexSave(
  "figCortAdmin_females"
  , plot = cortAdminPlot
  , width = twoCols
  , height = twoThirdsLength
  , units = "cm"
  , filePath = plotFolder
)

# cortAdminLeft <- align_plots(
#   cortAdmin_cort
#   , cortAdmin_LH_overTime
#   , align = "v"
#   , axis = "l"
# )
# 
# 
# cortAdminPlot_row1 <- plot_grid(
#   cortAdminLeft[[1]]
#   , cortAdmin_LHamp
#   , cortAdmin_ovulation_plot
#   , nrow = 1
#   , align = "h"
#   , axis = "tb"
#   , labels = c("A", "B", "C")
#   , label_fontfamily = "Arial"
#   , label_size = textSize
# )
# 
# cortAdminPlot <- plot_grid(
#   cortAdminPlot_row1
#   , cortAdminLeft[[2]]
#   , nrow = 2
#   , labels = c("", "D")
#   , label_fontfamily = "Arial"
#   , label_size = textSize
# )