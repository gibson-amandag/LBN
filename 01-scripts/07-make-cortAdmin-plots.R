BD_comboNutALPS <- rbind(
  BD_cortALPS %>%
    mutate(
      atePrevNutella = NA,
      ateNutella = NA,
      cortNutTrt = adultTrt
    ), 
  BD_cort4 %>% 
    filter(Sac_date == as_date("2022-05-27")) %>%
    mutate(
      cortNutTrt = dosage
    )
)

BD_comboNutALPS_plot <- BD_comboNutALPS %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    plotMean = FALSE, # we don't have enough data yet for mean/se to be helpful
    plotSE = FALSE,
    groupVar = cortNutTrt, # group by the dosage; this is why it needs to be a factor
    xBreaks = c(0, 0.5, 1, 3, 5), # set the axis ticks at these values
    xLabels = waiver(), # label the axis with the actual time values
    lineTypeGuide = "",
    positionDodge = 0.2, #this controls the spread of the data visually
    zoom_y = TRUE,
    ymax = 1000,
    ymin = 0
  ) +
  labs(
    x = "time since 1st administration (h)" # add a title to the x axis
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
      "ALPS"="black",
      "0" = "#F5C6F1",
      "2" = "#F127DF"
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

BD_ALPS_plot <- BD_comboNutALPS %>%
  filter(
    cortNutTrt == "ALPS"
  ) %>%
  cortPlot(
    pointSize = dotSize,
    fontSize = textSize,
    plotMean = FALSE, # we don't have enough data yet for mean/se to be helpful
    plotSE = FALSE,
    groupVar = cortNutTrt, # group by the dosage; this is why it needs to be a factor
    xBreaks = c(0, 0.5, 1, 3, 5), # set the axis ticks at these values
    xLabels = waiver(), # label the axis with the actual time values
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
    x = "time since 1st administration (h)" # add a title to the x axis
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
      "ALPS"="black",
      "0" = "#F5C6F1",
      "2" = "#F127DF"
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