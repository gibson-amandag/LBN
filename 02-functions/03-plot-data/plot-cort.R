cortPlot <- function(
  df_long,
  pointSize = 1.2,
  fontSize = 11
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = cort,
      group = comboTrt
    )
  ) +
    geom_line(
      alpha = 0.4,
      # color = "black",
      aes(group = mouseID, linetype = comboTrt, color = comboTrt),
      position = position_dodge(1.2)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt, color=comboTrt), 
      position = position_dodge(1.2), 
      size = pointSize
    ) +
    addMeanHorizontalBar(
      width = 1.4, 
      addLineType = TRUE,
      lineTypeName = "treatment",
      lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
      typeVar=comboTrt,
      color=comboTrt
      )+
    addMeanSE_vertBar(color=comboTrt)+
    comboTrtFillShape()+
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "corticosterone (ng/mL)"
    ) +
    scale_x_continuous(
      breaks = c(0, 5),
      labels = c("pre", "post")
    ) +
    textTheme(size = fontSize)+
    boxTheme()+
    guides()#linetype = "none")
}
baseCortPlot <- function(
  df_long,
  dotSize = 1.2
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = cort,
      group = interaction(earlyLifeTrt, adultTrt)
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = mouseID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt, color=comboTrt), 
      position = position_dodge(0.4), 
      size = dotSize
      ) +
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = TRUE,
      lineTypeName = "treatment",
      lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
      typeVar=comboTrt,
      color=comboTrt
    )+
    addMeanSE_vertBar(color=comboTrt)+
    comboTrtFillShape()+
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "corticosterone (ng/mL)",
      fill = "treatment"
    ) +
    scale_x_continuous(
      breaks = c(0, 5),
      labels = c("pre", "post")
    )
}

longCortPlot <- function(
  basePlot,
  fontSize = 11
){
  longPlot <- basePlot +
    facet_wrap(
      ~ comboTrt,
      strip.position = "bottom",
      ncol = 4,
      nrow = 1
    ) +
    rremove(
      "legend"
    ) + 
    textTheme(size = fontSize)+
    boxTheme()
  return(longPlot)
}

plotByUterineMass <- function(
  df,
  yVar,
  yLab,
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  plot <- df %>%
    ggplot(
      aes(
        x = ReproTract_mass,
        y = {{ yVar }},
        fill = comboTrt,
        shape = comboTrt,
        color = comboTrt,
      )
    ) +
    geom_jitter(size = dotSize) +
    expand_limits(x = 0, y = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    labs(x = "uterine mass (mg)", y = yLab)+
    comboTrtFillShape()+
    theme_pubr()+
    textTheme(size = fontSize)+
    boxTheme()
  return(plot)
}

plotUterineMassByGroup <- function(
  df,
  showHline = TRUE,
  hLineVal = 140,
  xGroupVar = comboTrt,
  fontSize = 11,
  dotSize = 1.2
){
  viz <- df %>%
    ggplot(
      aes(
        y = ReproTract_mass,
        x = {{ xGroupVar }},
        fill = comboTrt,
        shape = comboTrt
      )
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    comboTrtFillShape()+
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "uterine mass (mg)"
    )+
    expand_limits(y = 0)+
    textTheme(size = fontSize)+
    boxTheme()
  
  if(showHline){
    viz <- viz + geom_hline(yintercept = hLineVal, color = "red")
  }
  return(viz)
}

LHPlot <- function(
  df_long,
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = LH,
      group = comboTrt
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = mouseID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    addMeanHorizontalBar(width = 0.85, addLineType = TRUE)+
    addMeanSE_vertBar()+
    comboTrtFillShape()+
    theme_pubr() +
    labs(
      y = "LH (ng/mL)"
    ) +
    textTheme(size = fontSize)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) + #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    guides(linetype = "none")
}

propSurgedPlot <- function(
  df,
  xVar = comboTrt,
  fontSize = 11
){
  viz <- ggplot(df, aes(x = {{ xVar }}, fill = surged))+
    geom_bar(position = "fill", color = "black") +
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.3, colour = "darkgrey", position = "fill")+
    labs(y = "proportion with LH surge") + 
    scale_fill_manual(values = c("white", "black")) +
    # facet_wrap("comboTrt",
    #            strip.position = "bottom"
    # ) +
    theme_pubr() +
    textTheme(size = fontSize)+
    boxTheme()+
    rremove("legend") +
    rremove("xlab")
  return(viz)
}
