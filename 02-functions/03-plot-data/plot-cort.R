cortPlot <- function(
  df_long,
  pointSize = 1.2
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
      aes(group = Mouse_ID, linetype = comboTrt, color = comboTrt),
      position = position_dodge(1.2)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=Mouse_ID, shape=comboTrt, color=comboTrt), 
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
    textTheme()+
    boxTheme()+
    guides()#linetype = "none")
}
baseCortPlot <- function(
  df_long
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
      aes(group = Mouse_ID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      shape = 21,
      alpha = 1, 
      aes(fill=earlyLifeTrt,group=Mouse_ID), 
      position = position_dodge(0.4), 
      size = 1.2
      ) +
    addMeanHorizontalBar(width = 0.85)+
    addMeanSE_vertBar()+
    scale_fill_manual(values = c("white", "black")) +
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
  basePlot
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
    textTheme()+
    boxTheme()
  return(longPlot)
}

plotByUterineMass <- function(
  df,
  yVar,
  yLab
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
    geom_jitter() +
    expand_limits(x = 0, y = 0) +
    labs(x = "uterine mass (mg)", y = yLab)+
    comboTrtFillShape()+
    theme_pubr()+
    textTheme()+
    boxTheme()
  return(plot)
}

plotUterineMassByGroup <- function(
  df,
  showHline = TRUE,
  hLineVal = 140,
  xGroupVar = comboTrt
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
      aes(fill=comboTrt,group=Mouse_ID, shape=comboTrt), 
      position = position_dodge(0.4), 
      size = 1.2
    ) +
    comboTrtFillShape()+
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "uterine mass (mg)"
    )+
    expand_limits(y = 0)+
    textTheme()+
    boxTheme()
  
  if(showHline){
    viz <- viz + geom_hline(yintercept = hLineVal, color = "red")
  }
  return(viz)
}

LHPlot <- function(
  df_long
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
      aes(group = Mouse_ID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=Mouse_ID, shape=comboTrt), 
      position = position_dodge(0.4), 
      size = 1.2
    ) +
    addMeanHorizontalBar(width = 0.85, addLineType = TRUE)+
    addMeanSE_vertBar()+
    comboTrtFillShape()+
    theme_pubr() +
    labs(
      y = "LH (ng/mL)"
    ) +
    textTheme()+
    boxTheme()+
    guides(linetype = "none")
}