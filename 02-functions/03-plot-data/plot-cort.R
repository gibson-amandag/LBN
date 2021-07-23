cortPlot <- function(
  df_long
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
    scale_fill_manual(values = c("white", "white", "black", "black")) +
    scale_shape_manual(values = c(21, 23, 21, 23)) +
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "corticosterone (ng/mL)",
      fill = "treatment",
      shape = "treatment"
    ) +
    scale_x_continuous(
      breaks = c(0, 5),
      labels = c("pre", "post")
    ) +
    textTheme()+
    boxTheme()+
    guides(linetype = "none")
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
        y = {{ yVar }}
      )
    ) +
    geom_jitter() +
    expand_limits(x = 0, y = 0) +
    labs(x = "uterine mass (mg)", y = yLab)+
    textTheme()+
    boxTheme()
  return(plot)
}
