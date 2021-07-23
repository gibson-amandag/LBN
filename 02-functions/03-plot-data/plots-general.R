scatterPlotLBN <- function(
  df,
  yVar,
  yLab
){
  viz <- df %>%
    ggplot(
      aes(
        x = earlyLifeTrt,
        y = {{ yVar }},
        fill = earlyLifeTrt
      )
    ) +
    jitterGeom() +
    addMeanHorizontalBar() +
    addMeanSE_vertBar()+
    labs(y = yLab)+
    earlyLifeFill() +
    textTheme()+
    boxTheme() +
    expand_limits(y=0)+
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )
  
  return(viz)
}

scatterPlotTwoVars_byLBN <- function(
  df,
  yVar,
  yLab,
  xVar,
  xLab
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xVar }},
        y = {{ yVar }},
        fill = earlyLifeTrt
      )
    ) +
    jitterGeom() +
    labs(y = yLab, x = xLab)+
    expand_limits(x = 0, y = 0)+
    earlyLifeFill() +
    textTheme()+
    boxTheme()
  
  return(viz)
}