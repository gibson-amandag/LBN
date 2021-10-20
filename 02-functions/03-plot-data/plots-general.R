scatterPlotLBN <- function(
  df,
  yVar,
  yLab,
  STDColor = "white",
  LBNColor = "cyan4",
  textSize = 12,
  zoom_x = FALSE, # Zoom to part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL,
  dotSize = 1.5,
  fillAlpha = 1,
  jitterWidth = 0.35,
  jitterHeight = 0,
  title = NULL
){
  viz <- df %>%
    ggplot(
      aes(
        x = earlyLifeTrt,
        y = {{ yVar }},
        fill = earlyLifeTrt
      )
    ) +
    jitterGeom(
      size = dotSize,
      alpha = fillAlpha,
      width = jitterWidth,
      height = jitterHeight
    ) +
    addMeanHorizontalBar() +
    addMeanSE_vertBar()+
    labs(y = yLab, title = title)+
    earlyLifeFill(STDColor = STDColor, LBNColor = LBNColor) +
    theme_pubr()+
    expand_limits(y=0)+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})+
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )+
    textTheme(size = textSize)+
    boxTheme()
  
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

scatterPlotTwoVars_byComboTrt <- function(
  df,
  yVar,
  yLab,
  xVar,
  xLab,
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xVar }},
        y = {{ yVar }},
        fill = comboTrt,
        shape = comboTrt
      )
    ) +
    jitterGeom() +
    labs(y = yLab, x = xLab)+
    expand_limits(x = 0, y = 0)+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    comboTrtFillShape()+
    theme_pubr()+
    textTheme(size = fontSize)+
    boxTheme()
  
  return(viz)
}

scatterPlotComboTrt <- function(
  df,
  yVar,
  yLab,
  dotSize = 1.2,
  fontSize = 11
){
  viz <- df %>%
    filter(
      !is.na({{ yVar }})
    ) %>%
    ggplot(
      aes(
        x = comboTrt,
        y = {{ yVar }},
        fill = comboTrt,
        shape = comboTrt
      )
    ) +
    jitterGeom(size = dotSize) +
    addMeanHorizontalBar() +
    addMeanSE_vertBar()+
    labs(y = yLab)+
    comboTrtFillShape() +
    theme_pubr()+
    expand_limits(y=0)+
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )+
    textTheme(size = fontSize)+
    boxTheme()
  
  return(viz)
}