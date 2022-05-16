scatterPlot_general <- function(
  df,
  xVar,
  xLab,
  yVar,
  yLab,
  fillVar = NULL,
  fillLimits = NULL,
  fillValues = NULL,
  lineColorVar = NULL,
  lineColorLimits = NULL,
  lineColorValues = NULL,
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
  title = NULL,
  addMean = TRUE,
  addSE = TRUE
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xVar }},
        y = {{ yVar }},
        fill = {{ fillVar }},
        color = {{ lineColorVar }}
      )
    ) +
    jitterGeom(
      size = dotSize,
      alpha = fillAlpha,
      width = jitterWidth,
      height = jitterHeight
    ) +
    labs(title = title)+
    theme_pubr()+
    expand_limits(y=0)+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})+
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )+
    textTheme(size = textSize)+
    boxTheme()

  if(addMean){
    viz <- viz + addMeanHorizontalBar()
  }
  if(addSE){
    viz <- viz + addMeanSE_vertBar()
  }

  if(!is.null(enquo(lineColorVar))){
    viz <- viz + scale_color_manual(limits = lineColorLimits, values = lineColorValues)
  }
  if(!is.null(enquo(fillVar))){
    viz <- viz + scale_fill_manual(limits = fillLimits, values = fillValues)
  }

  if(!is.null(xLab)){
    viz <- viz + labs(x = xLab)
  }

  if(!is.null(yLab)){
    viz <- viz + labs(y = yLab)
  }
  
  return(viz)
}

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
  xLab,
  textSize = 11,
  dotSize = 1.5
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xVar }},
        y = {{ yVar }},
        fill = earlyLifeTrt
      )
    ) +
    jitterGeom(size = dotSize) +
    labs(y = yLab, x = xLab)+
    expand_limits(x = 0, y = 0)+
    earlyLifeFill() +
    textTheme(textSize)+
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
    jitterGeom(size = dotSize, width = 0) +
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
  fontSize = 11,
  addMeanSE = TRUE,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL
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
    labs(y = yLab)+
    comboTrtFillShape() +
    theme_pubr()+
    expand_limits(y=0)+
    coord_cartesian(if(zoom_y){ylim = c(ymin, ymax)}) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )+
    textTheme(size = fontSize)+
    boxTheme()
  
  if(addMeanSE){
    viz <- viz +
      addMeanHorizontalBar() +
      addMeanSE_vertBar()
  }
  
  return(viz)
}

getNiceName <- function(
  varName # as expression or as string, both work
  , labelDF = niceNames #data frame with labels
){
  label <- as.character(varName) # default to name of variable
  if(label %in% colnames(labelDF))
  {
    df <- labelDF %>%
      select(
        {{ varName }}
      )
    label <- df[[1]]
  }
  return(label)
}


