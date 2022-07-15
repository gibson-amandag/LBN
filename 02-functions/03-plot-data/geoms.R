#' Add a jitter geom to a ggplot
#' 
#' Defaults to a circle with fill and outline (shape 21). Provides defaults for
#' size, alpha (fill density), jitter width, and jitter height
#'
#' @param size a number. Size of the shape. Default is 1.5
#' @param alpha a number. density of fill. Default is 1
#' @param width a number. jitter width - spread of the data horizontally. Default is 0.35
#' @param height a number. jitter height - spread of the data vertically. Default is 0
#'
#' @return a geom_jitter with selected values
#' @export
#'
#' @examples
jitterGeom <- function(
  size = 1.5,
  alpha = 1,
  width = 0.35,
  height = 0
){
  geom_jitter(
    shape = 21,
    size = size,
    alpha = alpha,
    width = width,
    height = height
  ) 
}

addMeanHorizontalBar <- function(
  width = 0.7,
  size = 0.4,
  addLineType = FALSE,
  lineTypeName = "early life trt",
  lineTypeGuide = c("STD" = "dotted", "LBN" = "solid"),
  typeVar = earlyLifeTrt,
  barPosition = "identity",
  ... # Into aes
){
  if(!addLineType){
    geom <- stat_summary(
        geom = "errorbar", 
        fun.min = mean, 
        fun = mean, 
        fun.max = mean, 
        width = width,
        size = size,
        position = barPosition,
        aes(...)
      )
  } else{
    geom <- list(
      stat_summary(
        geom = "errorbar", 
        fun.min = mean, 
        fun = mean, 
        fun.max = mean, 
        width = width,
        size = size,
        position = barPosition,
        aes(linetype = {{ typeVar }}, ...)
      ),
      scale_linetype_manual(lineTypeName, values = lineTypeGuide)
    )
  }
}

addMeanSE_vertBar <- function(
  size = 0.4,
  barPosition = "identity",
  ... # into aes
){
  stat_summary(
    geom = "linerange", 
    fun.data = mean_se,
    size = size,
    position = barPosition,
    aes(...),
    show.legend = FALSE
  )
}

addMedianHorizontalBar <- function(
  width = 0.7,
  size = 0.4,
  color = "red",
  alpha = 0.7
){
  stat_summary(
    geom = "errorbar", 
    fun.min = median, 
    fun = median, 
    fun.max = median, 
    width = width,
    size = size,
    color = color,
    alpha = alpha
  )
}



# Lines -------------------------------------------------------------------

my_geom_line <- function(
  useLineType = TRUE,
  lineTypeVar,
  lineGroupVar,
  indivLineAlpha = 0.5,
  indivLineSize = 0.8
){
  # aesList <- list(
  #   if(useLineType) linetype = {{ lineTypeVar }},
  #   group = {{ lineGroupVar }}
  # )
  list(
    geom_line(
      alpha = indivLineAlpha, # semi-transparent
      if(useLineType){
        aes(linetype = {{ lineTypeVar }},
            group = {{ lineGroupVar }})
      } else{
        aes(group = {{ lineGroupVar }})
      },
      # aes(
      #   if(useLineType){linetype = {{ lineTypeVar }}},
      #   group = {{ lineGroupVar }}
      #   # aesList
      # ),
      size = indivLineSize
    )
  )
}

mean_geom_line <- function(
  useLineType, #TRUE/FALSE
  lineTypeVar,
  errorBarWidth = 1,
  meanLineSize = 1.4,
  meanAlpha = 1,
  errorBarSize = 1,
  # errorBarColor = "grey10",
  errorBarAlpha = 0.6
){
  list(
    stat_summary(
      fun = mean, 
      geom = "line", 
      if(useLineType){aes(linetype = {{ lineTypeVar }})}, 
      size = meanLineSize, 
      alpha = meanAlpha
    ),
    stat_summary(
      geom = "errorbar", 
      fun.data = mean_se, 
      if(useLineType){aes(group = {{ lineTypeVar }})}, 
      size = errorBarSize, 
      width = errorBarWidth, 
      # color = errorBarColor, 
      alpha = errorBarAlpha
    )
  )
}

plot_line_geom_layers <- function(
  useLineType, # TRUE/FALSE
  lineTypeVar,
  lineGroupVar,
  xtitle, #x axis label
  ytitle, #y axis label
  title = NULL, # plot title
  individualLines = TRUE, # plot individual lines
  meanLines = TRUE, # plot mean lines with SE
  zoom_x = FALSE, # Zoom to part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL,
  indivLineAlpha = 0.5,
  indivLineSize = 0.8,
  errorBarWidth = 1,
  meanLineSize = 1.4,
  meanAlpha = 1,
  errorBarSize = 1,
  # errorBarColor = "grey10",
  errorBarAlpha = 0.6,
  textSize = 11,
  axisSize = 0.5
){
  list(
    labs(
      x = xtitle,
      y = ytitle,
      title = title
    ),
    if(individualLines)
      my_geom_line(
        # Will always plot based on lineTypeVar
        useLineType = useLineType,
        lineTypeVar = {{ lineTypeVar }},
        lineGroupVar = {{ lineGroupVar }},
        indivLineAlpha = indivLineAlpha,
        indivLineSize = indivLineSize
      ),
    if(meanLines)
      mean_geom_line(
        useLineType = useLineType,
        lineTypeVar = {{ lineTypeVar }},
        errorBarWidth = errorBarWidth,
        meanLineSize = meanLineSize,
        meanAlpha = meanAlpha,
        errorBarSize = errorBarSize,
        # errorBarColor = errorBarColor,
        errorBarAlpha = errorBarAlpha
      ),
    expand_limits(y = 0),
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}),
    textTheme(size = textSize),
    boxTheme(axisSize = axisSize)
  )
}
