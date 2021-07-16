jitterGeom <- function(
  size = 1.2,
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
  lineTypeGuide = c("STD" = "dotted", "LBN" = "solid"),
  typeVar = earlyLifeTrt
){
  if(!addLineType){
    geom <- stat_summary(
        geom = "errorbar", 
        fun.min = mean, 
        fun = mean, 
        fun.max = mean, 
        width = width,
        size = size
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
        aes(linetype = {{ typeVar }})
      ),
      scale_linetype_manual(values = lineTypeGuide)
    )
  }
}

addMeanSE_vertBar <- function(
  size = 0.4
){
  stat_summary(
    geom = "linerange", 
    fun.data = mean_se,
    size = size
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