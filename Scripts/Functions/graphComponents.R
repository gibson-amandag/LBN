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

jitterGeom_shapeAes <- function(
  size = 1.2,
  alpha = 1,
  width = 0.35,
  height = 0
){
  geom_jitter(
    size = size,
    alpha = alpha,
    width = width,
    height = height
  ) 
}

scale_shape_color <- function(){
  list(
    scale_shape_manual(
    values = c(21, 22, 21, 22),
    name = "treatment"
  ),
  scale_fill_manual(
    values = c("white", "white", "black", "black"),
    name = "treatment"
  )
  )
}

textTheme <- function(size = 11){
  theme = theme(
    text = element_text(size = size, family = "Arial"),
    strip.text = element_text(face = "bold", size = size),
    axis.title = element_text(face = "bold")
  )
  return(theme)  
}

boxTheme <- function(axisSize = 0.5){
  theme = theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = axisSize),
    strip.background = element_blank(),
    strip.placement = "outside"
  )
}

addMeanHorizontalBar <- function(
  width = 0.7,
  size = 0.4
){
  stat_summary(
    geom = "errorbar", 
    fun.min = mean, 
    fun = mean, 
    fun.max = mean, 
    width = width,
    size = size
  )
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