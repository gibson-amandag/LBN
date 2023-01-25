textTheme <- function(size = 11){
  theme = theme(
    text = element_text(size = size, family = "Arial", color = "black"),
    axis.text = element_text(size = size, family = "Arial", color = "black"), # for some reason, not acquiring consistently
    strip.text = element_text(face = "bold", size = size),
    axis.title = element_text(face = "bold")
  )
  return(theme)  
}

boxTheme <- function(axisSize = 0.5){
  theme = theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = axisSize),
    axis.ticks = element_line(color = "black"),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.background = element_blank(), # testing, unsure if allowed
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(color = "black", fill = NA, inherit.blank = TRUE),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
}

earlyLifeFill <- function(
  STD = "STD",
  LBN = "LBN",
  STDColor = "white",
  LBNColor = "cyan4"
){
  fill <- scale_fill_manual(
    "early life trt", 
    values = c(STD=STDColor, LBN=LBNColor)
  )
  return(fill)
}

earlyLifeColor <- function(
  STD = "STD",
  LBN = "LBN",
  STDColor = "grey30",
  LBNColor = "cyan4"
){
  color <- scale_color_manual(
    "early life trt", 
    values = c(STD = STDColor, LBN = LBNColor)
  )
  return(color)
}

adultTrtFill <- function(
  CON = "CON",
  ALPS = "ALPS",
  CONColor = "white",
  ALPSColor = "black"
){
  fill <- scale_fill_manual(
    "early life trt", 
    values = c(CON=CONColor, ALPS=ALPSColor)
  )
  return(fill)
}

adultTrtColor <- function(
  CON = "CON",
  ALPS = "ALPS",
  CONColor = "black",
  ALPSColor = "black"
){
  color <- scale_color_manual(
    "early life trt", 
    values = c(CON = CONColor, ALPS = ALPSColor)
  )
  return(color)
}

adultTrtFillShape <- function(
  CON_color = "black",
  ALPS_color = "black",
  CON_fill = "white",
  ALPS_fill = "black"
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = c("CON"=CON_color, 
                 "ALPS"=ALPS_color)),
    scale_fill_manual(
      "treatment", 
      values = c("CON"=CON_fill, 
                 "ALPS"=ALPS_fill)),
    scale_shape_manual(
      "treatment", 
      values = c("CON"=21, 
                 "ALPS"=23))
  )
  return(layers)
}

earlyLifeLineType <- function(
  STD = "STD",
  LBN = "LBN"
){
  lineType <- scale_linetype_manual(
    "early life trt", 
    values = c(STD = "dashed", LBN = "solid")
  )
}

comboTrtFillShape <- function(
  STD_CON_color = "black",
  STD_ALPS_color = "black",
  LBN_CON_color = "darkcyan",
  LBN_ALPS_color = "darkcyan",
  STD_CON_fill = "white",
  STD_ALPS_fill = "black",
  LBN_CON_fill = "lightblue1",
  LBN_ALPS_fill = "darkcyan"
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = c("STD-CON"=STD_CON_color, 
                 "STD-ALPS"=STD_ALPS_color, 
                 "LBN-CON"=LBN_CON_color, 
                 "LBN-ALPS"=LBN_ALPS_color)),
    scale_fill_manual(
      "treatment", 
      values = c("STD-CON"=STD_CON_fill, 
                 "STD-ALPS"=STD_ALPS_fill, 
                 "LBN-CON"=LBN_CON_fill, 
                 "LBN-ALPS"=LBN_ALPS_fill)),
    scale_shape_manual(
      "treatment", 
      values = c("STD-CON"=21, 
                 "STD-ALPS"=23,
                 "LBN-CON"=21, 
                 "LBN-ALPS"=23))
  )
  return(layers)
}

dosageFillShape <- function(
  color_0 = "black",
  color_2 = "black",
  fill_0 = "white",
  fill_2 = "black"
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = c("0"=color_0, 
                 "2"=color_2)),
    scale_fill_manual(
      "treatment", 
      values = c("0"=fill_0, 
                 "2"=fill_2)),
    scale_shape_manual(
      "treatment", 
      values = c("0"=21,
                 "2"=21))
  )
  return(layers)
}

dosageColor <- function(
  color_0 = "black",
  color_2 = "black"
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = c("0"=color_0, 
                 "2"=color_2))
  )
  return(layers)
}

dosageFill <- function(
  fill_0 = "white",
  fill_2 = "black"
){
  layers <- list(
    scale_fill_manual(
      "treatment", 
      values = c("0"=fill_0, 
                 "2"=fill_2))
  )
  return(layers)
}