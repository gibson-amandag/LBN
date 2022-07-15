behavior_overTime <- function(
  df,
  yVar,
  yLab,
  timeBreaks = c(9, 14, 19, 0, 4),
  timeLabels = c("ZT9", "ZT14", "ZT19", "ZT0", "ZT4"),
  fontSize = 12,
  dotSize = 1.2,
  dotSize_byFirstDay = FALSE,
  lineSize = 1
){
  viz <- df %>%
    mutate(
      time = factor(time, levels = timeBreaks, labels = timeLabels)
    ) %>%
    ggplot(
      aes(
        x = time,
        y = {{ yVar }},
        fill = earlyLifeTrt
      )
    ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = damID, linetype = earlyLifeTrt),
      position = position_dodge(0.4),
      size = lineSize
    )
  
  if(dotSize_byFirstDay){
    viz <- viz + geom_point(
      alpha = 1, 
      aes(fill=earlyLifeTrt,group=damID, shape = firstDay), 
      position = position_dodge(0.4),
      size = dotSize
    )+ scale_shape_manual(
      values = c(21, 22)
    )
  } else {
    viz <- viz + geom_point(
      shape = 21,
      alpha = 1, 
      aes(fill=earlyLifeTrt,group=damID), 
      position = position_dodge(0.4), 
      size = dotSize
    )
  }
  
  viz <- viz +
    addMeanHorizontalBar(addLineType = TRUE)+
    addMeanSE_vertBar() +
    labs(y = yLab, linetype = "early life trt") +
    earlyLifeFill() +
    textTheme(
      size = fontSize
    )+
    boxTheme()
  return(viz)
}
behavior_overTime_dodge <- function(
  df,
  yVar,
  yLab,
  timeBreaks = c(15, 1),
  timeLabels = c("15", "1"),
  fontSize = 12,
  dotSize = 1.2,
  dotSize_byFirstDay = FALSE,
  lineSize = 1,
  dodgeVal = 0.4
){
  viz <- df %>%
    mutate(
      time = factor(time, levels = timeBreaks, labels = timeLabels)
    ) %>%
    ggplot(
      aes(
        x = time,
        y = {{ yVar }},
        fill = earlyLifeTrt,
        group = earlyLifeTrt
      )
    ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(
        linetype = earlyLifeTrt
        , group = interaction(damID, earlyLifeTrt) 
      ),
      position = position_dodge(dodgeVal),
      size = lineSize
    )
  
  if(dotSize_byFirstDay){
    viz <- viz + geom_point(
      alpha = 1, 
      aes(
        fill=earlyLifeTrt
        , group = interaction(damID, earlyLifeTrt)
        , shape = firstDay
        ), 
      position = position_dodge(dodgeVal),
      size = dotSize
    )+ scale_shape_manual(
      values = c(21, 22)
    )
  } else {
    viz <- viz + geom_point(
      shape = 21,
      alpha = 1, 
      aes(
        fill=earlyLifeTrt
        , group = interaction(damID, earlyLifeTrt)
        ), 
      position = position_dodge(dodgeVal), 
      size = dotSize
    )
  }
  
  viz <- viz +
    addMeanHorizontalBar(
      addLineType = TRUE
      , barPosition = position_dodge(dodgeVal)
      , width = 0.8
    )+
    addMeanSE_vertBar(
      barPosition = position_dodge(dodgeVal)
    ) +
    labs(y = yLab, linetype = "early life trt") +
    earlyLifeFill() +
    textTheme(
      size = fontSize
    )+
    boxTheme()
  return(viz)
}
