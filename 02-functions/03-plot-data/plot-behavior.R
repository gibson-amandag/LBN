behavior_overTime <- function(
  df,
  yVar,
  yLab,
  timeBreaks = c(9, 14, 19, 0),
  timeLabels = c("ZT9", "ZT14", "ZT19", "ZT0"),
  fontSize = 12,
  dotSize = 1.2
){
  viz <- df %>%
    mutate(
      time = factor(time, levels = c(9, 14, 19, 0), labels = timeBreaks)
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
      position = position_dodge(0.4)
    ) +
    geom_point(
      shape = 21,
      alpha = 1, 
      aes(fill=earlyLifeTrt,group=damID), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
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
