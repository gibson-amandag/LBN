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


## note - can't position dodge with date/time because it's a continous variable
## https://stackoverflow.com/questions/59857137/how-to-make-position-dodge-and-scale-x-date-work-together
behavior_overTime_days <- function(
  df,
  yVar,
  yLab,
  fontSize = 12,
  dotSize = 1.2,
  dotSize_byFirstDay = FALSE,
  lineSize = 1,
  dodgeVal = 0.4,
  addTriangleForMean = FALSE,
  redMean = FALSE
){
  # dayTimeBreaks <- c()
  # days <- c(4:11)
  # 
  # for (time in timeBreaks) {
  #   for (day in days){
  #     dayTimeBreaks <- append(dayTimeBreaks, (day-1)*24 + time)
  #   }
  # }
  
  viz <- df %>%
    mutate(
      dayTime = as_datetime(ymd_h(paste0(paste0("2000-01-", PND), paste0(" ", time)))),
      .after = time
    ) %>%
    ggplot(
      aes(
        x = dayTime,
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
  
  if(addTriangleForMean){ # horizontal bar is too small to show up
    if(redMean){
      viz <- viz +
        stat_summary(
          geom = "point",
          fun = mean,
          shape = 16,
          color = "red",
          size = dotSize * .5
        )
    }else {
      viz <- viz +
        stat_summary(
          geom = "point",
          fun = mean,
          shape = 24,
          size = dotSize
        )
    }
  }
  
  viz <- viz +
    addMeanHorizontalBar(addLineType = FALSE)+
    addMeanSE_vertBar() +
    labs(y = yLab, linetype = "early life trt") +
    earlyLifeFill() +
    textTheme(
      size = fontSize
    )+
    boxTheme() + 
    scale_x_datetime(
      breaks = c(
        as_datetime(ymd_h("2000-01-04 15")),
        as_datetime(ymd_h("2000-01-05 01")),
        as_datetime(ymd_h("2000-01-05 15")),
        as_datetime(ymd_h("2000-01-06 01")),
        as_datetime(ymd_h("2000-01-06 15")),
        as_datetime(ymd_h("2000-01-07 01")),
        as_datetime(ymd_h("2000-01-07 15")),
        as_datetime(ymd_h("2000-01-08 01")),
        as_datetime(ymd_h("2000-01-08 15")),
        as_datetime(ymd_h("2000-01-09 01")),
        as_datetime(ymd_h("2000-01-09 15")),
        as_datetime(ymd_h("2000-01-10 01")),
        as_datetime(ymd_h("2000-01-10 15")),
        as_datetime(ymd_h("2000-01-11 01"))
      ), 
      # date_labels = "PND%d\nZT%H"
      date_labels = "%d\n%H"
    )
  return(viz)
}
