# when using this with sapply in vectors, 
# it seems to simplify to a number
makeDateTime <- function(PND, hr){
  as_datetime(
    ymd_h(
      paste0(
        "2000-01-"
        , sprintf("%02d", as.integer(PND))
        , " "
        , sprintf("%02d", as.integer(hr))
      )
    )
  )
}


# This function will detect if PND or ZT are columns
# in the provided dataset.
# If they are, they will include them in the plot
# If neither are included, will plot trt on the x-axis
# Only color by treatment, and always use a horizontal
# line for the mean

# This function should meet the needs of most of the others
# in this file, with more flexibility
plotDamBehavior <- function(
    df
    , yVar
    , yLab
    , fontSize = 12
    , dotSize = 1.2
    , lineSize = 1
    , lineAlpha = 0.4
    , dodgeVal = 0.4
    , addTriangleForMean = FALSE
    , redMean = FALSE
    , colorByDam = FALSE
    , showDots = TRUE
    , addVertError = TRUE
    , facetByTrt = TRUE
    , facetByLitter = FALSE
    , removeLegend = TRUE
    , STDColor = "black"
    , LBNColor = "black"
    , STDFill = "grey30"
    , LBNFill = "cyan4"
    , zoom_x = FALSE # Zoom to part of x axis
    , xmin = NULL
    , xmax = NULL
    , zoom_y = FALSE # Zoom to part of y axis
    , ymin = NULL
    , ymax = NULL
){
  df <- df %>%
    rename(
      any_of(
        c(ZT = "time") # renames time as ZT, if it exists
      )
    )
  
  includesPND <- ifelse("PND" %in% names(df), TRUE, FALSE)
  includesZT <- ifelse("ZT" %in% names(df), TRUE, FALSE)
  
  if(includesPND & includesZT){
    df <- df %>%
      mutate(
        dayTime = as_datetime(ymd_h(paste0(paste0("2000-01-", PND), paste0(" ", ZT)))),
        .after = ZT
      )
    
    days <- c(4:11)
    dateTimes01 <- sapply(days, makeDateTime, 1)
    dateTimes15 <- sapply(days, makeDateTime, 15)
    dateTimes <- as_datetime(c(rbind(dateTimes01, dateTimes15)))
    
    viz <- df %>%
      ggplot(
        aes(
          x = dayTime,
          y = {{ yVar }}
        )
      ) + scale_x_datetime(
        breaks = c(
          dateTimes
        ), 
        date_labels = "%d\n%H"
      ) +
      xlab("PND\nZT")
  } else {
    if(includesPND){
      viz <- df %>%
        ggplot(
          aes(
            x = PND
            , y = {{ yVar }}
          )
        ) +
        xlab("PND")
    } else if(includesZT){
      viz <- df %>%
        ggplot(
          aes(
            x = ZT
            , y = {{ yVar }}
          )
        ) + 
        xlab("ZT") +
        expand_limits(x = c(0, 23))
    } else {
      viz <- df %>%
        ggplot(
          aes(
            x = earlyLifeTrt
            , y = {{ yVar }}
            , fill = earlyLifeTrt
          )
        ) +
        theme(
          axis.title.x = element_blank()
        ) + 
        earlyLifeFill(
          STDColor = STDFill
          , LBNColor = LBNFill
        )
    }
  }
  
  if(includesPND | includesZT){
    if(colorByDam){
      viz <- viz + geom_line(
        alpha = lineAlpha,
        aes(group = damID, color = damID),
        position = position_dodge(dodgeVal),
        size = lineSize
      )
      if(showDots){
        viz <- viz + geom_point(
          shape = 21,
          alpha = 1, 
          aes(color=damID, fill = damID, group=damID), 
          position = position_dodge(dodgeVal), 
          size = dotSize
        )
      }
    } else {
      viz <- viz + geom_line(
        alpha = lineAlpha,
        # color = "black",
        aes(group = damID, color = earlyLifeTrt),
        position = position_dodge(dodgeVal),
        size = lineSize
      )
      if(showDots){
        viz <- viz + geom_point(
          shape = 21,
          alpha = 1, 
          aes(fill=earlyLifeTrt,group=damID), 
          position = position_dodge(dodgeVal), 
          size = dotSize
        )
      } 
      viz <- viz +
        earlyLifeFill(
          STDColor = STDFill
          , LBNColor = LBNFill
        ) +
        earlyLifeColor(
          STDColor = STDColor
          , LBNColor = LBNColor
        )
    }
    
    if(addTriangleForMean){ # horizontal bar is too small to show up
      if(redMean){
        viz <- viz +
          stat_summary(
            geom = "point",
            fun = mean,
            aes(group = earlyLifeTrt),
            shape = 24,
            color = "red",
            fill = "red",
            size = dotSize * 1.25
            , position = position_dodge(dodgeVal) # if want the means to dodge, too
          )
      }else {
        viz <- viz +
          stat_summary(
            geom = "point",
            fun = mean,
            shape = 24,
            aes(fill = earlyLifeTrt, group = earlyLifeTrt),
            size = dotSize * 1.25
            , position = position_dodge(dodgeVal)
          )
      }
    }
  } else {
    # if don't have a time-based x-asis, always show dots
    viz <- viz + geom_point(
      shape = 21,
      alpha = 1, 
      aes(fill=earlyLifeTrt,group=damID), 
      position = position_dodge(dodgeVal), 
      size = dotSize
    )
  }
  
  if(addVertError){
    viz <- viz + addMeanSE_vertBar(
      # linetype = earlyLifeTrt,
      group = earlyLifeTrt
      , barPosition = position_dodge(dodgeVal)
    )
  }
  
  litterNum_label <- labeller(litterNum = c("1" = "first litter", "2" = "second litter"))
  
  colorMean <- FALSE
  if((includesPND | includesZT)){
    if(facetByTrt & facetByLitter){
      viz <- viz +
        facet_wrap(
          vars(litterNum, earlyLifeTrt)
          , labeller = litterNum_label
        )
    } else if(facetByTrt){
      viz <- viz + 
        facet_wrap(
          vars(earlyLifeTrt)
          , ncol = 2
        )
    } else if(facetByLitter){
      viz <- viz +
        facet_wrap(
          vars(litterNum)
          , labeller = litterNum_label
        ) 
      colorMean <- TRUE
    }
  } else {
    if(facetByLitter){
      viz <- viz +
        facet_wrap(
          vars(litterNum)
          , labeller = litterNum_label
        )
    }
  }
  
  if(removeLegend){
    viz <- viz +
      theme(
        legend.position = "none"
      )
  }
  
  if(colorMean){
    viz <- viz + addMeanHorizontalBar(
      addLineType = FALSE
      , group = earlyLifeTrt
      , color = earlyLifeTrt
      , barPosition = position_dodge(dodgeVal)
    )
  } else {
    viz <- viz +
      addMeanHorizontalBar(
        addLineType = FALSE
        , group = earlyLifeTrt
        , barPosition = position_dodge(dodgeVal)
      )
  }
  
  viz <- viz +
    labs(y = yLab, linetype = "early life trt") +
    textTheme(
      size = fontSize
    )+
    expand_limits(y = 0)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})
  
  return(viz)
}



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
  redMean = FALSE,
  colorByDam = FALSE,
  lineAlpha = 0.4,
  showDots = TRUE,
  addVertError = TRUE
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
    )
  
  if(colorByDam){
    viz <- viz + geom_line(
      alpha = lineAlpha,
      aes(group = damID, color = damID),
      position = position_dodge(0.4),
      size = lineSize
    )
  } else {
    viz <- viz + geom_line(
      alpha = lineAlpha,
      color = "black",
      aes(group = damID, linetype = earlyLifeTrt),
      position = position_dodge(0.4),
      size = lineSize
    )
    
  }
  
  if(showDots){
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
  
  if(addVertError){
    viz <- viz + addMeanSE_vertBar()
  }
  
  viz <- viz +
    addMeanHorizontalBar(addLineType = FALSE)+
    labs(y = yLab, linetype = "early life trt") +
    earlyLifeFill() +
    textTheme(
      size = fontSize
    )+
    boxTheme() + 
    scale_x_datetime(
      breaks = c(
        # as_datetime(ymd_h("2000-01-04 15")),
        as_datetime(ymd_h("2000-01-05 01")),
        # as_datetime(ymd_h("2000-01-05 15")),
        as_datetime(ymd_h("2000-01-06 01")),
        # as_datetime(ymd_h("2000-01-06 15")),
        as_datetime(ymd_h("2000-01-07 01")),
        # as_datetime(ymd_h("2000-01-07 15")),
        as_datetime(ymd_h("2000-01-08 01")),
        # as_datetime(ymd_h("2000-01-08 15")),
        as_datetime(ymd_h("2000-01-09 01")),
        # as_datetime(ymd_h("2000-01-09 15")),
        as_datetime(ymd_h("2000-01-10 01")),
        # as_datetime(ymd_h("2000-01-10 15")),
        as_datetime(ymd_h("2000-01-11 01"))
      ), 
      # date_labels = "PND%d\nZT%H"
      date_labels = "%d\n%H"
    )
  return(viz)
}

## note - can't position dodge with date/time because it's a continous variable
## https://stackoverflow.com/questions/59857137/how-to-make-position-dodge-and-scale-x-date-work-together
plotDamFrame_days <- function(
  df,
  yVar,
  yLab,
  fontSize = 12,
  dotSize = 1.2,
  dotSize_byFirstDay = FALSE,
  lineSize = 1,
  dodgeVal = 0.4,
  addTriangleForMean = FALSE,
  redMean = FALSE,
  colorByDam = FALSE,
  lineAlpha = 0.4,
  showDots = TRUE,
  addVertError = TRUE
){
  viz <- df %>%
    mutate(
      dayTime = as_datetime(ymd_h(paste0(paste0("2000-01-", PND), paste0(" ", 0)))),
      .after = PND
    ) %>%
    ggplot(
      aes(
        x = dayTime,
        y = {{ yVar }},
        fill = earlyLifeTrt
      )
    )
  
  if(colorByDam){
    viz <- viz + geom_line(
      alpha = lineAlpha,
      aes(group = damID, color = damID),
      position = position_dodge(0.4),
      size = lineSize
    )
  } else {
    viz <- viz + geom_line(
      alpha = lineAlpha,
      color = "black",
      aes(group = damID, linetype = earlyLifeTrt),
      position = position_dodge(0.4),
      size = lineSize
    )
    
  }
  
  if(showDots){
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
    } else {
      viz <- viz +
        stat_summary(
          geom = "point",
          fun = mean,
          shape = 24,
          size = dotSize
        )
    }
  }
  
  if(addVertError){
    viz <- viz + addMeanSE_vertBar()
  }
  
  viz <- viz +
    addMeanHorizontalBar(addLineType = FALSE)+
    labs(y = yLab, linetype = "early life trt", x = "PND") +
    earlyLifeFill() +
    textTheme(
      size = fontSize
    )+
    boxTheme() + 
    scale_x_datetime(
      breaks = c(
        as_datetime(ymd_h("2000-01-04 0")),
        as_datetime(ymd_h("2000-01-05 0")),
        as_datetime(ymd_h("2000-01-06 0")),
        as_datetime(ymd_h("2000-01-07 0")),
        as_datetime(ymd_h("2000-01-08 0")),
        as_datetime(ymd_h("2000-01-09 0")),
        as_datetime(ymd_h("2000-01-10 0")),
        as_datetime(ymd_h("2000-01-11 0"))
      ), 
      # date_labels = "PND%d\nZT%H"
      date_labels = "%d"
      # date_labels = "%d\n%H"
    )
  return(viz)
}
