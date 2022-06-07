cortPlot <- function(
  df_long,
  pointSize = 1.2,
  fontSize = 11,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL,
  plotMean = TRUE,
  plotSE = TRUE,
  xBreaks = c(0, 5),
  xLabels = c("pre", "post"),
  lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
  positionDodge = 1.2,
  groupVar = comboTrt
){
  viz <- ggplot(
    df_long,
    aes(
      x = time,
      y = cort,
      group =  {{ groupVar }}
    )
  ) +
    geom_line(
      alpha = 0.4,
      # color = "black",
      aes(group = mouseID, linetype =  {{ groupVar }}, color =  {{ groupVar }}),
      position = position_dodge(positionDodge)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill= {{ groupVar }},group=mouseID, shape= {{ groupVar }}, color= {{ groupVar }}), 
      position = position_dodge(positionDodge), 
      size = pointSize
    ) +
    theme_pubr() +
    expand_limits(y = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    # rremove("xlab") + ## seems like can't add back after if do this
    labs(
      y = "corticosterone (ng/mL)",
      x = NULL
    ) +
    scale_x_continuous(
      breaks = xBreaks, #c(0, 5),
      labels = xLabels, #c("pre", "post")
    ) +
    textTheme(size = fontSize)+
    boxTheme()+
    guides()#linetype = "none")
  
  if(plotMean){
    viz <- viz + addMeanHorizontalBar(
      width = 1.4, 
      addLineType = TRUE,
      lineTypeName = "treatment",
      # lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
      lineTypeGuide = lineTypeGuide,
      typeVar= {{ groupVar }},
      color= {{ groupVar }}
    )
  } else {
    viz <- viz + labs(linetype = "treatment")
  }
  
  if(plotSE){
    viz <- viz + addMeanSE_vertBar(color= {{ groupVar }})
  }
  
  if(deparse(substitute(groupVar)) == "comboTrt"){
    viz <- viz + comboTrtFillShape()
  } else {
    viz <- viz + labs(color = "treatment", shape = "treatment", fill = "treatment")
  }
  return(viz)
}

testCort <- function(
  df_long,
  pointSize = 1.2,
  fontSize = 11,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL,
  lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
  positionDodge = 1.2,
  groupVar = comboTrt
){
  viz <- ggplot(
    df_long,
    aes(
      x = time,
      y = cort,
      group = {{ groupVar }}
    )
  ) + 
    geom_line(
      alpha = 0.4,
      aes(group = mouseID, linetype = {{groupVar}}, color = {{groupVar}}),
      position = position_dodge(positionDodge)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill={{groupVar}},group=mouseID, shape={{groupVar}}, color={{groupVar}}), 
      position = position_dodge(positionDodge), 
      size = pointSize
    ) +
    addMeanHorizontalBar(
      width = 1.4,
      addLineType = FALSE,
      lineTypeName = "treatment",
      lineTypeGuide = lineTypeGuide,
      typeVar={{ groupVar }},
      color={{ groupVar }}
    )+
    addMeanSE_vertBar(color={{ groupVar }})+
    theme_pubr() +
    expand_limits(y = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    rremove("xlab") +
    textTheme(size = fontSize)+
    boxTheme()+
    guides()
  return(viz)
}

baseCortPlot <- function(
  df_long,
  dotSize = 1.2
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = cort,
      group = interaction(earlyLifeTrt, adultTrt)
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = mouseID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt, color=comboTrt), 
      position = position_dodge(0.4), 
      size = dotSize
      ) +
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = TRUE,
      lineTypeName = "treatment",
      lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
      typeVar=comboTrt,
      color=comboTrt
    )+
    addMeanSE_vertBar(color=comboTrt)+
    comboTrtFillShape()+
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "corticosterone (ng/mL)",
      fill = "treatment"
    ) +
    scale_x_continuous(
      breaks = c(0, 5),
      labels = c("pre", "post")
    )
}

longCortPlot <- function(
  basePlot,
  fontSize = 11,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  longPlot <- basePlot +
    facet_wrap(
      # ~ comboTrt,
      ~ earlyLifeTrt + adultTrt,
      strip.position = "bottom",
      ncol = 4,
      nrow = 1
    ) +
    expand_limits(y = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    rremove(
      "legend"
    ) + 
    textTheme(size = fontSize)+
    boxTheme()
  return(longPlot)
}

plotByUterineMass <- function(
  df,
  yVar,
  yLab,
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  plot <- df %>%
    ggplot(
      aes(
        x = ReproTract_mass,
        y = {{ yVar }},
        fill = comboTrt,
        shape = comboTrt,
        color = comboTrt,
      )
    ) +
    geom_jitter(size = dotSize) +
    expand_limits(x = 0, y = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    labs(x = "uterine mass (mg)", y = yLab)+
    comboTrtFillShape()+
    theme_pubr()+
    textTheme(size = fontSize)+
    boxTheme()
  return(plot)
}

plotUterineMassByGroup <- function(
  df,
  showHline = TRUE,
  hLineVal = 140,
  xGroupVar = comboTrt,
  fontSize = 11,
  dotSize = 1.2
){
  viz <- df %>%
    ggplot(
      aes(
        y = ReproTract_mass,
        x = {{ xGroupVar }},
        fill = comboTrt,
        shape = comboTrt
      )
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    comboTrtFillShape()+
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "uterine mass (mg)"
    )+
    expand_limits(y = 0)+
    textTheme(size = fontSize)+
    boxTheme()
  
  if(showHline){
    viz <- viz + geom_hline(yintercept = hLineVal, color = "red")
  }
  return(viz)
}

LHPlot <- function(
  df_long,
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = LH,
      group = comboTrt
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = mouseID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    addMeanHorizontalBar(width = 0.85, addLineType = TRUE)+
    addMeanSE_vertBar()+
    comboTrtFillShape()+
    theme_pubr() +
    labs(
      y = "LH (ng/mL)",
      x = "time (hr) relative to lights out"
    ) +
    scale_x_continuous(
      breaks = c(0, 5, 5.5, 6.5, 7.5, 8.5),
      labels = c(-7.5, "", -2, -1, 0, 1)
    )+
    textTheme(size = fontSize)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) + #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    guides(linetype = "none")
}
LHPlot_noMean <- function(
  df_long,
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = LH,
      group = comboTrt
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = mouseID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    # addMeanHorizontalBar(width = 0.85, addLineType = TRUE)+
    # addMeanSE_vertBar()+
    comboTrtFillShape()+
    theme_pubr() +
    labs(
      y = "LH (ng/mL)",
      x = "time (hr) relative to lights out"
    ) +
    scale_x_continuous(
      breaks = c(0, 5, 5.5, 6.5, 7.5, 8.5),
      labels = c(-7.5, "", -2, -1, 0, 1)
    )+
    textTheme(size = fontSize)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) + #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    guides(linetype = "none")
}

LHPlot_adultTrt <- function(
  df_long,
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = LH,
      group = adultTrt
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = mouseID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      shape = 21,
      alpha = 1, 
      aes(fill=adultTrt
          ,group=mouseID
          # ,shape=adultTrt
          ), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = TRUE,
      lineTypeName = "adult trt",
      lineTypeGuide = c("CON" = "dotted", "ALPS" = "solid"),
      typeVar = adultTrt
    )+
    addMeanSE_vertBar()+
    adultTrtFill()+
    adultTrtColor()+
    # scale_color_manual(
    #     "treatment", 
    #     values = c("CON" = "black"
    #                # ,"ALPS" = "darkcyan"
    #                ,"ALPS" = "black"
    #                )
    #     )+
    # scale_fill_manual(
    #   "treatment", 
    #   values = c("CON" = "white"
    #              # ,"ALPS" = "darkcyan"
    #              ,"ALPS" = "black"
    #              )
    #   )+
    # scale_shape_manual(
    #   "treatment", 
    #   values = c("CON" = 21,
    #              "ALPS" = 23))+
    theme_pubr() +
    labs(
      y = "LH (ng/mL)"
    ) +
    textTheme(size = fontSize)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) + #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    guides(linetype = "none")
}

propSurgedPlot <- function(
  df,
  xVar = comboTrt,
  fontSize = 11
){
  viz <- ggplot(df, aes(x = {{ xVar }}, fill = surged))+
    geom_bar(position = "fill", color = "black") +
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.3, colour = "darkgrey", position = "fill")+
    labs(y = "proportion with LH surge") + 
    scale_fill_manual(values = c("white", "black")) +
    # facet_wrap("comboTrt",
    #            strip.position = "bottom"
    # ) +
    theme_pubr() +
    textTheme(size = fontSize)+
    boxTheme()+
    rremove("legend") +
    rremove("xlab")
  return(viz)
}
propSurgedPlotCombo <- function(
  df,
  fontSize = 11
){
  viz <- df %>%
    ggplot(aes(x = comboTrt, fill = interaction(comboTrt, surged), color = interaction(comboTrt, surged)))+
      geom_bar(position = "fill") +
      # geom_text(
      #   aes(label = ..count..),
      #   stat = "count",
      #   vjust = 1.3,
      #   colour = "darkgrey",
      #   position = "fill"
      # )+
      labs(y = "proportion with LH surge") + 
      # scale_fill_manual(values = c("white", "black")) +
      # facet_wrap("comboTrt",
      #            strip.position = "bottom"
      # ) +
      scale_color_manual(values = c("white", "white", "white", "white", "black", "black", "darkcyan", "darkcyan"))+
      scale_fill_manual(values = c("white", "white", "white", "white", "white", "black", "lightblue1", "darkcyan")) +
      theme_pubr() +
      textTheme(size = fontSize)+
      boxTheme()+
      rremove("legend") +
      rremove("xlab")
  return(viz)
}

plotLHAmp <- function(
  df, 
  surgeMin, 
  textSize = 11, 
  dotSize = 2
){
  plot <- df %>%
    mutate(
      surgeStatus = 
        case_when(
          adultTrt == "CON" ~ "control",
          adultTrt == "ALPS" & maxLH > surgeMin ~ "stress surge",
          TRUE ~ "stress no surge"
        )
    ) %>%
    mutate(
      surgeStatus = factor(surgeStatus, levels = c("control", "stress surge", "stress no surge"))
    ) %>%
    ggplot(
      aes(
        x = surgeStatus,
        y = maxLH,
        fill = surgeStatus
      )
    ) +
    geom_point(
      alpha = 1,
      position = position_dodge2(0.4),
      size = dotSize,
      shape = 21,
      color = "black"
    )+
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = FALSE
    ) +
    addMeanSE_vertBar()+
    scale_fill_manual(
      values = c("control" = "white", 
                 "stress surge" = "black", 
                 "stress no surge" = "grey60"
      )
    )+
    boxTheme()+
    textTheme(textSize) +
    ylab("LH (ng/mL)")+
    scale_x_discrete(
      labels = c("Control", "Stress\nsurge", "Stress  \nno surge")
    )+
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
    )
  return(plot)
}
plotLHAmp_comboTrt <- function(
  df, 
  surgeMin, 
  textSize = 11, 
  dotSize = 2,
  angleX = TRUE
){
  plot <- df %>%
    mutate(
      surgeStatus = 
        case_when(
          adultTrt == "CON" & maxLH > surgeMin ~ "control surge",
          adultTrt == "CON" & maxLH <= surgeMin ~ "control no surge",
          adultTrt == "ALPS" & maxLH > surgeMin ~ "stress surge",
          TRUE ~ "stress no surge"
        )
    ) %>%
    mutate(
      surgeStatus = factor(surgeStatus, levels = c("control surge", "control no surge", "stress surge", "stress no surge"))
    ) %>%
    ggplot(
      aes(
        x = surgeStatus,
        y = maxLH,
        fill = comboTrt
      )
    ) +
    geom_point(
      alpha = 1,
      position = position_dodge2(0.4),
      size = dotSize,
      shape = 21,
      color = "black"
    )+
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = FALSE
    ) +
    addMeanSE_vertBar()+
    # scale_fill_manual(
    #   values = c("control" = "white", 
    #              "stress surge" = "black", 
    #              "stress no surge" = "grey60"
    #   )
    # )+
    comboTrtFillShape() +
    boxTheme()+
    textTheme(textSize) +
    ylab("LH (ng/mL)")+
    facet_wrap(
      ~earlyLifeTrt,
      scales = "free_y"
    )
  
  if(angleX){
    plot <- plot +
      scale_x_discrete(
        labels = c("CON \nsurge", "CON  \nno surge", "ALPS \nsurge", "ALPS  \nno surge")
      )+
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
      )
  } else {
    plot <- plot +
      scale_x_discrete(
        labels = c("CON\nsurge", "CON\nno\nsurge", "ALPS\nsurge", "ALPS\nno\nsurge")
      )+
      theme(
        legend.position = "none",
        axis.title.x = element_blank()
      )
  }
  return(plot)
}