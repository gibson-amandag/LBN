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
  , meanWidth = 1.4
  , meanFollowsLineType = TRUE
  , yUnitsNewLine = FALSE
  , pointAlpha = 1
  , lineAlpha =1
  , lineSize = 0.25
  , clipVal = "on"
  , forManuscript = isManuscript
){
  if(yUnitsNewLine){
    yLab <- "corticosterone\n(ng/mL)"
  } else {
    yLab <- "corticosterone (ng/mL)"
  }
  
  viz <- ggplot(
    df_long,
    aes(
      x = time,
      y = cort,
      group =  {{ groupVar }}
    )
  ) +
    geom_line(
      alpha = lineAlpha,
      # color = "black",
      aes(group = mouseID, linetype =  {{ groupVar }}, color =  {{ groupVar }}),
      position = position_dodge(positionDodge)
      , linewidth = lineSize
      # https://github.com/eclarke/ggbeeswarm/issues/55
      # position = position_quasirandom() # 2023-06-18, I think there's an error here, and that this is the right path for this to work
    )
    
    # 2023-06-23: See above error note. Also, might be possible to still use geom_point with the position_quasirandom function instead
    # geom_quasirandom(
    #   alpha = 1, 
    #   aes(fill= {{ groupVar }}
    #       ,group=mouseID
    #       , shape= {{ groupVar }}
    #       , color= {{ groupVar }}), 
    #   # position = position_dodge(positionDodge), 
    #   size = pointSize
    # ) + 
  
  if(forManuscript){
    viz <- viz + geom_point(
      aes(
        group = mouseID
        , shape = {{ groupVar }}
        , color = {{ groupVar }}
      )
      , position = position_dodge(positionDodge)
      , size = pointSize
    )
  } else {
    viz <- viz + geom_point(
      color = "black"
      , aes(
        group = mouseID
        , fill = {{ groupVar }}
        , shape = {{ groupVar }}
      )
      , position = position_dodge(positionDodge)
      , size = pointSize
    )
  }
  
  viz <- viz +
    theme_pubr() +
    expand_limits(y = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}, clip = clipVal) +
    labs(
      y = yLab,
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
    if(meanFollowsLineType){
      viz <- viz + addMeanHorizontalBar(
        width = meanWidth, 
        addLineType = meanFollowsLineType,
        lineTypeName = "treatment",
        lineTypeGuide = lineTypeGuide,
        typeVar= {{ groupVar }},
        color= {{ groupVar }}
      )
    } else {
      viz <- viz + addMeanHorizontalBar(
        width = meanWidth
        , addLineType = FALSE
        , color = {{ groupVar }}
      ) +
        labs(linetype = "treatment") +
        scale_linetype_manual("treatment", values = lineTypeGuide)
    }
  } else {
    viz <- viz +
    labs(linetype = "treatment") +
      scale_linetype_manual("treatment", values = lineTypeGuide)
  }
  
  if(plotSE){
    viz <- viz + addMeanSE_vertBar(color= {{ groupVar }})
  }
  
  if(deparse(substitute(groupVar)) == "comboTrt"){
    viz <- viz + 
    comboTrtFillShape(fillAlpha = pointAlpha, forManuscript = forManuscript)
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
  , onlyLBN = FALSE
){
  if(onlyLBN){
    longPlot <- basePlot +
      facet_wrap(
        ~ earlyLifeTrt
        , strip.position = "bottom"
        , ncol = 2
      )
  } else {
    longPlot <- basePlot +
      facet_wrap(
        # ~ comboTrt,
        ~ earlyLifeTrt + adultTrt,
        strip.position = "bottom",
        ncol = 4,
        nrow = 1
      )
  }
  longPlot <- longPlot +
    expand_limits(y = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    rremove(
      "legend"
    ) + 
    textTheme(size = fontSize)+
    boxTheme()
  return(longPlot)
}


plotCortByNutellaConsumption <- function(
    df
    , fontSize = 11
    , zoom_x = FALSE #Zoom to a part of x axis
    , xmin = NULL
    , xmax = NULL
    , zoom_y = FALSE #Zoom to a part of y axis
    , ymin = NULL
    , ymax = NULL
    , yLab = "corticosterone (ng/mL)"
    , groupVar = atePrevNutella
    , groupLegendLabel = "prev Nutella"
    , dotSize = 3
    , shapeVector = c("NA" = 23, "none" = 24, "some" = 22, "all" = 21)
    , colorVector = c("NA" = "black", "none" = "black", "some" = "black", "all" = "black")
    , fillVector = c("NA" = "white", "none" = "lightpink", "some" = "lightblue", "all" = "black")
    , wrapPlot = TRUE
    , facetWrapStatement = facet_wrap(
        ~dosage
        , labeller = labeller(
          dosage = c("0" = "0 mg/kg", "2" = "2mg/kg")
        )
      )
){
  viz <- ggplot(
    df
    , aes(
      x = time
      , y = cort
      , group = {{ groupVar }}
    )
  ) +
    geom_line(
      alpha = 0.4
      , aes(group = mouseID)
      , position = position_dodge(0.4)
      # At some point, update to position_quasirandom, but need to adjust how it handles paths and points
    ) +
    geom_point(
      alpha = 1
      , aes(
        fill = {{ groupVar }}
        , group = mouseID
        , shape = {{ groupVar }}
        , color = {{ groupVar }}
      )
      , position = position_dodge(0.4)
      , size = dotSize
    ) +
    theme_pubr() +
    expand_limits(y = 0) +
    labs(
      y = yLab
      , x = NULL
    ) +
    scale_x_continuous(
      breaks = c(0, 1, 3, 5)
      , labels = c(0, 1, 3, 5)
    ) +
    textTheme(size = fontSize) +
    boxTheme() +
    guides()
  
  viz <- viz +
    labs(
      color = groupLegendLabel
      , shape = groupLegendLabel
      , fill = groupLegendLabel
    ) +
    scale_shape_manual(
      values = shapeVector
    ) +
    scale_color_manual(
      values = colorVector
    ) +
    scale_fill_manual(
      values = fillVector
    )
  
  if(wrapPlot)
  {
    viz <- viz + facetWrapStatement
  }
  
  return(viz)
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
    geom_jitter(size = dotSize, height = 0) + # AGG - added height = 0 to jitter 2023-07-05
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
  ymax = NULL,
  dodgeAmnt = 0.4
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
      position = position_dodge(dodgeAmnt)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt), 
      position = position_dodge(dodgeAmnt), 
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
      breaks = c(0, 5, 5.5, 6.5, 7.5, 8.5, 9.5),
      labels = c(-7.5, "", -2, -1, 0, 1, 2)
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
      position = position_dodge(0.15)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill=comboTrt,group=mouseID, shape=comboTrt), 
      position = position_dodge(0.15), 
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
      breaks = c(0, 5, 5.5, 6.5, 7.5, 8.5, 9.5),
      labels = c(-7.5, "", -2, -1, 0, 1, 2)
    )+
    textTheme(size = fontSize)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) + #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    guides(linetype = "none")
}

LHPlot_noMean_lineColor <- function(
  df_long,
  fontSize = 11,
  dotSize = 0.5,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
  , addPoint = TRUE
  , xBreaks = c(5, 5.5, 6.5, 7.5, 8.5, 9.5)
  , xLabels = c("", -2, -1, 0, 1, 2)
  , clipVar = "on"
  , lineAlpha = 1
  , lineSize = 0.3
){
  plot <- ggplot(
    df_long,
    aes(
      x = time,
      y = LH,
      group = comboTrt
    )
  ) +
    geom_line(
      alpha = lineAlpha,
      aes(group = mouseID, color = color),
      position = position_dodge(0.15),
      linewidth = lineSize
    )
  
  if(addPoint){
    plot <- plot +
      geom_point(
        alpha = 1
        ## Normal comboTrt fill
        # , aes(fill=comboTrt,group=mouseID, shape=comboTrt)
        
        , shape = 21
        ## Fill by Color
        # , aes(fill=color,group=mouseID)
        
        ## fill black
        , aes(group = mouseID)
        , color = "black"
        , fill = "black"
        
        , position = position_dodge(0.15) 
        , size = dotSize
        , stroke = 0
      # ) +
      # comboTrtFillShape(

      )
  }
    
  plot <- plot +
    theme_pubr() +
    labs(
      y = "LH (ng/mL)",
      x = "time (h) relative to lights out"
    ) +
    scale_x_continuous(
      breaks = xBreaks,
      labels = xLabels
    )+
    scale_color_identity()+
    textTheme(size = fontSize)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}, clip = clipVar) + 
    guides(linetype = "none")
  return(plot)
}

LHPlot_adultTrt_color <- function(
  df_long,
  trtVar = adultTrt,
  trtName = "adult trt",
  trtLineGuide = c("CON" = "dotted", "ALPS" = "solid"),
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  # df_long <- df_long %>%
  #   addOrderedColors(
  #     LH
  #     , mouseID
  #     , colorByGroups = FALSE
  #     , pkg = "rainbow"
  #     , byMax = FALSE
  #     , revOrder = TRUE
  #     , adultTrt
  #   )
  
  ggplot(
    df_long,
    aes(
      x = time,
      y = LH,
      group = {{ trtVar }}
    )
  ) +
    geom_line(
      alpha = 0.4,
      # aes(group = mouseID, color = color),
      aes(group = mouseID, color = mouseID),
      position = position_dodge(0.4)
      , size = 1
    ) +
    geom_point(
      shape = 21,
      alpha = 1, 
      aes(
        # fill= color
        # , color = color
        fill = mouseID
        , color = mouseID
        ,group=mouseID
        # ,shape={{ trtVar }}
        ), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = TRUE,
      lineTypeName = trtName,
      lineTypeGuide = trtLineGuide,
      typeVar = {{ trtVar }}
    )+
    addMeanSE_vertBar()+
    labs(
      y = "LH (ng/mL)",
      x = "time (h) relative to lights out"
    ) +
    scale_x_continuous(
      breaks = c(0, 5, 5.5, 6.5, 7.5, 8.5, 9.5),
      labels = c(-7.5, "", -2, -1, 0, 1, 2)
    )+
    theme_pubr() +
    textTheme(size = fontSize)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) + #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    guides(linetype = "none") # +
    # scale_color_identity() +
    # scale_fill_identity()
}

LHPlot_adultTrt <- function(
  df_long,
  trtVar = adultTrt,
  trtName = "adult trt",
  trtLineGuide = c("CON" = "dotted", "ALPS" = "solid"),
  trtFill = adultTrtFill(),
  trtColor = adultTrtColor(),
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
      group = {{ trtVar }}
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
      aes(fill={{ trtVar }}
          ,group=mouseID
          # ,shape={{ trtVar }}
          ), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = TRUE,
      lineTypeName = trtName,
      lineTypeGuide = trtLineGuide,
      typeVar = {{ trtVar }}
    )+
    addMeanSE_vertBar()+
    trtFill +
    trtColor +
    # adultTrtFill()+
    # adultTrtColor()+
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
    labs(
      y = "LH (ng/mL)",
      x = "time (hr) relative to lights out"
    ) +
    scale_x_continuous(
      breaks = c(0, 5, 5.5, 6.5, 7.5, 8.5, 9.5),
      labels = c(-7.5, "", -2, -1, 0, 1, 2)
    )+
    theme_pubr() +
    textTheme(size = fontSize)+
    boxTheme()+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) + #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    guides(linetype = "none")
}


propOvulatedPlot <- function(
  df,
  xVar = comboTrt,
  fontSize = 11
  , labelFontSize = 10
  , forManuscript = isManuscript
){
  viz <- ggplot(df, aes(x = {{ xVar }}, fill = ovulated, linetype = ovulated))+
    geom_bar(position = "fill", color = "black") +
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.3, colour = "darkgrey", position = "fill", size=labelFontSize)+
    labs(y = "% with oocytes") + 
    scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1)
      , labels = c("0", "25","50", "75", "100")
      # labels = scales::percent
    )+
    scale_fill_manual(values = c("white", "black")) +
    scale_linetype_manual(values = c("dotted", "solid"))+
    theme_pubr() +
    textTheme(size = fontSize, boldXText = TRUE)+
    boxTheme()+
    theme(
      legend.position = "none",
      axis.title.x = element_blank()
    )
  return(viz)
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
      #   colour = "black",
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
propSurgedPlotCombo_forSBN <- function(
  df,
  fontSize = 11
  , labelSize = 12
  , forManuscript = isManuscript
){
  if(!forManuscript){
    viz <- df %>%
      ggplot(aes(
        x = comboTrt
        , fill = interaction(comboTrt, surged)
        , color = interaction(comboTrt, surged)
        , linetype = interaction(comboTrt, surged)
      )
      ) +
      scale_color_manual(values = c(
        "STD-CON.FALSE"="grey20",
        "STD-ALPS.FALSE"="grey20",
        "LBN-CON.FALSE"="grey20",
        "LBN-ALPS.FALSE"="grey20",
        "STD-CON.TRUE"="black",
        "STD-ALPS.TRUE"="black",
        "LBN-CON.TRUE"="black",
        "LBN-ALPS.TRUE"="black"
      )) +
      scale_fill_manual(values = c(
        "STD-CON.FALSE"="white",
        "STD-ALPS.FALSE"="white",
        "LBN-CON.FALSE"="white",
        "LBN-ALPS.FALSE"="white",
        "STD-CON.TRUE"="#CCCCCC",
        "STD-ALPS.TRUE"="black",
        "LBN-CON.TRUE"="cyan3",
        "LBN-ALPS.TRUE"="#FF0099"
      )) +
      scale_linetype_manual(values = c(
        "STD-CON.FALSE"="dotted",
        "STD-ALPS.FALSE"="dotted",
        "LBN-CON.FALSE"="dotted",
        "LBN-ALPS.FALSE"="dotted",
        "STD-CON.TRUE"="solid",
        "STD-ALPS.TRUE"="solid",
        "LBN-CON.TRUE"="solid",
        "LBN-ALPS.TRUE"="solid"
      ))
  } else {
    viz <- df %>%
      ggplot(aes(
        x = comboTrt
        , fill = interaction(comboTrt, surged)
      )
      , color = NA
      )+
      scale_fill_manual(values = c(
        "STD-CON.FALSE"="white",
        "STD-ALPS.FALSE"="white",
        "LBN-CON.FALSE"="white",
        "LBN-ALPS.FALSE"="white",
        "STD-CON.TRUE"="#CCCCCC",
        "STD-ALPS.TRUE"="black",
        "LBN-CON.TRUE"="cyan3",
        "LBN-ALPS.TRUE"="#FF0099"
      ))
  }
  
  
  viz <-viz +
      geom_bar(position = "fill") +
      geom_text(
        aes(label = ..count..),
        stat = "count",
        vjust = 1.3,
        colour = "grey70",
        position = "fill",
        size = labelSize
      )+
      labs(y = "% with LH surge") + 
      theme_pubr() +
      textTheme(size = fontSize, boldXText = TRUE)+
      boxTheme()+
      rremove("legend") +
      rremove("xlab") +
      scale_y_continuous(
        breaks = c(0, 0.25, 0.5, 0.75, 1)
        , labels = c("0", "25","50", "75", "100")
      ) +
      scale_x_discrete(
        labels = c(
          "STD-CON" = "STD\nCON"
          , "STD-ALPS" = "STD\nALPS"
          , "LBN-CON" = "LBN\nCON"
          , "LBN-ALPS" = "LBN\nALPS"
        
      ))
  return(viz)
}

percSurgedPlot <- function(
  df
  , fontSize = 11
  , labelFontSize = 10
){
  viz <- df %>%
    ggplot(aes(x = comboTrt, fill = interaction(comboTrt, surged), color = interaction(comboTrt, surged)))+
    geom_bar(position = "fill", color = "black") +
    geom_text(aes(label = ..count..), stat = "count", vjust = 1.3, colour = "darkgrey", position = "fill", size=labelFontSize)+
    labs(y = "% with LH surge") + 
    scale_y_continuous(labels = scales::percent)+
    # scale_color_manual(values = c("white", "white", "white", "white", "black", "black", "darkcyan", "darkcyan"))+
    # scale_fill_manual(values = c("white", "white", "white", "white", "grey90", "black", "lightblue1", "darkcyan")) +
    scale_color_manual(values = c(
      "STD-CON.FALSE"="white",
      "STD-ALPS.FALSE"="white",
      "LBN-CON.FALSE"="white",
      "LBN-ALPS.FALSE"="white",
      "STD-CON.TRUE"="black",
      "STD-ALPS.TRUE"="black",
      "LBN-CON.TRUE"="black",
      "LBN-ALPS.TRUE"="black"
      )
    )+
    scale_fill_manual(values = c(
      "STD-CON.FALSE"="white",
      "STD-ALPS.FALSE"="white",
      "LBN-CON.FALSE"="white",
      "LBN-ALPS.FALSE"="white",
      "STD-CON.TRUE"="grey90",
      "STD-ALPS.TRUE"="black",
      "LBN-CON.TRUE"="lightblue1",
      "LBN-ALPS.TRUE"="darkcyan"
      )
    ) +
    theme_pubr() +
    textTheme(size = fontSize)+
    boxTheme()+
    rremove("legend")
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

plotLHAmp_dosage <- function(
  df, 
  textSize = 11, 
  dotSize = 2
  , textAngle = 0
  , forManuscript = isManuscript
  , addMedianIQR = FALSE
){
  if(forManuscript){
    plot <- df %>%
      ggplot(
        aes(
          x = dosage
          , y = maxLH
          , shape = dosage
          , color = dosage
        )
      )
  } else {
    plot <- df %>%
      ggplot(
        aes(
          x = dosage
          , y = maxLH
          , shape = dosage
          , fill = dosage
        )
      )
  }

  plot <- plot +
    jitterGeom_shapeAes(
      size = dotSize
      , alpha = 1
    ) +
    geom_hline(yintercept = surgeMin, color = "grey")
  
  if(addMedianIQR){
    plot <- plot +
      addMedianHorizontalBar(width = 0.9, size = 0.6, color = "black")+
      stat_summary(fun.min = function(z) { quantile(z,0.25) },
                   fun.max = function(z) { quantile(z,0.75) }
                   , geom = "linerange", color = "black")
  }
  
  plot <- plot +
    dosageFillShape(forManuscript = forManuscript)+
    boxTheme()+
    textTheme(textSize, boldXText = TRUE) +
    ylab("max evening LH (ng/mL)")+
    scale_x_discrete(
      labels = c(
        "0mg/kg"
        , "2mg/kg"
      )
    )+
    theme(
      legend.position = "none",
      axis.title.x = element_blank()
    )
  return(plot)
}

plotLHAmp_dosage_bySurgeStatus <- function(
  df, 
  surgeMin, 
  textSize = 11, 
  dotSize = 2
  , textAngle = 45
  , forManuscript = isManuscript
){
  df <- df %>%
    mutate(
      surgeStatus = 
        case_when(
          dosage == "0" & maxLH > surgeMin ~ "0 mg/kg surge",
          dosage == "0" & maxLH <= surgeMin ~ "0 mg/kg no surge",
          dosage == "2" & maxLH > surgeMin ~ "2 mg/kg surge",
          TRUE ~ "2 mg/kg no surge"
        )
    ) %>%
    mutate(
      surgeStatus = factor(surgeStatus, levels = c(
        "0 mg/kg surge"
        , "0 mg/kg no surge"
        , "2 mg/kg surge"
        , "2 mg/kg no surge"
      ))
    ) 
  
  
  if(forManuscript){
    plot <- df %>%
      ggplot(
        aes(
          x = surgeStatus
          , y = maxLH
          , shape = dosage
          , color = dosage
        )
      )
  } else {
    plot <- df %>%
      ggplot(
        aes(
          x = surgeStatus
          , y = maxLH
          , shape = dosage
          , fill = dosage
        )
      )
  }

  plot <- plot +
    jitterGeom_shapeAes(
      size = dotSize
      , alpha = 1
    ) +
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = FALSE
    ) +
    addMeanSE_vertBar()+
    dosageFillShape(forManuscript = forManuscript)+
    boxTheme()+
    textTheme(textSize) +
    ylab("LH (ng/mL)")+
    scale_x_discrete(
      labels = c(
        "0 mg/kg \nsurge"
        , "0 mg/kg\nno surge"
        , "2 mg/kg \nsurge"
        , "2 mg/kg\nno surge"
      )
    )+
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      if(textAngle > 0){axis.text.x = element_text(angle = textAngle, vjust = 1, hjust=1)}
    )
  return(plot)
}

plotLHTime_dosage <- function(
  df, 
  surgeMin, 
  textSize = 11, 
  dotSize = 2
){
  plot <- df %>%
    mutate(
      surgeStatus = 
        case_when(
          dosage == "0" & maxLH > surgeMin ~ "0 mg/kg surge",
          dosage == "0" & maxLH <= surgeMin ~ "0 mg/kg no surge",
          dosage == "2" & maxLH > surgeMin ~ "2 mg/kg surge",
          TRUE ~ "2 mg/kg no surge"
        )
    ) %>%
    mutate(
      surgeStatus = factor(surgeStatus, levels = c(
        "0 mg/kg surge"
        , "0 mg/kg no surge"
        , "2 mg/kg surge"
        , "2 mg/kg no surge"
      ))
    ) %>%
    ggplot(
      aes(
        x = surgeStatus,
        y = timeAtMax,
        fill = dosage,
        shape = dosage
      )
    ) +
    geom_point(
      alpha = 1,
      position = position_dodge2(0.4),
      size = dotSize,
      # shape = 21,
      color = "black"
    )+
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = FALSE
    ) +
    addMeanSE_vertBar()+
    dosageFillShape()+
    boxTheme()+
    textTheme(textSize) +
    ylab("time at maximum LH\n(hr) relative to lights out")+
    scale_x_discrete(
      labels = c(
        "0 mg/kg \nsurge"
        , "0 mg/kg\nno surge"
        , "2 mg/kg \nsurge"
        , "2 mg/kg\nno surge"
      )
    )+
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
    ) + 
    scale_y_continuous(
      breaks = c(5, 5.5, 6.5, 7.5, 8.5),
      labels = c(-2.5, -2, -1, 0, 1)
    )
  return(plot)
}


plotLHAmp_comboTrt <- function(
  df, 
  surgeMin, 
  textSize = 11, 
  dotSize = 2,
  angleX = TRUE,
  addSurgeMinLine = FALSE
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
        fill = comboTrt,
        shape = comboTrt
      )
    ) +
    # geom_point(
    geom_quasirandom(
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
        labels = c("control surge"="CON \nsurge", "control no surge"="CON  \nno surge", "stress surge"="ALPS \nsurge", "stress no surge"="ALPS  \nno surge")
        , drop=FALSE
      )+
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
      )
  } else {
    plot <- plot +
      scale_x_discrete(
        labels = c("control surge"="CON\nsurge", "control no surge"="CON\nno surge", "stress surge"="ALPS\nsurge", "stress no surge"="ALPS\nno surge")
        , drop=FALSE
      )+
      theme(
        legend.position = "none",
        axis.title.x = element_blank()
      )
  }
  
  if(addSurgeMinLine){
    plot <- plot + geom_hline(yintercept = surgeMin, color = "magenta", alpha = 0.3)
      
  }
  return(plot)
}

plotLHAmp_comboTrt_byEarlylife <- function(
  df, 
  surgeMin, 
  textSize = 11, 
  dotSize = 2,
  angleX = TRUE,
  addSurgeMinLine = FALSE
){
  plot <- df %>%
    mutate(
      surgeStatus = 
        case_when(
          earlyLifeTrt == "STD" & maxLH > surgeMin ~ "STD surge",
          earlyLifeTrt == "STD" & maxLH <= surgeMin ~ "STD no surge",
          earlyLifeTrt == "LBN" & maxLH > surgeMin ~ "LBN surge",
          TRUE ~ "LBN no surge"
        )
    ) %>%
    mutate(
      surgeStatus = factor(surgeStatus, levels = c("STD surge", "STD no surge", "LBN surge", "LBN no surge"))
    ) %>%
    ggplot(
      aes(
        x = surgeStatus,
        y = maxLH,
        fill = comboTrt,
        shape = comboTrt
      )
    ) +
    # geom_point(
    geom_quasirandom(
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
        labels = c("STD surge"="STD \nsurge", "STD no surge"="STD  \nno surge", "LBN surge"="LBN \nsurge", "LBN no surge"="LBN  \nno surge")
        , drop=FALSE
      )+
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
      )
  } else {
    plot <- plot +
      scale_x_discrete(
        # labels = c("STD surge"="STD\nsurge", "STD no surge" = "STD\nno\nsurge", "LBN surge"="LBN\nsurge", "LBN no surge"="LBN\nno\nsurge")
        # labels = c("STD surge"="STD\nsurge", "STD no surge" = "STD\nno surge", "LBN surge"="LBN\nsurge", "LBN no surge"="LBN\nno surge")
        labels = c("STD surge"="surge", "STD no surge" = "no surge", "LBN surge"="surge", "LBN no surge"="no surge")
        , drop=FALSE
      )+
      theme(
        legend.position = "none",
        axis.title.x = element_blank()
      )
  }
  
  if(addSurgeMinLine){
    plot <- plot + geom_hline(yintercept = surgeMin, color = "magenta", alpha = 0.3)
      
  }
  return(plot)
}

scatterPlotComboTrt_surgeAmp <- function(
    df,
    yVar,
    yLab,
    dotSize = 1.2,
    fontSize = 11,
    addMeanSE = TRUE,
    zoom_y = FALSE, # Zoom to part of y axis
    ymin = NULL,
    ymax = NULL
    , jitterWidth = 0.35
    , surgedAlpha = .8
    , notSurgedAlpha = 0.8
    , addSurgeMinLine = TRUE
    , surgeMin = 3
    , surgeLineColor = "magenta"
    , twoLineXLabs = FALSE
    , tiltedXLabs = FALSE
    , forManuscript = isManuscript
){
  STD_CON_fill  <- "white"
  STD_ALPS_fill  <- "black"
  LBN_CON_fill  <- "lightblue1"
  LBN_ALPS_fill  <- "darkcyan"
  
  trtFillScale <- list(
    scale_fill_manual(
      "treatment", 
      values = c(
        "STD-CON.surged" = alpha(STD_CON_fill, surgedAlpha)
        , "STD-CON.no surge" = alpha(STD_CON_fill, notSurgedAlpha)
        , "STD-ALPS.surged" = alpha(STD_ALPS_fill, surgedAlpha)
        , "STD-ALPS.no surge" = alpha(STD_ALPS_fill, notSurgedAlpha)
        , "LBN-CON.surged" = alpha(LBN_CON_fill, surgedAlpha)
        , "LBN-CON.no surge" = alpha(LBN_CON_fill, notSurgedAlpha)
        , "LBN-ALPS.surged" = alpha(LBN_ALPS_fill, surgedAlpha)
        , "LBN-ALPS.no surge" = alpha(LBN_ALPS_fill, notSurgedAlpha)
      )
    ),
    scale_shape_manual(
      "treatment", 
      values = c("STD-CON"=21, 
                 "STD-ALPS"=23,
                 "LBN-CON"=21, 
                 "LBN-ALPS"=23))
  )
  
  viz <- df %>%
    filter(
      !is.na({{ yVar }})
    ) %>%
    mutate(
      surged = 
        case_when(
          maxLH > surgeMin ~ "surged",
          maxLH <= surgeMin ~ "no surge"
        )
    ) %>%
    ggplot(
      aes(
        x = comboTrt,
        y = {{ yVar }},
        fill = interaction(comboTrt, surged),
        shape = comboTrt
        # , color = surged
      )
    ) +
    geom_quasirandom(
      size = dotSize
      , width = jitterWidth
    )+ 
    labs(y = yLab)+
    trtFillScale +
    theme_pubr()+
    expand_limits(y=0)+
    coord_cartesian(if(FALSE){xlim = c(NULL, NULL)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    textTheme(size = fontSize, boldXText = TRUE)+
    boxTheme()+
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )
  
  if(addMeanSE){
    viz <- viz +
      addMeanHorizontalBar() +
      addMeanSE_vertBar()
  }
  
  if(addSurgeMinLine){
    viz <- viz + geom_hline(yintercept = surgeMin, color = surgeLineColor, alpha = 0.6)
    
  }
  
  if(twoLineXLabs){
    viz <- viz + 
      scale_x_discrete(
        labels = c(
          "STD-CON" = "STD\nCON"
          , "STD-ALPS" = "STD\nALPS"
          , "LBN-CON" = "LBN\nCON"
          , "LBN-ALPS" = "LBN\nALPS"
          
        )
      )
    
  } else {
    if(tiltedXLabs){
      viz <- viz + 
        theme(
          axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)
        )
    }
  }
  
  
  
  return(viz)
}

plotLHTime_comboTrt <- function(
  df, 
  surgeMin, 
  textSize = 11, 
  dotSize = 2,
  angleX = TRUE
){
  plot <- df %>%
    filter(
      maxLH > surgeMin
    ) %>%
    mutate(
      surgeStatus = 
        case_when(
          adultTrt == "CON" & maxLH > surgeMin ~ "control surge",
          adultTrt == "ALPS" & maxLH > surgeMin ~ "stress surge",
        )
    ) %>%
    mutate(
      surgeStatus = factor(surgeStatus, levels = c("control surge", "stress surge"))
    ) %>%
    ggplot(
      aes(
        x = surgeStatus,
        y = timeAtMax,
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
    scale_y_continuous(
      breaks = c(5, 5.5, 6.5, 7.5, 8.5),
      labels = c(-2.5, -2, -1, 0, 1)
    )+
    ylab("time at maximum LH\n(hr) relative to lights out")+
    facet_wrap(
      ~earlyLifeTrt,
      scales = "free_y"
    )
  
  if(angleX){
    plot <- plot +
      scale_x_discrete(
        labels = c("control surge"="CON \nsurge", "stress surge"="ALPS \nsurge")
        , drop=FALSE
      )+
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
      )
  } else {
    plot <- plot +
      scale_x_discrete(
        labels = c("control surge"="CON\nsurge", "stress surge"="ALPS\nsurge")
        , drop=FALSE
      )+
      theme(
        legend.position = "none",
        axis.title.x = element_blank()
      )
  }
  
  
  return(plot)
}
