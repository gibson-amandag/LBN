
# facets ------------------------------------------------------------------


litterNum_label <- c(
  "1" = "first litter",
  "2" = "second litter"
)

facetForLitterNum <- facet_grid(
  cols = vars(litterNum),
  labeller = labeller(
    litterNum = litterNum_label
  )
)

facetForLBN <- facet_wrap(
  ~ earlyLifeTrt,
  ncol = 2
)

facetForStage <- facet_wrap(
  ~ Sac_cycle,
  ncol = 2
)



# Saving ------------------------------------------------------------------



# Add a basename and figure number for file prefix
# This function creates another function that can be used for a particular
# presentation file
makeBaseNameFunc <- function(prefix){
  if(!prefix==""){
    prefix <- paste0(prefix, "_")
  }
  func <- function(figNum){
    baseName <- paste0(prefix, figNum, "_")
    return(baseName)
  }
  return(func)
}

makeSavePPTFunc <- function(presPPT, presFolder, presFileName, addDate){
  fullFileName <- paste0(presFileName)
  if(addDate){
    fullFileName <- paste0(fullFileName, "_", Sys.Date())
  }
  fullFileName <- paste0(fullFileName, ".pptx")
  
  saveFunc <- function(){
    print(presPPT, target = file.path(presFolder, fullFileName))
  }
  return(saveFunc)
}


exportImg_forPurposeFunc <- function(imgType, figNumFunc, plotFolder, compType){
  func <- function(plot, fileBaseName, figNum, units, width, height){
    flexSave(
      baseName = fileBaseName,
      thisFilePrefix = figNumFunc(figNum),
      plot = plot,
      fileType =imgType,
      filePath = plotFolder,
      compType = compType,
      units = units,
      width = width,
      height = height
    )
  }
  return(func)
}



# Dam behavior ------------------------------------------------------------

plotDamBehavior_daysFunc <- function(
    litterNums # as c()
    , fontSize = 16
    , dotSize = 3
    , lineSize = 0.75
    , facet = FALSE
    , facetInfo = facetForLBN
    , addTriangleForMean = FALSE
    , redMean = FALSE
    , colorByDam = FALSE
    , lineAlpha = 0.4
    , showDots = TRUE
    , removeLegend = FALSE
    , addDarkBox = FALSE # currently, only P5-6
    , addVertErrorBars = TRUE
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      behavior_overTime_days(
        yVar = Num_exits,
        yLab = "# of exits / hour",
        fontSize = fontSize,
        dotSize = dotSize,
        dotSize_byFirstDay = FALSE,
        lineSize = lineSize,
        addTriangleForMean = addTriangleForMean,
        redMean = redMean,
        colorByDam = colorByDam,
        lineAlpha = lineAlpha,
        showDots = showDots,
        addVertError = addVertErrorBars
      ) +
      theme(legend.position = "bottom") +
      labs(
        x = "PND\nZT hour"
      )
    
    if(facet){
      plot <- plot +
        facetInfo +
        guides(
          fill = "none"
          , linetype = "none"
        )
    }
    
    if(removeLegend){
      plot <- plot + theme(legend.position = "none")
    }
    
    if(addDarkBox){
      plot$layers <- c(
        geom_rect(
          xmin = as_datetime(ymd_hm("2000-01-05 13:55"))
          , xmax = as_datetime(ymd_hm("2000-01-05 23:55"))
          , ymin = -Inf
          , ymax = Inf
          , fill = "grey90"
          , alpha = 0.1
        )
        , plot$layers
      )
    }
    
    return(plot)
  }
  return(plotFunc)
}

# to-do : 2023-03-12, finish updating this. Add options for grouping
# model after behaviorDamModule.R 

# plotDamBehavior_func <- function(
#     litterNums # as c()
#     , fontSize = 16
#     , dotSize = 3
#     , lineSize = 0.75
#     , lineAlpha = 0.5
#     , dodgeVal = 0.2
#     , addTriangleForMean = TRUE
#     , redMean = TRUE
#     , colorByDam = TRUE
#     , showDots = FALSE
#     , addVertError = TRUE
#     , facetByTrt = TRUE
#     , facetByLitter = TRUE
#     , removeLegend = TRUE
#     , zoom_x = FALSE # Zoom to part of x axis
#     , xmin = 4
#     , xmax = 11
#     , zoom_y = FALSE # Zoom to part of y axis
#     , ymin = 0
#     , ymax = 90
# ) {
#   plotFunc <- function(df){
#     
#   }
# }
#   
#   plot <- df %>%
#   plotDamBehavior(
#     yVar = !! input$singleVar
#     , yLab = yText()
#     , fontSize = input$fontSize
#     , dotSize = input$dotSize
#     , lineSize = input$lineSize
#     , lineAlpha = input$indivAlpha
#     , dodgeVal = input$dodgeVal
#     , addTriangleForMean = input$triangleMean
#     , redMean = input$redMean
#     , colorByDam = input$colorByDam
#     , showDots = input$showDots
#     , addVertError = input$addVertError
#     , facetByTrt = input$facetByTrt
#     , facetByLitter = input$facetByLitter
#     , removeLegend = input$removeLegend
#     , STDColor = input$STDColor
#     , LBNColor = input$LBNColor
#     , STDFill = input$STDFill
#     , LBNFill = input$LBNFill
#     , zoom_x = zoom_x$zoom() # Zoom to part of x axis
#     , xmin = zoom_x$min()
#     , xmax = zoom_x$max()
#     , zoom_y = zoom_y$zoom() # Zoom to part of y axis
#     , ymin = zoom_y$min()
#     , ymax = zoom_y$max()
#   )


# Dam Mass ----------------------------------------------------------

plotDamMass_func <- function(
    litterNums # as c()
    , fontSize = 16
    , zoom_x = TRUE
    , zoom_x_min = 0
    , zoom_x_max = 21
    , zoom_y = TRUE
    , zoom_y_min = 0
    , zoom_y_max = 40
    , indivLineSize = 0.8
    , meanLineSize = 1.4
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      plot_dam_mass_lines(
        useLineType = FALSE, # TRUE/FALSE
        lineTypeVar = earlyLifeTrt,
        lineGroupVar = damID,
        xtitle = "postnatal day", #x axis label
        ytitle = "mass (g)", #y axis label
        title = NULL, # plot title
        individualLines = TRUE, # plot individual lines
        meanLines = TRUE, # plot mean lines with SE
        zoom_x = zoom_x, # Zoom to part of x axis
        xmin = zoom_x_min,
        xmax = zoom_x_max,
        zoom_y = zoom_y, # Zoom to part of y axis
        ymin = zoom_y_min,
        ymax = zoom_y_max,
        indivLineAlpha = .3,
        indivLineSize = indivLineSize,
        errorBarWidth = 0,
        meanLineSize = meanLineSize,
        meanAlpha = 1,
        errorBarSize = 1,
        # errorBarColor = "grey10",
        errorBarAlpha = 1,
        textSize = fontSize,
        axisSize = 0.5,
        # legendPosition = "bottom",
        legendPosition = c(0.85, 0.2),
        STDColor = "#4D4D4D",
        LBNColor = "#008B8B"
      ) +
      theme(
        legend.key = element_rect(fill = NA)
      )
      
    return(plot)
  }
  return(plotFunc)
}
# Offspring Mass ----------------------------------------------------------

plotOffspringMass <- function(
    litterNums # as c()
    , fontSize = 16
    , zoom_x = FALSE
    , zoom_x_min = 0
    , zoom_x_max = 21
    , zoom_y = TRUE
    , zoom_y_min = 0
    , zoom_y_max = 35
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      plot_mass_lines(
        groupByDam = TRUE,
        facetBySex = TRUE,
        useLineType = FALSE, # TRUE/FALSE
        lineTypeVar = earlyLifeTrt,
        lineGroupVar = damID,
        xtitle = "postnatal day", #x axis label
        ytitle = "mass (g)", #y axis label
        title = NULL, # plot title
        individualLines = TRUE, # plot individual lines
        meanLines = TRUE, # plot mean lines with SE
        zoom_x = zoom_x, # Zoom to part of x axis
        xmin = zoom_x_min,
        xmax = zoom_x_max,
        zoom_y = zoom_y, # Zoom to part of y axis
        ymin = zoom_y_min,
        ymax = zoom_y_max,
        indivLineAlpha = .3,
        indivLineSize = 0.8,
        errorBarWidth = 0,
        meanLineSize = 1.4,
        meanAlpha = 1,
        errorBarSize = 1,
        # errorBarColor = "grey10",
        errorBarAlpha = 1,
        textSize = fontSize,
        axisSize = 0.5,
        # legendPosition = "bottom",
        legendPosition = c(0.85, 0.2),
        STDColor = "#4D4D4D",
        LBNColor = "#008B8B"
      ) +
      theme(
        legend.key = element_rect(fill = NA)
      )
      
    return(plot)
  }
  return(plotFunc)
}


# Offspring maturation ----------------------------------------------------

getMaxMatVals <- function(df_f, df_m) {
  max_VO_age <- getMax(df_f, expr(VO_age))
  max_VO_mass <- getMax(df_f, expr(VO_mass))
  max_estrus_age <- getMax(df_f, expr(Estrus_age))
  max_estrus_mass <- getMax(df_f, expr(Estrus_mass))
  max_PPS_age <- getMax(df_m, expr(PreputialSep_age))
  max_PPS_mass <- getMax(df_m, expr(PreputialSep_mass))
  max_AGD_f <- getMax(df_f, expr(AGD_adult))
  max_AGD_m <- getMax(df_m, expr(AGD_adult))
  
  # Buffer for plotting
  max_age <- max(max_VO_age, max_estrus_age, max_PPS_age, na.rm = TRUE) + 3
  max_mass <- max(max_VO_mass, max_estrus_mass, max_PPS_mass, na.rm = TRUE) + 3
  max_AGD <- max(max_AGD_f, max_AGD_m, na.rm = TRUE) + 1

  return(
    list(
      max_age = max_age,
      max_mass = max_mass,
      max_VO_age = max_VO_age,
      max_VO_mass = max_VO_mass,
      max_estrus_age = max_estrus_age,
      max_estrus_mass = max_estrus_mass,
      max_PPS_age = max_PPS_age,
      max_PPS_mass = max_PPS_mass
      , max_AGD = max_AGD
      , max_AGD_f = max_AGD_f
      , max_AGD_m = max_AGD_m
    )
  )  
}

cumFreq_geom <- function(
    zoom_x = FALSE, # Zoom to part of x axis
    xmin = NULL,
    xmax = NULL,
    zoom_y = FALSE, # Zoom to part of y axis
    ymin = NULL,
    ymax = NULL,
    textSize = 11,
    lineSize = 2
) {
  list(
    stat_ecdf(
      size = lineSize
      , geom = "line"
      , pad = FALSE
    ),
    stat_ecdf(
      geom = "point"
      , pad = FALSE
    ),
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}),
    textTheme(size = textSize),
    boxTheme()
  )
}

cumFreq_plot <- function(
    df,
    phenoVar,
    phenotypeName,
    zoom_x = FALSE, # Zoom to part of x axis
    xmin = NULL,
    xmax = NULL,
    zoom_y = FALSE, # Zoom to part of y axis
    ymin = NULL,
    ymax = NULL,
    textSize = 11,
    lineSize = 1,
    STDColor = "grey30",
    LBNColor = "cyan4"
) {
  df %>%
    filter(!is.na({{ phenoVar }})) %>%
    ggplot(
      aes(
        {{ phenoVar }},
        color = earlyLifeTrt
      )
    ) +
    expand_limits(y = 0, x = 21) +
    cumFreq_geom(
      zoom_x,
      xmin,
      xmax,
      zoom_y,
      ymin,
      ymax,
      textSize,
      lineSize
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    earlyLifeColor(STDColor = STDColor, LBNColor = LBNColor) +
    labs(
      y = paste("cumulative % of", phenotypeName),
      x = "age (days)"
    ) + 
    theme(
      legend.key = element_rect(fill = NA),
      legend.position = c(0.8, 0.2)
    )
}

# Vaginal opening ---------------------------------------------------------

plotVaginalOpeningAgeFunc <- function(fontSize = 16, dotSize = 3, facetLitter = FALSE){
  plotFunc <- function(df){
    plot <- df %>%
      scatterPlotLBN(
        yVar = VO_age
        , "age at vaginal opening (days)"
        , textSize = fontSize
        , dotSize = dotSize
      )
    if(facetLitter){
      plot <- plot +
        facetForLitterNum
    }
    return(plot)
  }
  return(plotFunc)
}

plotVaginalOpeningMassFunc <- function(fontSize = 16, dotSize = 3, facetLitter = FALSE){
  plotFunc <- function(df){
    plot <- df %>%
      scatterPlotLBN(
        yVar = VO_mass
        , "mass at vaginal opening (g)"
        , textSize = fontSize
        , dotSize = dotSize
      )
    if(facetLitter){
      plot <- plot +
        facetForLitterNum
    }
    return(plot)
  }
  return(plotFunc)
}

plotVOAgeCumFreqFunc <- function(
    litterNums #c()
    , maxAge
    ,fontSize = 16
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      cumFreq_plot(
        VO_age,
        "vaginal opening",
        zoom_x = TRUE,
        xmin = 21,
        xmax = maxAge,
        textSize = fontSize
      )
    return(plot)
  }
  return(plotFunc)
}

# First estrus ------------------------------------------------------------

plotEstrusAgeFunc <- function(fontSize = 16, dotSize = 3, facetLitter = FALSE){
  plotFunc <- function(df){
    plot <- df %>%
      scatterPlotLBN(
        yVar = Estrus_age
        , "age at first estrus (days)"
        , textSize = fontSize
        , dotSize = dotSize
      )
    if(facetLitter){
      plot <- plot +
        facetForLitterNum
    }
    return(plot)
  }
  return(plotFunc)
}

plotEstrusMassFunc <- function(fontSize = 16, dotSize = 3, facetLitter = FALSE){
  plotFunc <- function(df){
    plot <- df %>%
      scatterPlotLBN(
        yVar = Estrus_mass
        , "mass at first estrus (g)"
        , textSize = fontSize
        , dotSize = dotSize
      )
    if(facetLitter){
      plot <- plot +
        facetForLitterNum
    }
    return(plot)
  }
  return(plotFunc)
}

plotEstrusAgeCumFreqFunc <- function(
    litterNums #c()
    , maxAge
    ,fontSize = 16
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      cumFreq_plot(
        Estrus_age,
        "first estrus",
        zoom_x = TRUE,
        xmin = 21,
        xmax = maxAge,
        textSize = fontSize
      )
    return(plot)
  }
  return(plotFunc)
}

# Preputial separation ----------------------------------------------------

plotPreputialSepAgeFunc <- function(fontSize = 16, dotSize = 3, facetLitter = FALSE){
  plotFunc <- function(df){
    plot <- df %>%
      scatterPlotLBN(
        yVar = PreputialSep_age
        , "age at preputial sep. (days)"
        , textSize = fontSize
        , dotSize = dotSize
      )
    if(facetLitter){
      plot <- plot +
        facetForLitterNum
    }
    return(plot)
  }
  return(plotFunc)
}

plotPreputialSepMassFunc <- function(fontSize = 16, dotSize = 3, facetLitter = FALSE){
  plotFunc <- function(df){
    plot <- df %>%
      scatterPlotLBN(
        yVar = PreputialSep_mass
        , "mass at preputial sep. (g)"
        , textSize = fontSize
        , dotSize = dotSize
      )
    if(facetLitter){
      plot <- plot +
        facetForLitterNum
    }
    return(plot)
  }
  return(plotFunc)
}

plotPreputialSepAgeCumFreqFunc <- function(
    litterNums #c()
    , maxAge
    ,fontSize = 16
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      cumFreq_plot(
        PreputialSep_age,
        "preputial separation",
        zoom_x = TRUE,
        xmin = 21,
        xmax = maxAge,
        textSize = fontSize
      )
    return(plot)
  }
  return(plotFunc)
}


# Cycles ------------------------------------------------------------------


## Representative ----------------------------------------------------------


plotRepCyclesFunc <- function(
    trts #c()
    , litterNums #c()
    , numMice = 6
    , ncol = 2
    , nrow = 3
    , fontSize = 16
    , breakSeq = seq(7, 400, 7)
){
  if(ncol*nrow!=numMice){
    print("WARNING: number of mice selected doesn't match layout")
  }
  # df is not long version of cycles df
  plotFunc <- function(df){
    df_filtered <- df %>%
      filter(
        earlyLifeTrt %in% trts
        , litterNum %in% litterNums
      )
    
    dfToUse <- df_filtered[sample(nrow(df_filtered))[1:numMice], ] %>%
      makeCyclesLong() %>%
      addCycleStartCol() %>%
      addPNDForCyles()
    
    plot <- plotCycleTraces(
      dfToUse
      , colorValues = c("grey30", "cyan4")
      , fontSize = fontSize
      , removeFacets = ifelse(length(trts)==1, TRUE, FALSE)
      , ncol = ncol
      , nrow = nrow
      , day = PND
      , breakSeq = breakSeq
    ) + theme(
      panel.border = element_rect(color = "lightgrey", fill = NA)
    )
    return(plot)
  }
  return(plotFunc)
}


## Percent days in stage ---------------------------------------------------

plotCyclesPercentFunc <- function(
    litterNums #c()
    , fontSize = 16
    , dotSize = 3
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      plotCyclesPercent(
        fontSize = fontSize
        , dotSize = dotSize
      )
    return(plot)
  }
  return(plotFunc)
}


# ALPS --------------------------------------------------------------------


## cort --------------------------------------------------------------------

manuscriptCortPlotFunc <- function(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 450
    , zoom_x = TRUE
    , xmin = -2
    , xmax = 7
    , fontSize = 11
    , dotSize = 2
    , yUnitsNewLine = TRUE
    , onlyLBN = FALSE
    , jitterPosition = 1.8
    , meanWidth = 1.4
    , wrapLegend = TRUE
    , useALPSLineType = FALSE
    , ALPSdodge = 1
    , stripPosition = "bottom"
    , plotMean = TRUE
    , plotSE = TRUE
    , pointAlpha = .9
    , lineAlpha = 0.4
){
  if(useALPSLineType){
    lineGuide <-  c("STD-CON"="dotted", "STD-ALPS"="solid", "LBN-CON"="dotted", "LBN-ALPS"="solid")
  } else{
    lineGuide <- c("solid", "solid", "solid", "solid")
  }
  
  
  plotFunc <- function(df){
    if(onlyLBN){
      df <- df %>%
        mutate(
          time = ifelse(
            adultTrt == "CON"
            , time - ALPSdodge
            , time + ALPSdodge
          )
        )
    }
    
    plot <- df %>%
      cortPlot(
        pointSize = dotSize
        , fontSize = fontSize
        , zoom_y = zoom_y
        , ymin = ymin
        , ymax = ymax
        , zoom_x = zoom_x
        , xmin = xmin
        , xmax = xmax
        , positionDodge = jitterPosition
        , meanWidth = meanWidth
        , yUnitsNewLine = yUnitsNewLine
        , lineTypeGuide = lineGuide
        , plotMean = plotMean
        , plotSE = plotSE
        , pointAlpha = pointAlpha
        , lineAlpha = lineAlpha
      )
    
    if(onlyLBN){
      plot <- plot + facet_wrap(
        ~ earlyLifeTrt
        , nrow = 1
        , strip.position = stripPosition
      )
      if(wrapLegend){
        plot <- plot +
          guides(
            linetype = guide_legend(nrow = 2)
            , color = guide_legend(nrow = 2)
            , shape = guide_legend(nrow = 2)
            , fill = guide_legend(nrow = 2)
          )
      }
    } else {
      plot <- plot + facet_wrap(
        # ~ earlyLifeTrt + adultTrt
        ~ comboTrt
        , nrow = 1
        , strip.position = stripPosition
      )
      plot <- plot +
        rremove("legend")
    }
    return(plot)
  }
  
  return(plotFunc)
}

plotCortFunc <- function(
    litterNums
    , zoom_y = TRUE
    , ymin = 0
    # , ymax = 1025 # changed 2022-12-07
    , ymax = 500 #2023-04-01
    , zoom_x = TRUE
    , xmin = -2
    , xmax = 7
    , fontSize = 16
    , dotSize = 3
    , breaks = c(0, 150, 300, 450, 600, 750)
    # , breaks = c(0, 200, 400, 600, 800, 1000)
    # , breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
    , yUnitsNewLine = FALSE
    , onlyLBN = FALSE
){
  plotFunc <- function(df){
    df <- df %>%
      filter(
        litterNum %in% litterNums
      )
    
    basePlot <- df %>%
      baseCortPlot(dotSize = dotSize)
    
    if(yUnitsNewLine){
      yLab <- "corticosterone\n(ng/mL)"
    } else {
      yLab <- "corticosterone (ng/mL)"
    }
    plot <- basePlot %>%
      longCortPlot(
        fontSize = fontSize
        , zoom_y = zoom_y
        , ymin = ymin
        , ymax = ymax
        , zoom_x = zoom_x
        , xmin = xmin
        , xmax = xmax
        , onlyLBN = onlyLBN
      ) +
      ylab(yLab) +
      scale_y_continuous(
        breaks = breaks
      )
    
    return(plot)
  }
  
  return(plotFunc)
}



## Body mass ---------------------------------------------------------------



## Uterine mass ------------------------------------------------------------

plotUterineMassByGroupFunc <- function(
    litterNums #(c)
    , proUterineMin = 125
    , diUterineMax = 100
    , fontSize = 16
    , dotSize = 3
    , facet = FALSE
    , facetInfo = NULL
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      plotUterineMassByGroup(
        hLineVal = proUterineMin
        , fontSize = fontSize
        , dotSize = dotSize
      ) + 
      geom_hline(
        yintercept = diUterineMax
        , color = "blue"
      ) + 
      theme(
        legend.position = "none"
      )
    
    if(facet){
      plot <- plot + facetInfo
    }
    
    return(plot)
  }
  return(plotFunc)
}


## LH ----------------------------------------------------------------------

plotLHFunc <- function(
  litterNums #c()
  , fontSize = 16
  , dotSize = 3
  , ymax = 40
  , addSurgeLine = FALSE
  , surgeMin = 3
  # , removeDotsBeyondYMax = FALSE
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      LHPlot_noMean(
        fontSize = fontSize
        , dotSize = dotSize
        , zoom_y = TRUE
        , ymin = 0
        , ymax = ymax
      ) + theme(
        legend.position = "none"
      ) + facet_wrap(
        ~comboTrt,
        scale = "free"
      )
    
    # Don't want to do this, because it removes the connecting lines
    # if(removeDotsBeyondYMax){
    #   plot <- plot +
    #     scale_y_continuous(limits = c(0, ymax))
    # }
    
    if(addSurgeLine){
      plot <- plot + 
        geom_hline(
          yintercept = surgeMin
          , color = "red"
          , alpha = 0.3
        )
    }
    
    return(plot)
  }
  return(plotFunc)
}


## Surge amplitude ---------------------------------------------------------

plotSurgeAmpFunc <- function(
    litterNums  
    , surgeMin = 3
    , fontSize = 16
    , dotSize = 3
    , angleX = FALSE
    , addSurgeMinLine = TRUE
    , ymax = 40
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      plotLHAmp_comboTrt(
        surgeMin = surgeMin
        , textSize = fontSize
        , dotSize = dotSize
        , angleX = angleX
        , addSurgeMinLine = addSurgeMinLine
      ) + 
      coord_cartesian(ylim = c(0, ymax))
    return(plot)
  }
  return(plotFunc)
}


## Percent Surged ----------------------------------------------------------

plotPercSurgedFunc <- function(
    litterNums
    , fontSize = 16
    , labelFontSize = 10
    , twoLineXLabs = TRUE
){
  plotFunc <- function(df){
    plot <- df %>%
      filter(
        litterNum %in% litterNums
      ) %>%
      percSurgedPlot(
        fontSize = fontSize
        , labelFontSize = labelFontSize
      ) +
      theme(
        axis.title.x = element_blank()
      )
    
    if(twoLineXLabs){
      plot <- plot + scale_x_discrete(
        labels = c(
          "STD-CON" = "STD\nCON"
          , "STD-ALPS" = "STD\nALPS"
          , "LBN-CON" = "LBN\nCON"
          , "LBN-ALPS" = "LBN\nALPS"
          
        )
      )
    }
    
    return(plot)
  }
  return(plotFunc)
}


# GABA PSCs ---------------------------------------------------------------

plotCatVarFunc <- function(
    singleVar # as expr()
    , fontSize = 16
    , dotSize = 3
    , twoLineXLabs = FALSE
    , tiltedXLabs = FALSE
    , useFacetLabels = TRUE
    , useSpecYLab = FALSE
    , thisYLab = ""
    , addLegend = FALSE
    , removeXTicks = FALSE
    , alpha = 0.7
    , addMeanSE = TRUE
    , jitterWidth = 0.35
){
  yVar <- as.character(singleVar)
  if(!useSpecYLab){
    yLabel <- getNiceName(yVar)
  } else {
    yLabel <- thisYLab
  }
  plotFunc <- function(
    df
    , zoom_y = FALSE
    , ymin = 0
    , ymax = 20
  ){
    plot <- df %>%
      filter(
        !is.na( {{singleVar}} )
        , !is.na(adultTrt)
      ) %>%
      scatterPlotComboTrt(
        yVar = !! singleVar
        , yLab = yLabel
        , dotSize = dotSize
        , fontSize = fontSize
        , zoom_y = zoom_y
        , ymin = ymin
        , ymax = ymax
        , alpha = alpha
        , addMeanSE = addMeanSE
        , jitterWidth = jitterWidth
      )
    
    if(twoLineXLabs){
      plot <- plot + 
        scale_x_discrete(
          labels = c(
            "STD-CON" = "STD\nCON"
            , "STD-ALPS" = "STD\nALPS"
            , "LBN-CON" = "LBN\nCON"
            , "LBN-ALPS" = "LBN\nALPS"

          )
        )
      if(tiltedXLabs){
        plot <- plot +
          theme( # fully vertical
            axis.text.x = element_text(
              angle = 90
              , vjust = 0.5
            )
          )
      }
    } else {
      if(tiltedXLabs){
        plot <- plot + 
          theme(
            axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)
          )
      }
    }
    
    if(useFacetLabels){
      plot <- plot + facet_wrap(
        ~earlyLifeTrt + adultTrt
        , ncol = 4
        , scales = "free_x"
        , strip.position = "bottom"
      ) +
      theme(
        axis.text.x = element_blank()
        , strip.text = element_text(face = "plain")
      )
    }
    
    if(addLegend){
      plot <- plot +
        theme(
          legend.position = "top"
        )
    }
    
    if(removeXTicks){
      plot <- plot +
        theme(
          axis.text.x = element_blank()
        )
    }
    return(plot)
  }
  return(plotFunc)
}


# Male cort admin ---------------------------------------------------------
plotMaleCortAdminFunc <- function(
    singleVar # as expr()
    , fontSize = 16
    , dotSize = 3
    , useSpecYLab = TRUE
    , thisYLab = ""
    , addLegend = FALSE
    , removeXTicks = FALSE
    , alpha = 0.7
    , addMeanSE = TRUE
){
  yVar <- as.character(singleVar)
  if(!useSpecYLab){
    yLabel <- getNiceName(yVar)
  } else {
    yLabel <- thisYLab
  }
  plotFunc <- function(
    df
    , zoom_y = FALSE
    , ymin = 0
    , ymax = 20
  ){
    plot <- df %>%
      filter(
        !is.na( {{singleVar}} )
        , !is.na(adultTrt)
        , !is.na(dosage)
      ) %>%
      scatterPlot_general(
        yVar = !! singleVar
        , yLab = yLabel
        , xVar = dosage
        , xLab = "dosage (mg/kg)"
        , dotSize = dotSize
        , textSize = fontSize
        , zoom_y = zoom_y
        , ymin = ymin
        , ymax = ymax
        , fillAlpha = alpha
        , addMean = addMeanSE
        , addSE = addMeanSE
        , fillVar = dosage
        , fillValues = c("white", "black")
        , hideXAxisLab = FALSE
      ) +
      facet_wrap(
        ~ earlyLifeTrt
      )
    
    if(addLegend){
      plot <- plot +
        theme(
          legend.position = "top"
        )
    }

    if(removeXTicks){
      plot <- plot +
        theme(
          axis.text.x = element_blank()
        )
    }
    return(plot)
  }
  return(plotFunc)
}

