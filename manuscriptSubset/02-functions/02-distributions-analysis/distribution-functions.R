getAD_Pval <- function(result, version = 1, fromPaired = FALSE){
  if(fromPaired){
    result <- result$test_result
  }
  tbl <- result$ad
  tbl_tibble <- tbl %>% as.tibble()
  p_val <- tbl_tibble$` asympt. P-value`[[version]]
  return(p_val)
}

calcCumFreq <- function(
    df
    , varToSum
){
  df %>%
    arrange(
      {{ varToSum }}
    ) %>%
    mutate(
      cumFreq = ecdf({{ varToSum }})({{ varToSum }}) * 100
    )
}

plotCumulativeFreqDist <- function(
    df
    , xVar
    , xLab
    , yLab = "cumulative frequency (%)"
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 100
    , textSize = 11
    , legendPosition = c(0.6, 0.3)
    , scaleLog10 = FALSE
) {
  plot <- df %>%
    combineStress() %>%
    group_by(earlyLifeTrt, adultTrt) %>%
    calcCumFreq({{ xVar }}) %>%
    ggplot(
      aes(
        x = {{ xVar }}
        , y = cumFreq
        , group = comboTrt
        , color = comboTrt
      )
    ) +
    geom_line() +
    boxTheme() +
    textTheme(size = textSize)
  
  if(scaleLog10){
    plot <- plot + scale_x_log10()
  } else {
    plot <- plot + 
      expand_limits(x = 0) +
      coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})
  }
  
  plot <- plot +
    expand_limits(y = 0) +
    xlab(xLab) +
    ylab(yLab) +
    comboTrtLineColor() +
    theme(
      legend.position = legendPosition 
      , legend.key = element_blank()
    )
  return(plot)
}

plotDensityDistribution <- function(
    df
    , xVar
    , xLab
    , yLab = "density"
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , zoom_y = FALSE
    , ymin = NA
    , ymax = NA
    , textSize = 11
    , legendPosition = c(0.6, 0.3)
    , STD_CON_fill = "lightgrey"
    , STD_CON_color = "grey30"
    , fillAlpha = 0.8
    , scaleLog10 = FALSE
) {
  plot <- df %>%
    combineStress() %>%
    ggplot(
      aes(
        x = {{ xVar }}
        , color = comboTrt
        # , fill = comboTrt
      )
    ) +
    geom_density(alpha = fillAlpha)
  
  if(scaleLog10){
    plot <- plot + scale_x_log10()
  } else {
    plot <- plot +
      expand_limits(x = 0) +
      coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})
  }
  plot <- plot +
    xlab(xLab) +
    ylab(yLab) +
    comboTrtFillShape(STD_CON_fill = STD_CON_fill, STD_CON_color = STD_CON_color) +
    theme_pubr() +
    boxTheme() +
    textTheme(textSize)
  
  return(plot)
}

plotPSCProp_log <- function(
    df
    , yVar
    , yLab
    , logBreaks = waiver()
    , logLabels = waiver()
    , byCell = TRUE
    , dotSize = 0.75
    , dotAlpha = 0.6
    , jitterWidth = 0.35
    , byQuartiles = TRUE
    , sortByQuartile = TRUE
    , textSize = textSize
){
  if(byCell){
    if(sortByQuartile){
      df_counts <- df %>%
        group_by(cellID) %>%
        summarize(
          quartile50 = quantile({{ yVar }}, probs = 0.50, na.rm = TRUE)
          , .groups = "drop"
        ) %>%
        arrange(
          desc(quartile50)
        )
      
    } else {
      df_counts <- df %>%
        group_by(cellID) %>%
        summarize(
          count = n()
          , .groups = "drop"
        ) %>%
        arrange(
          desc(count)
        )
      
    }
    
    df <- df %>%
      mutate(
        cellID = factor(
          cellID
          , levels = df_counts$cellID
        )
      )
  }
  
  df <- df %>%
    combineStress()
  
  if(byQuartiles){
    df <- df %>%
      add_quartile_colors(
        if(byCell){cellID} else {comboTrt}
        , value_col = {{ yVar }}
      )
  }
  
  viz <- df %>%
    ggplot(
      aes(
        x = if(byCell){cellID} else{comboTrt}
        , y = {{ yVar }}
        , color = if(byQuartiles){Color} else {log({{ yVar }})}
        , fill = if(byQuartiles){Color} else {log({{ yVar }})}
      )
    ) +
    jitterGeom(
      size = dotSize,
      alpha = dotAlpha,
      width = jitterWidth,
      height = 0
    ) +
    theme_pubr()+
    theme(
      legend.position = "none"
    )+
    textTheme(size = textSize)+
    boxTheme() +
    theme(
      axis.text.x = element_blank()
    ) + 
    labs(y = yLab, x = ifelse(byCell, "cell", ""))
  
  if(byQuartiles){
    viz <- viz +
      scale_color_identity() +
      scale_fill_identity()
  } else {
    viz <- viz +
      scale_color_viridis() +
      scale_fill_viridis()
  }
  
  viz <- viz +
    facet_wrap(
      ~ comboTrt
      , nrow = 1
      , scales = "free_x"
      , strip.position = "bottom"
    ) +
    scale_y_log10(
      breaks = logBreaks
      , labels = logLabels
    )
  
  return(viz)
}

add_quartile_colors <- function(
    data
    , group_col
    , value_col
    , reverse_order = FALSE
) {
  data <- data %>%
    group_by({{ group_col }}) %>%
    mutate(
      QuartileID = cut({{ value_col }},
                       breaks = quantile({{ value_col }}, probs = 0:4/4, na.rm = TRUE),
                       include.lowest = TRUE,
                       labels = FALSE)
    ) %>% 
    ungroup()
  
  # quartile_colors <- viridis(4, option = "D")
  quartile_colors <- c("1" = "#2c7bb6", "2" = "#abd9e9", "3" = "#fdae61", "4" = "#d7191c")
  if (reverse_order) {
    color_assignment <- (4 - as.numeric(data$QuartileID)) +1
  } else {
    color_assignment <- as.numeric(data$QuartileID)
  }
  
  data$Color <- quartile_colors[color_assignment]
  
  data <- data %>%
    select(-QuartileID)
  
  # mutate(
  #   Color = viridis(4, option = "D")[as.numeric(QuartileID)]
  # ) %>%
  # select(-QuartileID)
  
  return(data)
}


# 4-way anderson darling --------------------------------------------------

fourWayAD <- function(
    df
    , feature
    , xLab
    , testVersion = 1
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 100
    , textSize = 11
    , legendPosition = c(0.6, 0.3)
    , STD_CON_fill = "lightgrey" # for density plot
    , STD_CON_color = "grey30"
    , colorCellByQuartiles = TRUE
){
  SC <- df %>%
    filter(
      earlyLifeTrt == "STD"
      , adultTrt == "CON"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  SA <- df %>%
    filter(
      earlyLifeTrt == "STD"
      , adultTrt == "ALPS"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  LC <- df %>%
    filter(
      earlyLifeTrt == "LBN"
      , adultTrt == "CON"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  LA <- df %>%
    filter(
      earlyLifeTrt == "LBN"
      , adultTrt == "ALPS"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  adTestResult <- ad.test(
    SC
    , SA
    , LC
    , LA
  )
  
  pVal <- getAD_Pval(adTestResult, version = testVersion)
  
  cumFreqPlot <- plotCumulativeFreqDist(
    df
    , {{ feature }}
    , xLab
    , zoom_x = zoom_x
    , xmin = xmin
    , xmax = xmax
    , zoom_y = zoom_y
    , ymin = ymin
    , ymax = ymax
    , textSize = textSize
    , legendPosition = legendPosition
  )
  
  fullDistCumFreqPlot <- plotCumulativeFreqDist(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , legendPosition = legendPosition
  )
  
  cumFreqPlot_log <- plotCumulativeFreqDist(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , legendPosition = c(0.1, 0.6)
    , scaleLog10 = TRUE
  )
  
  densityPlot <- plotDensityDistribution(
    df
    , {{ feature }}
    , xLab
    , zoom_x = zoom_x
    , xmin = xmin
    , xmax = xmax
    , textSize = textSize
    , legendPosition = legendPosition
    , STD_CON_fill = STD_CON_fill
    , STD_CON_color = STD_CON_color
  )
  
  fullDensityPlot <- plotDensityDistribution(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , legendPosition = legendPosition
    , STD_CON_fill = STD_CON_fill
    , STD_CON_color = STD_CON_color
  )
  
  densityPlot_log <- plotDensityDistribution(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , legendPosition = legendPosition
    , STD_CON_fill = STD_CON_fill
    , STD_CON_color = STD_CON_color
    , scaleLog10 = TRUE
  )
  
  plotByCell_log <- plotPSCProp_log(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , byQuartiles = colorCellByQuartiles
    , sortByQuartile = colorCellByQuartiles
  )
  
  plotQuartiles_log <- plotPSCProp_log(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , byQuartiles = colorCellByQuartiles
    , sortByQuartile = colorCellByQuartiles
    , byCell = FALSE
  )
  
  return(
    list(
      "adTest" = adTestResult
      , "pVal" = pVal
      , "plots" = list(
        "cumulativeFreqPlot" = cumFreqPlot
        , "fullCumulativeFreqPlot" = fullDistCumFreqPlot
        , "cumulativeFreqPlot_log" = cumFreqPlot_log
        , "densityPlot" = densityPlot
        , "fullDensityPlot" = fullDensityPlot
        , "densityPlot_log" = densityPlot_log
        , "plotByCell_log" = plotByCell_log
        , "plotsQuartiles_log" = plotQuartiles_log
      )
      , "vectors" = list(
        "SC" = SC
        , "SA" = SA
        , "LC" = LC
        , "LA" = LA
      )
    )
  )
}

runFourWayAD_allProps <- function(df, colorCellByQuartiles = TRUE, appendToPCol = NULL){
  amplitudeRes <- fourWayAD(df, amplitude, "amplitude (pA)", zoom_x = TRUE, xmin = 0, xmax = 125, colorCellByQuartiles = colorCellByQuartiles)
  riseTimeRes <- fourWayAD(df, riseTime, "rise time (ms)", zoom_x = TRUE, xmin = 0, xmax = 2.2, colorCellByQuartiles = colorCellByQuartiles)
  decayTimeRes <- fourWayAD(df, decay9010, "decay time (ms)", zoom_x = TRUE, xmin = 0, xmax = 80, colorCellByQuartiles = colorCellByQuartiles)
  fwhmRes <- fourWayAD(df, fwhm, "fwhm (ms)", zoom_x = TRUE, xmin = 0, xmax = 20, colorCellByQuartiles = colorCellByQuartiles)
  
  pValDF <- data.frame(
    "feature" = c("amplitude", "rise time", "decay time", "fwhm")
    , "p.value" = c(amplitudeRes$pVal, riseTimeRes$pVal, decayTimeRes$pVal, fwhmRes$pVal)
  )
  
  if(!is.null(appendToPCol)){
    newColName = paste0("p.value", appendToPCol)
    pValDF <- pValDF %>%
      rename(
        !!newColName := p.value
      )
  }
  return(
    list(
      "amplitude" = amplitudeRes
      , "riseTime" = riseTimeRes
      , "decayTime" = decayTimeRes
      , "fwhm" = fwhmRes
      , "pVals" = pValDF
    )
  )
}

## Pairwise KS/AD -----------------

dist_pairwise <- function(adRes, variable, isInt = FALSE){
  if(!isInt){
    vectorList <- adRes[[variable]]$vectors
  } else{
    vectorList <- adRes$vectors
  }
  ks_SCvSA <- ks.test(vectorList$SC, vectorList$SA)
  ks_SCvLC <- ks.test(vectorList$SC, vectorList$LC)
  # ks_SCvLA <- ks.test(vectorList$SC, vectorList$LA)
  # ks_LCvSA <- ks.test(vectorList$LC, vectorList$SA)
  ks_LCvLA <- ks.test(vectorList$LC, vectorList$LA)
  ks_SAvLA <- ks.test(vectorList$SA, vectorList$LA)
  
  KS_res <- adjust_pvalue(
    data.frame(
      "comp" = c(
        "STD-CON vs STD-ALPS"
        , "STD-CON vs LBN-CON"
        # , "STD-CON vs LBN-ALPS"
        # , "STD-ALPS vs LBN-CON"
        , "LBN-CON vs LBN-ALPS"
        , "STD-ALPS vs LBN-ALPS"
      )
      , "p" = c(
        ks_SCvSA$p.value
        , ks_SCvLC$p.value
        # , ks_SCvLA$p.value
        # , ks_LCvSA$p.value
        , ks_LCvLA$p.value
        , ks_SAvLA$p.value
      )
    )
    , p.col = "p"
    , method = "holm"
  ) %>%
    formatPCol() %>%
    rename(
      orgP = p
      , p = p.adj
    ) %>%
    formatPCol() %>%
    rename(
      p = orgP
      , `holm adj p` = p
    )
  
  ad_SCvSA <- ad.test(vectorList$SC, vectorList$SA)
  ad_SCvLC <- ad.test(vectorList$SC, vectorList$LC)
  # ad_SCvLA <- ad.test(vectorList$SC, vectorList$LA)
  # ad_LCvSA <- ad.test(vectorList$LC, vectorList$SA)
  ad_LCvLA <- ad.test(vectorList$LC, vectorList$LA)
  ad_SAvLA <- ad.test(vectorList$SA, vectorList$LA)
  
  AD_res <- adjust_pvalue(
    data.frame(
      "comp" = c(
        "STD-CON vs STD-ALPS"
        , "STD-CON vs LBN-CON"
        # , "STD-CON vs LBN-ALPS"
        # , "STD-ALPS vs LBN-CON"
        , "LBN-CON vs LBN-ALPS"
        , "STD-ALPS vs LBN-ALPS"
      )
      , "p" = c(
        getAD_Pval(ad_SCvSA)
        , getAD_Pval(ad_SCvLC)
        # , getAD_Pval(ad_SCvLA)
        # , getAD_Pval(ad_LCvSA)
        , getAD_Pval(ad_LCvLA)
        , getAD_Pval(ad_SAvLA)
      )
    )
    , p.col = "p"
    , method = "holm"
  ) %>%
    formatPCol() %>%
    rename(
      orgP = p
      , p = p.adj
    ) %>%
    formatPCol() %>%
    rename(
      p = orgP
      , `holm adj p` = p
    )
  
  joinedRes <- KS_res %>%
    left_join(
      AD_res
      , by = "comp"
      , suffix = c(".KS", ".AD")
    )
  
  return(list(
    "KS" = KS_res
    , "AD" = AD_res
    , "joinedRes" = joinedRes
  ))
}

# Bootstrap -----------------------

## main bootstrapping functions --------------------

getOneBootstrapMeans <- function(
    df
    , maxPerCell = 100
    , groupingVars = exprs(earlyLifeTrt, adultTrt)
    , replace = TRUE
    , bootstrapIt = NA
){
  if(!is.null(maxPerCell)){
    df <- df %>%
      group_by(cellID) %>%
      slice_sample(n = maxPerCell) %>%
      ungroup()
  }
  
  df <- slice_sample(
    df %>%
      group_by(!!! groupingVars)
    , prop = 1
    , replace = replace
  )
  
  sumDF <- df %>%
    summarize(
      amplitude = mean(amplitude, na.rm = TRUE)
      , riseTime = mean(riseTime, na.rm = TRUE)
      , decay9010 = mean(decay9010, na.rm = TRUE)
      , fwhm = mean(fwhm, na.rm = TRUE)
      , .groups = "drop"
    )
  if(!is.na(bootstrapIt)){
    sumDF <- sumDF %>%
      mutate(
        bootIt = bootstrapIt
      )
  }
  return(sumDF)
}
getOneBootstrapMeans_int <- function(
    df
    , maxPerCell = 100
    , groupingVars = exprs(earlyLifeTrt, adultTrt)
    , replace = TRUE
    , bootstrapIt = NA
){
  if(!is.null(maxPerCell)){
    df <- df %>%
      group_by(cellID) %>%
      slice_sample(n = maxPerCell) %>%
      ungroup()
  }
  
  df <- slice_sample(
    df %>%
      group_by(!!! groupingVars)
    , prop = 1
    , replace = replace
  )
  
  sumDF <- df %>%
    summarize(
      interval = mean(interval, na.rm = TRUE)
      , .groups = "drop"
    )
  if(!is.na(bootstrapIt)){
    sumDF <- sumDF %>%
      mutate(
        bootIt = bootstrapIt
      )
  }
  return(sumDF)
}

bootstrapGroup <- function(df
                           , nBootstrap = 2000
                           , maxPerCell = 100 # set to NULL if don't want to limit per cell
                           , groupingVars = exprs(earlyLifeTrt, adultTrt)
                           , replace = TRUE # only for sampling after picking events from each cell
                           , setSeed = NULL
                           , forInterval = FALSE
) {
  if (!is.null(setSeed)) {
    set.seed(setSeed)
  }
  
  if(!forInterval){
    bootstrapRes <- bind_rows(
      map(
        1:nBootstrap
        , ~ getOneBootstrapMeans(df, replace = replace, bootstrapIt = .x, maxPerCell = maxPerCell, groupingVars = groupingVars)
      )
    )
  } else{
    bootstrapRes <- bind_rows(
      map(
        1:nBootstrap
        , ~ getOneBootstrapMeans_int(df, replace = replace, bootstrapIt = .x, maxPerCell = maxPerCell, groupingVars = groupingVars)
      )
    )
  }
  
  return(bootstrapRes)
}

## Plots errors on x axis ---------------

plotBootstrapErrors_xAxis <- function(
    df
    , xLab
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , textSize = 11
    , dotSize = 0.5
    , meanBarWidth = 0.75
    , barSize = 0.5
) {
  df %>%
    ggplot(
      aes(
        y = comboTrt
        , x = y
        , color = comboTrt
      )
    ) +
    # geom_point(size = dotSize) +
    geom_errorbar( # not sure that meanbar is working with the flippped aes. Just delete the extra
      aes(
        y = comboTrt
        , xmin = y
        , xmax = y
      )
      , width = meanBarWidth
      , linewidth = barSize
    ) +
    geom_linerange(
      aes(
        y = comboTrt
        , xmin = lower
        , xmax = upper
      )
    ) +
    comboTrtLineColor() +
    expand_limits(x = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}) +
    labs(x = xLab) +
    scale_y_discrete(
      limits = rev
      , labels = c(
        "STD-CON" = "SC"
        , "LBN-CON" = "LC"
        , "STD-ALPS" = "SA"
        , "LBN-ALPS" = "LA"
        # "STD-CON" = "STD\nCON"
        # , "LBN-CON" = "LBN\nCON"
        # , "STD-ALPS" = "STD\nALPS"
        # , "LBN-ALPS" = "LBN\nALPS"
      )
      # , position = "right"
    )+
    theme_pubr()+
    boxTheme() +
    textTheme(size = textSize) +
    theme(
      axis.title.y = element_blank()
      , axis.text.y = element_text(face = "bold")
      , legend.position = "none"
    )
}

## format results ----------

formatBootstrapPvaluesCI <- function(bootDiffCI){
  bootDiffCI %>%
    group_by(
      variable
    ) %>%
    mutate(
      mult2.p.value = p.value * 2
    ) %>%
    adjust_pvalue(p.col = "mult2.p.value", method = "holm") %>%
    formatPCol(p.value) %>%
    formatPCol(mult2.p.value) %>%
    formatPCol(mult2.p.value.adj) %>%
    rename(
      p = mult2.p.value
      , p.adj = mult2.p.value.adj
    ) %>%
    mutate(
      CI = paste0("[", roundDecimals(lowerCIDiff, 2), ",", roundDecimals(upperCIDiff, 2), "]")
      , .after = meanDiff
    ) %>%
    select(
      - c(bothSameDir, p.value, lowerCIDiff, upperCIDiff, minDiff, maxDiff)
    )
}

renameBootstrapComparisonGroups <- function(df, removeComparison = TRUE, twoLineLabels = FALSE){
  df <- df %>%
    mutate(
      comp = case_when(
        comparison == "STD_ALPSvLBN_CON" ~ "STD-ALPS vs LBN-CON"
        , comparison == "STD_CONvLBN_ALPS" ~ "STD-CON vs LBN-ALPS"
        , comparison == "alpsSTDvLBN" ~ "STD-ALPS vs LBN-ALPS"
        , comparison == "conSTDvLBN" ~ "STD-CON vs LBN-CON"
        , comparison == "lbnCONvALPS" ~ "LBN-CON vs LBN-ALPS"
        , comparison == "stdCONvALPS" ~ "STD-CON vs STD-ALPS"
      )
      , comp = factor(
        comp
        , levels = c(
          "STD-CON vs STD-ALPS"
          , "LBN-CON vs LBN-ALPS"
          , "STD-CON vs LBN-CON"
          , "STD-ALPS vs LBN-ALPS"
          , "STD-CON vs LBN-ALPS"
          , "STD-ALPS vs LBN-CON"
        )
      )
      , .after = comparison
    ) %>%
    arrange(
      variable, comp
    )
  
  if(twoLineLabels){
    df <- df %>%
      makeCompTwoLines()
  }
  
  if(removeComparison) {
    df <- df %>%
      select(
        -comparison
      )
  }
  return(df)
}

makeCompTwoLines <- function(df){
  df %>%
    mutate(
      comp = gsub(" vs ", "\n", comp, fixed = TRUE)
      , comp = factor(
        comp
        , levels = c(
          "STD-CON\nSTD-ALPS"
          , "LBN-CON\nLBN-ALPS"
          , "STD-CON\nLBN-CON"
          , "STD-ALPS\nLBN-ALPS"
          , "STD-CON\nLBN-ALPS"
          , "STD-ALPS\nLBN-CON"
        )
      )
    )
}

## Plot pairwise -------------

plotBootstrapPairwiseDiffs <- function(bootRes, variableAsString, yLab, compProp = 0.15, specMeanDiffsDF = NULL){
  diffCItoPlot <- bootRes$diffsCI %>%
    makeCompTwoLines() %>%
    rename(
      y = meanDiff
      , lower = lowerCIDiff
      , upper = upperCIDiff
    )
  
  percOfSTDCON = bootRes$meansCI %>% 
    filter(variable == variableAsString
           , earlyLifeTrt == "STD"
           , adultTrt == "CON") %>%
    pull(mean) * compProp
  
  if(is.null(specMeanDiffsDF)){
    meanDiffs <- bootRes$extra$meanDiffs
  } else {
    meanDiffs <- specMeanDiffsDF
  }
  
  plot <- meanDiffs %>%
    renameBootstrapComparisonGroups(twoLineLabels = TRUE) %>%
    filter(
      variable == variableAsString
    ) %>%
    ggplot(
      aes(
        x = comp
        , y = difference
      )
    ) +
    jitterGeom() +
    theme_pubr()+
    boxTheme() +
    textTheme() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = percOfSTDCON, color = "blue") +
    geom_hline(yintercept = - percOfSTDCON, color = "blue") +
    labs(y = yLab, x = "pairwise comparisons between groups") +
    theme(
      axis.title.x = element_text(margin = margin(t = 0))
    )
  
  if(is.null(specMeanDiffsDF)){
    plot <- plot +
      plotError_LMM(
        diffCItoPlot %>%
          filter(
            variable == variableAsString
          )
        , xVar = comp
      )
  }
  return(plot)
}

## Run bootstrap -------------
analyzeBootstrapResults <- function(
    df
    , nBootstrap = 2000
    , maxPerCell = 100
    , groupingVars = exprs(earlyLifeTrt, adultTrt)
    , replace = TRUE
    , setSeed = NULL
    , dotSize = 1
    , textSize = 11
    , forInterval = FALSE
    , propDifferent = 0.15
    , CIprop = 0.05
    , legendPosition = c(0.7, 0.3)
){
  bootstrapRes <- df %>% 
    bootstrapGroup(nBootstrap = nBootstrap, maxPerCell = maxPerCell, groupingVars = groupingVars, replace = replace, setSeed = setSeed, forInterval = forInterval)
  
  if(!forInterval){
    bootstrapMeans <- bootstrapRes %>%
      pivot_longer(
        cols = c(amplitude, riseTime, decay9010, fwhm)
        , names_to = "variable"
        , values_to = "mean"
      ) %>%
      mutate(
        variable = factor(variable, levels = c("amplitude", "riseTime", "decay9010", "fwhm"))
      )
  } else {
    bootstrapMeans <- bootstrapRes %>%
      pivot_longer(
        cols = c(interval)
        , names_to = "variable"
        , values_to = "mean"
      )
  }
  
  bootstrapMeanSum <- bootstrapMeans %>%
    group_by(variable, earlyLifeTrt, adultTrt) %>%
    summarize(
      lowerCI = quantile(mean, probs = CIprop / 2, na.rm = TRUE)
      , upperCI = quantile(mean, probs = 1-(CIprop /2), na.rm = TRUE)
      , min = min(mean, na.rm = TRUE)
      , max = max(mean, na.rm = TRUE)
      , mean = mean(mean, na.rm = TRUE)
      , .groups = "drop"
    ) %>%
    relocate(
      mean, .before = lowerCI
    )
  
  bootstrapMeanDiffs <- bootstrapMeans %>%
    pivot_wider(
      id_cols = c(bootIt, variable)
      , names_from = c(earlyLifeTrt, adultTrt)
      , names_sep = "_"
      , values_from = mean
    ) %>%
    mutate(
      stdCONvALPS = STD_ALPS - STD_CON
      , lbnCONvALPS = LBN_ALPS - LBN_CON
      , conSTDvLBN = LBN_CON - STD_CON
      , alpsSTDvLBN = LBN_ALPS - STD_ALPS
      # , STD_CONvLBN_ALPS = LBN_ALPS - STD_CON
      # , STD_ALPSvLBN_CON = LBN_CON - STD_ALPS
    )
  
  bootstrapMeanDiffs_long <- bootstrapMeanDiffs %>%
    pivot_longer(
      cols = stdCONvALPS:alpsSTDvLBN
      # cols = stdCONvALPS:STD_ALPSvLBN_CON
      , names_to = "comparison"
      , values_to = "difference"
    )
  
  bootstrapMeanDiffsSum <- bootstrapMeanDiffs_long %>%
    group_by(
      variable
      , comparison
    ) %>%
    summarize(
      meanDiff = mean(difference, na.rm = TRUE)
      , lowerCIDiff = quantile(difference, probs = CIprop / 2, na.rm = TRUE)
      , upperCIDiff = quantile(difference, probs = 1-(CIprop / 2), na.rm = TRUE)
      , minDiff = min(difference, na.rm = TRUE)
      , maxDiff = max(difference, na.rm = TRUE)
      , bothSameDir = ifelse(
        (lowerCIDiff < 0 & upperCIDiff < 0) | (lowerCIDiff > 0 & upperCIDiff > 0)
        , TRUE
        , FALSE
      )
      , p.value = ifelse(
        meanDiff > 0
        , 1 - mean(difference > 0, na.rm = TRUE)
        , 1 - mean(difference < 0, na.rm = TRUE)
      )
      , .groups = "drop"
    ) %>%
    renameBootstrapComparisonGroups()
  
  bootstrapDiffPs <- bootstrapMeanDiffsSum %>%
    formatBootstrapPvaluesCI()
  
  if(!forInterval){
    origMean <- pscProps %>%
      group_by(
        earlyLifeTrt
        , adultTrt
      ) %>%
      summarize(
        amplitude = mean(amplitude, na.rm = TRUE)
        , riseTime = mean(riseTime, na.rm = TRUE)
        , decay9010 = mean(decay9010, na.rm = TRUE)
        , fwhm = mean(fwhm, na.rm = TRUE)
        , .groups = "drop"
      )
    
    getPerc_STD_CON_mean <- function(df, column, prop = propDifferent){
      stdCon <- df %>%
        filter(
          earlyLifeTrt == "STD"
          , adultTrt == "CON"
        )
      
      stdConMean <- stdCon[[column]]
      
      return(stdConMean * prop)
    }
    
    bootstrapCompDiffs <-  bootstrapMeanDiffs %>%
      rowwise() %>%
      mutate(
        diffOfInterest = getPerc_STD_CON_mean(origMean, variable)
        , .after = variable
      ) %>%
      mutate(
        stdCONvALPS_isBigger = (stdCONvALPS <= -abs(diffOfInterest) | stdCONvALPS >= abs(diffOfInterest))
        , lbnCONvALPS_isBigger = (lbnCONvALPS <= -abs(diffOfInterest) | lbnCONvALPS >= abs(diffOfInterest))
        , conSTDvLBN_isBigger = (conSTDvLBN <= -abs(diffOfInterest) | conSTDvLBN >= abs(diffOfInterest))
        , alpsSTDvLBN_isBigger = (alpsSTDvLBN <= -abs(diffOfInterest) | alpsSTDvLBN >= abs(diffOfInterest))
        # , STD_CONvLBN_ALPS_isBigger = (STD_CONvLBN_ALPS <= -abs(diffOfInterest) | STD_CONvLBN_ALPS >= abs(diffOfInterest))
        # , STD_ALPSvLBN_CON_isBigger = (STD_ALPSvLBN_CON <= -abs(diffOfInterest) | STD_ALPSvLBN_CON >= abs(diffOfInterest))
      ) %>%
      group_by(
        variable
      ) %>%
      summarize( # these are p-values, 1 - proportion where larger than original difference
        stdCONvALPS = 1 - mean(stdCONvALPS_isBigger)
        , lbnCONvALPS = 1 - mean(lbnCONvALPS_isBigger)
        , conSTDvLBN = 1 - mean(conSTDvLBN_isBigger)
        , alpsSTDvLBN = 1- mean(alpsSTDvLBN_isBigger)
        # , STD_CONvLBN_ALPS = 1 - mean(STD_CONvLBN_ALPS_isBigger)
        # , STD_ALPSvLBN_CON = 1- mean(STD_ALPSvLBN_CON_isBigger)
        , .groups = "drop"
      ) %>%
      pivot_longer(
        cols = stdCONvALPS:alpsSTDvLBN
        # cols = stdCONvALPS:STD_ALPSvLBN_CON
        , names_to = "comparison"
        , values_to = "p.value"
      ) %>%
      renameBootstrapComparisonGroups()
    
    # Errors 
    amplitudeErrors <- bootstrapMeanSum %>%
      filter(
        variable == "amplitude"
      ) %>%
      combineStress() %>%
      rename(
        y = mean
        , lower = lowerCI
        , upper = upperCI
      )
    
    riseTimeErrors <- bootstrapMeanSum %>%
      filter(
        variable == "riseTime"
      ) %>%
      combineStress() %>%
      rename(
        y = mean
        , lower = lowerCI
        , upper = upperCI
      )
    
    decay9010Errors <- bootstrapMeanSum %>%
      filter(
        variable == "decay9010"
      ) %>%
      combineStress() %>%
      rename(
        y = mean
        , lower = lowerCI
        , upper = upperCI
      )
    
    fwhmErrors <- bootstrapMeanSum %>%
      filter(
        variable == "fwhm"
      ) %>%
      combineStress() %>%
      rename(
        y = mean
        , lower = lowerCI
        , upper = upperCI
      )
    
    # Plots
    amplitude_plot <- bootstrapRes %>%
      combineStress() %>%
      scatterPlotComboTrt(
        yVar = amplitude
        , yLab = "amplitude (pA)"
        , dotSize = dotSize
        , fontSize = textSize
        , addMeanSE = FALSE
      ) +
      plotError_LMM_comboTrt(
        amplitudeErrors
      )
    
    
    riseTime_plot <- bootstrapRes %>%
      combineStress() %>%
      scatterPlotComboTrt(
        yVar = riseTime
        , yLab = "rise time (ms)"
        , dotSize = dotSize
        , fontSize = textSize
      ) +
      plotError_LMM_comboTrt(
        riseTimeErrors
      )
    
    decay9010_plot <- bootstrapRes %>%
      combineStress() %>%
      scatterPlotComboTrt(
        yVar = decay9010
        , yLab = "decay time (ms)"
        , dotSize = dotSize
        , fontSize = textSize
      ) +
      plotError_LMM_comboTrt(
        decay9010Errors
      )
    
    fwhm_plot <- bootstrapRes %>%
      combineStress() %>%
      scatterPlotComboTrt(
        yVar = fwhm
        , yLab = "full width half maximum (ms)"
        , dotSize = dotSize
        , fontSize = textSize
      ) +
      plotError_LMM_comboTrt(
        fwhmErrors
      )
    
    amplitude_ecdfPlot <- plotCumulativeFreqDist(
      df
      , amplitude
      , "amplitude (pA)"
      , zoom_x = TRUE
      , xmin = 0
      , xmax = 125
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    amplitude_ecdfPlot_full <- plotCumulativeFreqDist(
      df
      , amplitude
      , "amplitude (pA)"
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    riseTime_ecdfPlot <- plotCumulativeFreqDist(
      df
      , riseTime
      , "rise time (ms)"
      , zoom_x = TRUE
      , xmin = 0
      , xmax = 2.2
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    riseTime_ecdfPlot_full <- plotCumulativeFreqDist(
      df
      , riseTime
      , "rise time (ms)"
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    decay9010_ecdfPlot <- plotCumulativeFreqDist(
      df
      , decay9010
      , "decay time (ms)"
      , zoom_x = TRUE
      , xmin = 0
      , xmax = 80
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    decay9010_ecdfPlot_full <- plotCumulativeFreqDist(
      df
      , decay9010
      , "decay time (ms)"
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    fwhm_ecdfPlot <- plotCumulativeFreqDist(
      df
      , fwhm
      , "full width half maximum (ms)"
      , zoom_x = TRUE
      , xmin = 0
      , xmax = 20
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    fwhm_ecdfPlot_full <- plotCumulativeFreqDist(
      df
      , fwhm
      , "full width half maximum (ms)"
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    amplitude_errors_x <- amplitudeErrors %>%
      plotBootstrapErrors_xAxis(
        xLab = "amplitude (pA)"
        , zoom_x = TRUE
        , xmin = 0
        , xmax = 125
      )
    
    riseTime_errors_x <- riseTimeErrors %>%
      plotBootstrapErrors_xAxis(
        xLab = "rise time (ms)"
        , zoom_x = TRUE
        , xmin = 0
        , xmax = 2.2
      )
    
    decay9010_errors_x <- decay9010Errors %>%
      plotBootstrapErrors_xAxis(
        xLab = "decay time (ms)"
        , zoom_x = TRUE
        , xmin = 0
        , xmax = 80
      )
    
    fwhm_errors_x <- fwhmErrors %>%
      plotBootstrapErrors_xAxis(
        xLab = "full width half maximum (ms)"
        , zoom_x = TRUE
        , xmin = 0
        , xmax = 20
      )
    
    freqPlots_aligned <- align_plots(
      amplitude_ecdfPlot
      , riseTime_ecdfPlot
      , decay9010_ecdfPlot
      , fwhm_ecdfPlot
      , align = "h"
      , axis = "tb"
    )
    
    distPlots <- plot_grid(
      amplitude_errors_x
      , riseTime_errors_x
      , freqPlots_aligned[[1]]
      , freqPlots_aligned[[2]]
      , decay9010_errors_x
      , fwhm_errors_x
      , freqPlots_aligned[[3]]
      , freqPlots_aligned[[4]]
      , ncol = 2
      , rel_heights = c(rep(c(0.4, 1), 4))
      , align = "v"
      , axis = "lr"
      , labels = c("A", "B", "", "", "C", "D", "", "")
      , label_fontfamily = "Arial"
      , label_size = textSize
    )
    
    return(
      list(
        "results" = bootstrapRes
        , "meansCI" = bootstrapMeanSum
        , "diffsCI" = bootstrapMeanDiffsSum
        , "P_formatted" = bootstrapDiffPs
        , "compDiffs" = bootstrapCompDiffs
        , "distPlots" = distPlots
        , "plots" = list(
          "amplitude" = amplitude_plot
          , "riseTime" = riseTime_plot
          , "decayTime" = decay9010_plot
          , "fwhm" = fwhm_plot
        )
        , "cumulativeFreqPlots" = list(
          "amplitude" = amplitude_ecdfPlot
          , "riseTime" = riseTime_ecdfPlot
          , "decayTime" = decay9010_ecdfPlot
          , "fwhm" = fwhm_ecdfPlot
        )
        , "cumulativeFreqPlots_full" = list(
          "amplitude" = amplitude_ecdfPlot_full
          , "riseTime" = riseTime_ecdfPlot_full
          , "decayTime" = decay9010_ecdfPlot_full
          , "fwhm" = fwhm_ecdfPlot_full
        )
        , "errorPlots" = list(
          "amplitude" = amplitude_errors_x
          , "riseTime" = riseTime_errors_x
          , "decayTime" = decay9010_errors_x
          , "fwhm" = fwhm_errors_x
        )
        , "errors" = list(
          "amplitude" = amplitudeErrors
          , "riseTime" = riseTimeErrors
          , "decayTime" = decay9010Errors
          , "fwhm" = fwhmErrors
        )
        , "extra" = list(
          "longMeans" = bootstrapMeans
          , "meanDiffs" = bootstrapMeanDiffs_long
        )
      )
    )
  } else {
    intervalErrors <- bootstrapMeanSum %>%
      filter(
        variable == "interval"
      ) %>%
      combineStress() %>%
      rename(
        y = mean
        , lower = lowerCI
        , upper = upperCI
      )
    
    # Plots
    interval_plot <- bootstrapRes %>%
      combineStress() %>%
      scatterPlotComboTrt(
        yVar = interval
        , yLab = "interval (s)"
        , dotSize = dotSize
        , fontSize = textSize
        , addMeanSE = FALSE
      ) +
      plotError_LMM_comboTrt(
        intervalErrors
      )
    
    interval_ecdfPlot <- plotCumulativeFreqDist(
      df
      , interval
      , "interval (s)"
      , zoom_x = TRUE
      , xmin = 0
      , xmax = 20
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    interval_ecdfPlot_full <- plotCumulativeFreqDist(
      df
      , interval
      , "interval (s)"
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    interval_errors_x <- intervalErrors %>%
      plotBootstrapErrors_xAxis(
        xLab = "interval (s)"
        , zoom_x = TRUE
        , xmin = 0
        , xmax = 20
      )
    
    return(
      list(
        "results" = bootstrapRes
        , "meansCI" = bootstrapMeanSum
        , "diffsCI" = bootstrapMeanDiffsSum
        , "P_formatted" = bootstrapDiffPs
        , "plots" = list(
          "interval" = interval_plot
        )
        , "cumulativeFreqPlots" = list(
          "interval" = interval_ecdfPlot
        )
        , "cumulativeFreqPlots_full" = list(
          "interval" = interval_ecdfPlot_full
        )
        , "errorPlots" = list(
          "interval" = interval_errors_x
        )
        , "errors" = list(
          "interval" = intervalErrors
        )
        , "extra" = list(
          "longMeans" = bootstrapMeans
          , "meanDiffs" = bootstrapMeanDiffs_long
        )
      )
    )
  }
  
}
