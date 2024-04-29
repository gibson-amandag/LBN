scatterPlot_general <- function(
  df,
  xVar,
  xLab,
  yVar,
  yLab,
  fillVar = NULL,
  fillLimits = NULL,
  fillValues = NULL,
  lineColorVar = NULL,
  lineColorLimits = NULL,
  lineColorValues = NULL,
  textSize = 12,
  zoom_x = FALSE, # Zoom to part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL,
  dotSize = 1.5,
  fillAlpha = 1,
  jitterWidth = 0.42,
  jitterHeight = 0,
  title = NULL,
  addMean = TRUE,
  addSE = TRUE,
  hideXAxisLab = TRUE
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xVar }},
        y = {{ yVar }},
        fill = {{ fillVar }},
        color = {{ lineColorVar }}
      )
    ) +
    jitterGeom(
      size = dotSize,
      alpha = fillAlpha, # doesn't do anything. Provide the alpha when giving the fillValues
      width = jitterWidth,
      height = jitterHeight
    ) +
    labs(title = title)+
    theme_pubr()+
    expand_limits(y=0)+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})+
    theme(
      legend.position = "none"
    )+
    textTheme(size = textSize)+
    boxTheme()

  if(addMean){
    viz <- viz + addMeanHorizontalBar()
  }
  if(addSE){
    viz <- viz + addMeanSE_vertBar()
  }

  if(!is.null(enquo(lineColorVar))){
    viz <- viz + scale_color_manual(limits = lineColorLimits, values = lineColorValues)
  }
  
  if(!is.null(enquo(fillVar))){
    viz <- viz + scale_fill_manual(limits = fillLimits, values = fillValues)
  }

  if(hideXAxisLab){
    viz <- viz + theme(
      axis.title.x = element_blank()
    )
  }
  
  if(!is.null(xLab)){
    viz <- viz + labs(x = xLab)
  }

  if(!is.null(yLab)){
    viz <- viz + labs(y = yLab)
  }
  
  return(viz)
}

scatterPlotLBN <- function(
  df,
  yVar,
  yLab,
  STDColor = if(forManuscript){"#CCCCCC"}else{"white"},
  LBNColor = "cyan3",
  textSize = 12,
  zoom_x = FALSE, # Zoom to part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL,
  dotSize = 1.5,
  fillAlpha = 1, 
  jitterWidth = 0.42,
  jitterHeight = 0,
  title = NULL,
  addMean = TRUE
  , addSEM = TRUE
  , meanColor = "black"
  , barColor = "black"
  , boldX = TRUE
  , forManuscript = isManuscript
){
  if(forManuscript){
    viz <- df %>%
      ggplot(
        aes(
          x = earlyLifeTrt,
          y = {{ yVar }}
          , color = earlyLifeTrt
        )
      ) +
      earlyLifeColor(STDColor = STDColor, LBNColor = LBNColor, colorAlpha = fillAlpha, forManuscript = forManuscript)
  } else {
    viz <- df %>%
      ggplot(
        aes(
          x = earlyLifeTrt,
          y = {{ yVar }},
          fill = earlyLifeTrt
        )
      ) +
      earlyLifeFill(STDColor = STDColor, LBNColor = LBNColor, fillAlpha = fillAlpha, forManuscript = forManuscript)
  }
  
  viz <- viz +
    jitterGeom(
      size = dotSize,
      alpha = fillAlpha,
      width = jitterWidth,
      height = jitterHeight
      , forManuscript = forManuscript
    )+
    labs(y = yLab, title = title)+
    # theme_pubr(margin = FALSE)+
    theme_pubr()+
    expand_limits(y=0)+
    scale_x_discrete(expand = expansion(add = 0.52))+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})+
    textTheme(size = textSize, boldXText = boldX)+
    boxTheme() +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )
  
  if(addMean){
    viz <- viz + 
      addMeanHorizontalBar(
        meanColor = meanColor
      )
  }
  
  if(addSEM){
    viz <- viz +
      addMeanSE_vertBar(
        barColor = barColor
      )
  }
  
  return(viz)
}

scatterPlotTwoVars_byLBN <- function(
  df,
  yVar,
  yLab,
  xVar,
  xLab,
  textSize = 11,
  dotSize = 1.5,
  jitterWidth = 0.1,
  jitterHeight = 0,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL,
  STDColor = "white",
  LBNColor = "cyan4",
  xIsDate = FALSE
  , forManuscript = isManuscript
  , fillAlpha = 1
){
  if(forManuscript){
    viz <- df %>%
      ggplot(
        aes(
          x = {{xVar }},
          y = {{ yVar }}
          , color = earlyLifeTrt
        )
      ) +
      earlyLifeColor(STDColor = STDColor, LBNColor = LBNColor, colorAlpha = fillAlpha, forManuscript = forManuscript)
  } else {
    viz <- df %>%
      ggplot(
        aes(
          x = {{xVar }},
          y = {{ yVar }},
          fill = earlyLifeTrt
        )
      ) +
      earlyLifeFill(STDColor = STDColor, LBNColor = LBNColor, fillAlpha = fillAlpha, forManuscript = forManuscript)
  }
  
  viz <- viz +
    jitterGeom(
      size = dotSize,
      width = jitterWidth,
      height = jitterHeight
      , forManuscript = forManuscript
    ) +
    labs(y = yLab, x = xLab)
  
  if(xIsDate){
    viz <- viz +
      expand_limits(y = 0)
  } else {
    viz <- viz +
      expand_limits(x = 0, y = 0)
  }
  
  viz <- viz +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    earlyLifeFill(STDColor = STDColor, LBNColor = LBNColor) +
    textTheme(textSize)+
    boxTheme()
  
  return(viz)
}

scatterPlotTwoVars_byComboTrt <- function(
  df,
  yVar,
  yLab,
  xVar,
  xLab,
  fontSize = 11,
  dotSize = 1.2,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
  , xIsDate = FALSE
  , forManuscript = isManuscript
  , dotAlpha = 1
){
  if(forManuscript){
    viz <- df %>%
      ggplot(
        aes(
          x = {{ xVar }},
          y = {{ yVar }}
          , color = comboTrt
          , shape = comboTrt
        )
      )
  } else {
    viz <- df %>%
      ggplot(
        aes(
          x = {{ xVar }},
          y = {{ yVar }}
          , fill = comboTrt
          , shape = comboTrt
        )
      )
  }
  
  viz <- viz +
    jitterGeom(size = dotSize, width = 0, forManuscript = forManuscript) +
    labs(y = yLab, x = xLab)
  
  if(xIsDate){
    viz <- viz + expand_limits(y = 0)
  } else {
    viz <- viz + expand_limits(x = 0, y = 0)
  }
  
  viz <- viz +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    comboTrtFillShape(forManuscript = forManuscript, colorAlpha = dotAlpha, fillAlpha = dotAlpha)+
    theme_pubr()+
    textTheme(size = fontSize)+
    boxTheme()
  
  return(viz)
}

scatterPlotComboTrt <- function(
  df,
  yVar,
  yLab,
  dotSize = 1.2,
  fontSize = 11,
  addMeanSE = TRUE,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL
  , jitterWidth = 0.42
  , alpha = 1
  , forManuscript = isManuscript
  , plotJitter = TRUE # almost always going to be true. Want option for bootstrapping
){
  df <- df %>%
    filter(
      !is.na({{ yVar }})
    )
  
  if(forManuscript){
    viz <- df %>%
      ggplot(
        aes(
          x = comboTrt
          , y = {{ yVar }}
          , shape = comboTrt
          , color = comboTrt
        )
      ) 
  } else {
    viz <- df %>%
      ggplot(
        aes(
          x = comboTrt
          , y = {{ yVar }}
          , fill = comboTrt
          , shape = comboTrt
        )
      )
  }
  
  if(plotJitter){
  viz <- viz +
    jitterGeom_shapeAes(
      size = dotSize
      , width = jitterWidth
      , alpha = alpha
    ) 
  }
  
  viz <- viz +
    labs(y = yLab)+
    comboTrtFillShape(fillAlpha = alpha, forManuscript = forManuscript) +
    theme_pubr()+
    expand_limits(y=0)+
    coord_cartesian(if(FALSE){xlim = c(NULL, NULL)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    scale_x_discrete(expand = expansion(add = 0.52)) +
    textTheme(size = fontSize, boldXText = TRUE)+
    boxTheme() +
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )
  
  if(addMeanSE){
    viz <- viz +
      addMeanHorizontalBar(width = .95
                           , size = 0.6
                           , meanColor = "black") +
      addMeanSE_vertBar(size = 0.6
                        , barColor = "black")
  }
  
  return(viz)
}

getNiceName <- function(
  varName # as expression or as string, both work
  , labelDF = niceNames #data frame with labels
){
  label <- as.character(varName) # default to name of variable
  if(label %in% colnames(labelDF))
  {
    df <- labelDF %>%
      select(
        {{ varName }}
      )
    label <- df[[1]]
  }
  return(label)
}

addRandomColors <- function(df, colorVar, subjectVar, colorByGroups = FALSE, pkg = "rainbow", seedVal = 123, ...){
  if(colorByGroups){
    grouped_df <- df %>%
      group_by(
        {{ subjectVar }}
        , ...
      )
  } else{
    grouped_df <- df %>%
      group_by(
        {{ subjectVar }}
      )
  }
  
  grouped_df <- grouped_df %>%
    summarize(
      n = n()
      , .groups = "drop"
    )
  
  if(colorByGroups){
    grouped_df <- grouped_df %>%
      group_by(
        ...
      )
  }
  
  set.seed(seedVal)
  
  ranked_df <- grouped_df %>%
    mutate(
      randNum = runif(n())
      , rank = rank(randNum, ties.method = "first")
    ) %>%
    select(
      -c(n, randNum)
    )
  
  if(pkg == "rainbow"){
    ranked_df <- ranked_df %>%  
      mutate(
        color = rainbow(max(rank))[rank]
      ) %>%
      ungroup()
  }else{
    if(pkg == "viridis"){
      if(!require(viridis))install.packages('viridis')
      ranked_df <- ranked_df %>%  
        mutate(
          # these are different color options
          color = viridis::viridis(max(rank))[rank]
        ) %>%
        ungroup()
    } else { # default rainbow
      ranked_df <- ranked_df %>%  
        mutate(
          color = rainbow(max(rank))[rank]
        ) %>%
        ungroup()
    }
  }
  
  subjectVarStr <- deparse(substitute(subjectVar))
  ellipsisVars <- substitute(alist(...))
  ellipsisStrs <- sapply(ellipsisVars[-1], as.character)
  # print(subjectVarStr)
  # print(ellipsisStrs)
  # add the calculated colors back to the original data
  df <- df %>%
    left_join(
      ranked_df
      , by = c( subjectVarStr, ellipsisStrs)
    )
  
  return(df)
}

addOrderedColors <- function(df, orderVar, subjectVar, colorByGroups = FALSE, pkg = "rainbow", byMax = FALSE, revOrder = TRUE, ...) {
  # calculate the mean for each subject within each group
  mean_df <- df %>%
    group_by(
      {{ subjectVar }}
      , ...
    ) 
  
  if(byMax){
    mean_df <- mean_df %>%
      summarize(
        mean_orderVar = max(
          {{ orderVar }}
          , na.rm = TRUE
        )
        , .groups = "drop"
      )
  } else{
    mean_df <- mean_df %>%
      summarize(mean_orderVar = mean(
        {{ orderVar }}
        , na.rm = TRUE
      )
      , .groups = "drop"
      )
      
  }
  
  if(colorByGroups){
    mean_df <- mean_df %>%
      group_by(
        ...
      )
  }
  
  if(revOrder){
    ranked_df <- mean_df %>%
      mutate(
        rank = rank(
          -mean_orderVar
          , ties.method = "first"
        )
      )
  } else {
    ranked_df <- mean_df %>%
      mutate(
        rank = rank(
          mean_orderVar
          , ties.method = "first"
        )
      )
  }
  
  if(pkg == "rainbow"){
    ranked_df <- ranked_df %>%  
      mutate(
        color = rainbow(max(rank))[rank]
      ) %>%
      ungroup()
  }else{
    if(pkg == "viridis"){
      if(!require(viridis))install.packages('viridis')
      ranked_df <- ranked_df %>%  
        mutate(
          # these are different color options
          color = viridis::viridis(max(rank))[rank]
        ) %>%
        ungroup()
    } else { # default rainbow
      ranked_df <- ranked_df %>%  
        mutate(
          color = rainbow(max(rank))[rank]
        ) %>%
        ungroup()
    }
  }
  
  subjectVarStr <- deparse(substitute(subjectVar))
  ellipsisVars <- substitute(alist(...))
  ellipsisStrs <- sapply(ellipsisVars[-1], as.character)
  # add the calculated colors back to the original data
  df <- df %>%
    left_join(
      ranked_df
      , by = c( subjectVarStr, ellipsisStrs)
    )
  
  return(df)
}

getRandomSubjects <- function(df, subjectVar, n, seed=123) {
  
  # Sets the seed for the random number generator
  set.seed(seed)
  
  # Step 1: Randomly pick n subjects
  df_subjects <- df %>%
    distinct({{ subjectVar }}) %>% 
    sample_n(n)
  
  IDs <- df_subjects %>%
    pull( {{ subjectVar }} )
  
  print(IDs)
  
  # Step 2: Filter the original dataframe 
  # to include only rows that match the selected subjects
  df_filtered <- df %>% 
    filter({{ subjectVar }} %in% IDs) %>%
    ungroup()
  
  return(df_filtered)
  
  # This is a more streamlined approach to consider:
  # slice_sample() which can handle groups that are smaller than n
}
