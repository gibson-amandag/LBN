makeCyclesLong <- function(df, afterVar = earlyLifeTrt){
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("Day"),
      names_to = "day",
      names_prefix = "Day",
      values_to = "stage",
      values_drop_na = TRUE,
      names_transform = list("day" = as.integer),
      values_transform = list("stage" = as.integer)
    ) %>%
    relocate(
      day,
      stage,
      .after = {{ afterVar }}
    ) %>%
    mutate(
      stageName = case_when(
        stage == 1 ~ "estrus",
        stage == 2 ~ "diestrus",
        stage == 3 ~ "proestrus",
        TRUE ~ NA_character_
      )
    )
  return(df_long)
}

addPNDForCyles <- function(
  df
){
  df <- df %>%
    mutate(
      cycleDate = cycleStartDate + (day - 1),
      PND = cycleDate - DOB,
      .after = stage
    )
  return(df)
}

makeCyclesPercLong <- function(
  df,
  estrus_label = "estrus",
  diestrus_label = "diestrus",
  proestrus_label = "proestrus"
){
  df_long = df %>%
    pivot_longer( # New version of gather
      c(percE, percD, percP),
      names_to = "stage",
      names_prefix = "perc",
      values_to = "percent",
      values_drop_na = TRUE
    ) %>%
    select(
      -(starts_with("Day"))
    )
  
  df_long$stage <- factor(
    df_long$stage,
    levels = c("E", "D", "P"),
    labels = c(estrus_label, diestrus_label, proestrus_label)
  )
  
  return(df_long)
}

plotCycleTraces <- function(
  df,
  day = day,
  stage = stage,
  MouseID = mouseID,
  ncol = NULL,
  nrow = NULL,
  lineColorVar = earlyLifeTrt,
  # colorKey = c("STD" = "grey", "LBN" = "black"),
  colorLimits = c("STD", "LBN"),
  colorValues = c("grey", "black"),
  removeFacets = FALSE,
  removeLegend = TRUE,
  scales = "fixed",
  fontSize = 12
  , breakSeq = seq(7, 400, 7)
  , facetDir = "h"
){
  viz <- ggplot(df, aes(x = {{ day }}, y = {{ stage }}, color = {{ lineColorVar }})) +
    geom_line() +
    facet_wrap(
      vars( {{ MouseID }} ),
      ncol = ncol,
      nrow = nrow,
      scales = scales
      , dir = facetDir
    ) +
    scale_y_continuous(
      breaks = c(1, 2, 3), #axis ticks only at 1, 2, 3
      labels = c("E", "D", "P") #replace with E, D, and P
    ) +
    scale_x_continuous(
      breaks = breakSeq #labels every third integer
    # ) +
    # expand_limits(
    #   y = 0
    ) +
    coord_cartesian(ylim = c(0.5, 3.5))
  
  viz <- viz + 
    theme_pubr() +
    textTheme(size = fontSize) + 
    boxTheme()
  
  if(!is.null(enquo(lineColorVar))){
    viz <- viz + scale_color_manual(limits = colorLimits, values = colorValues)
      # values = colorKey)
  }
  
  if(removeLegend == TRUE) {
    viz <- viz + rremove("legend")
  } 
  
  if(removeFacets == TRUE) {
    viz <- viz + theme(
      strip.text = element_blank(), 
      strip.background = element_blank()
    )
  }
  
  return(viz)
}

plotCycleTraces_single <- function(
  df,
  day = day,
  stage = stage,
  MouseID = mouseID,
  ncol = NULL,
  nrow = NULL,
  lineColorVar = earlyLifeTrt,
  # colorKey = c("STD" = "grey", "LBN" = "black"),
  colorLimits = c("STD", "LBN"),
  colorValues = c("grey", "black"),
  removeFacets = FALSE,
  removeLegend = TRUE,
  fontSize = 12
){
  viz <- ggplot(df, aes(x = {{ day }}, y = {{ stage }}, color = {{ lineColorVar }})) +
    geom_line(aes(group = {{ MouseID }})) +
    # facet_wrap(
    #   vars( {{ MouseID }} ),
    #   ncol = ncol,
    #   nrow = nrow
    # ) +
    scale_y_continuous(
      breaks = c(1, 2, 3), #axis ticks only at 1, 2, 3
      labels = c("E", "D", "P") #replace with E, D, and P
    ) +
    scale_x_continuous(
      breaks = seq(1, 400, 3) #labels every third integer
    ) +
    coord_cartesian(ylim = c(0.5, 3.5))
  
  viz <- viz + 
    theme_pubr() +
    textTheme(size = fontSize) + 
    boxTheme()
  
  if(!is.null(enquo(lineColorVar))){
    viz <- viz + scale_color_manual(limits = colorLimits, values = colorValues)
  }
  
  if(removeLegend == TRUE) {
    viz <- viz + rremove("legend")
  } 
  
  # if(removeFacets == TRUE) {
  #   viz <- viz + theme(
  #     strip.text = element_blank(), 
  #     strip.background = element_blank()
  #   )
  # }
  
  return(viz)
}

plotCyclesPercent <- function(
  df,
  xVar = earlyLifeTrt,
  ylabel = "% days in stage",
  alpha = 0.7, # changed 2023-06-18
  dotSize = 1.2,
  barWidth = 0.7,
  barSize = 0.4,
  addMedian = FALSE,
  medianColor = "red",
  medianAlpha = 0.7,
  strip.position = "bottom",
  fontSize = 11,
  fillScale = earlyLifeFill()
){
  viz <- df %>%
    ggplot(
      aes(x = {{ xVar }}, y = percent, fill = {{ xVar }})
    ) +
    facet_wrap(
      ~ stage,
      strip.position = strip.position
    ) +
    labs(y = ylabel)+
    fillScale +
    # scale_fill_manual(values = c("white", "black"))+
    jitterGeom(size = dotSize, alpha = alpha)+
    addMeanHorizontalBar(width = 0.85, size = 0.4) +
    addMeanSE_vertBar(size = 0.4)+
    expand_limits(y = 0) +
    theme_pubr()+
    rremove(
      "legend"
    ) +
    textTheme(size = fontSize)+
    boxTheme()+
    rremove("xlab")
  
  
  if(addMedian){
    viz <- viz + addMedianHorizontalBar(
      width = barWidth, 
      size = barSize,
      color = medianColor,
      alpha = medianAlpha
    )
  }
  
  return(viz)
}
