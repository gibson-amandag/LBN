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

plotPSCProp_log <- function(
    df
    , yVar
    , yLab
    , logBreaks
    , logLabels
    , byCell = TRUE
    , dotSize = 0.75
    , dotAlpha = 0.6
    , jitterWidth = 0.35
    , byQuartiles = TRUE
    , sortByQuartile = TRUE
){
  if(byCell){
    if(sortByQuartile){
      df_counts <- df %>%
        group_by(cellID) %>%
        summarize(
          quartile25 = quantile({{ yVar }}, probs = 0.25, na.rm = TRUE)
          , .groups = "drop"
        ) %>%
        arrange(
          desc(quartile25)
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

plotPSCProp_negLog <- function(
    df
    , yVar
    , yLab
    , logBreaks = c(-2.5, -5, -10, -20, -40, -80, -160, -320, -480)
    , logLabels = c("-2.5", "-5", "-10", "-20", "-40", "-80", "-160", "-320", "-480")
    , byCell = TRUE
    , dotSize = 0.75
    , dotAlpha = 0.6
    , jitterWidth = 0.35
    , byQuartiles = TRUE
    , reverseColor = FALSE
    , sortByQuartile = TRUE
){
  # Define the custom transformation
  neg_log_trans <- trans_new(
    name = "neg_log"
    , transform = function(y) -log10(-y)
    , inverse = function(y) -10^(-y)
    , domain = c(-Inf, -0.0001)
  )
  
  if(byCell){
    if(sortByQuartile){
      df_counts <- df %>%
        group_by(cellID) %>%
        summarize(
          quartile25 = quantile({{ yVar }}, probs = 0.25, na.rm = TRUE)
          , .groups = "drop"
        ) %>%
        arrange(
          desc(quartile25)
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
        , reverse_order = reverseColor
      )
  }
  
  viz <- df %>%
    ggplot(
      aes(
        x = if(byCell){cellID} else{comboTrt}
        , y = {{ yVar }}
        , color = if(byQuartiles){Color} else {- log(- {{ yVar }})}
        , fill = if(byQuartiles){Color} else {- log(- {{ yVar }})}
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
    scale_y_continuous(
      trans = neg_log_trans
      , breaks = logBreaks
      , labels = logLabels
    )
  
  return(viz)
}