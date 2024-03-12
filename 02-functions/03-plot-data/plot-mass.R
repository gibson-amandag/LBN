makeDamMassLong <- function(df){
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("Dam_Mass_P"),
      names_to = "day",
      names_prefix = "Dam_Mass_P",
      values_to = "mass",
      values_drop_na = TRUE,
      names_transform = list("day" = as.integer)
    )
  return(df_long)
}

makeOffMassLong <- function(df){
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("Mass_P"),
      names_to = "day",
      names_prefix = "Mass_P",
      values_to = "mass",
      values_drop_na = TRUE,
      names_transform = list("day" = as.integer)
    )
  return(df_long)
}

plot_mass_lines <- function(
  df, #wide form mass df
  groupByDam = FALSE,
  facetBySex = FALSE,
  # groupInteraction = FALSE,
  # interactionGroupVar = litterNum,
  useLineType = TRUE, # TRUE/FALSE
  lineTypeVar = earlyLifeTrt,
  lineGroupVar = mouseID,
  xtitle = "PND", #x axis label
  ytitle = "mass (g)", #y axis label
  title = NULL, # plot title
  individualLines = TRUE, # plot individual lines
  meanLines = TRUE, # plot mean lines with SE
  zoom_x = FALSE, # Zoom to part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL,
  indivLineAlpha = 0.5,
  indivLineSize = 0.8,
  errorBarWidth = 1,
  meanLineSize = 1.4,
  meanAlpha = 1,
  errorBarSize = 1,
  # errorBarColor = "grey10",
  errorBarAlpha = 0.6,
  textSize = 11,
  axisSize = 0.5,
  legendPosition = "top",
  STDColor = "grey30",
  LBNColor = "cyan4"
  , clipVal = "on"
  , moveDownFacets = FALSE
){
  if(groupByDam == TRUE){
    df <- df %>%
      getAvgByDam(bySex = facetBySex)
  }
  
  df_long <- df %>%
    makeOffMassLong()
  
  plot <- df_long %>%
    ggplot(
      aes(
        x = day,
        y = mass,
        color = earlyLifeTrt,
        group = earlyLifeTrt
        # group = if(groupInteraction == TRUE){
        #   interaction({{ interactionGroupVar }}, earlyLifeTrt)
        # } else {earlyLifeTrt}
      )
    ) +
    plot_line_geom_layers (
      useLineType = useLineType, # TRUE/FALSE
      lineTypeVar = {{ lineTypeVar }},
      lineGroupVar = {{ lineGroupVar }},
      xtitle = xtitle, #x axis label
      ytitle = ytitle, #y axis label
      title = title, # plot title
      individualLines = individualLines, # plot individual lines
      meanLines = meanLines, # plot mean lines with SE
      zoom_x = zoom_x, # Zoom to part of x axis
      xmin = xmin,
      xmax = xmax,
      zoom_y = zoom_y, # Zoom to part of y axis
      ymin = ymin,
      ymax = ymax,
      indivLineAlpha = indivLineAlpha,
      indivLineSize = indivLineSize,
      errorBarWidth = errorBarWidth,
      meanLineSize = meanLineSize,
      meanAlpha = meanAlpha,
      errorBarSize = errorBarSize,
      # errorBarColor = errorBarColor,
      errorBarAlpha = errorBarAlpha,
      textSize = textSize,
      axisSize = axisSize
      , clipVal = clipVal
    )+
    earlyLifeColor(
      STDColor = STDColor,
      LBNColor = LBNColor
    ) +
    earlyLifeLineType() +
    theme(
      legend.position = legendPosition
    )
  
  if(facetBySex){
    plot <- plot +
      facet_wrap(vars(sex), labeller = labeller(sex = c("F"="female", "M"="male")), scales = "free_y")
  }
  
  if(moveDownFacets){
    plot <- plot + theme(
      strip.text.x.top = element_text(margin = margin(t = -10)) # this still seems to be clipped even when it shouldn't be
    )
  }
  return(plot)
}

plot_dam_mass_lines <- function(
  df, #wide form mass df
  useLineType = TRUE, # TRUE/FALSE
  lineTypeVar = earlyLifeTrt,
  lineGroupVar = damID,
  xtitle = "PND", #x axis label
  ytitle = "mass (g)", #y axis label
  title = NULL, # plot title
  individualLines = TRUE, # plot individual lines
  meanLines = TRUE, # plot mean lines with SE
  zoom_x = FALSE, # Zoom to part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL,
  indivLineAlpha = 0.5,
  indivLineSize = 0.8,
  errorBarWidth = 1,
  meanLineSize = 1.4,
  meanAlpha = 1,
  errorBarSize = 1,
  # errorBarColor = "grey10",
  errorBarAlpha = 0.6,
  textSize = 11,
  axisSize = 0.5,
  legendPosition = "top",
  STDColor = "grey30",
  LBNColor = "cyan4"
){
  df_long <- df %>%
    makeDamMassLong()
  
  plot <- df_long %>%
    ggplot(
      aes(
        x = day,
        y = mass,
        color = earlyLifeTrt,
        group = earlyLifeTrt
        # group = if(groupInteraction == TRUE){
        #   interaction({{ interactionGroupVar }}, earlyLifeTrt)
        # } else {earlyLifeTrt}
      )
    ) +
    plot_line_geom_layers (
      useLineType = useLineType, # TRUE/FALSE
      lineTypeVar = {{ lineTypeVar }},
      lineGroupVar = {{ lineGroupVar }},
      xtitle = xtitle, #x axis label
      ytitle = ytitle, #y axis label
      title = title, # plot title
      individualLines = individualLines, # plot individual lines
      meanLines = meanLines, # plot mean lines with SE
      zoom_x = zoom_x, # Zoom to part of x axis
      xmin = xmin,
      xmax = xmax,
      zoom_y = zoom_y, # Zoom to part of y axis
      ymin = ymin,
      ymax = ymax,
      indivLineAlpha = indivLineAlpha,
      indivLineSize = indivLineSize,
      errorBarWidth = errorBarWidth,
      meanLineSize = meanLineSize,
      meanAlpha = meanAlpha,
      errorBarSize = errorBarSize,
      # errorBarColor = errorBarColor,
      errorBarAlpha = errorBarAlpha,
      textSize = textSize,
      axisSize = axisSize
    )+
    earlyLifeColor(
      STDColor = STDColor,
      LBNColor = LBNColor
    ) +
    earlyLifeLineType() +
    theme(
      legend.position = legendPosition
    )
  
  return(plot)
}
