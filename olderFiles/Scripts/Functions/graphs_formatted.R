twoWayDotPlot <- function(
  df,
  yVar, 
  yLab,
  xVar = Stress_treatment,
  fillVar = Treatment,
  shapeVar = Stress_treatment,
  facetVar = Treatment,
  strip.position = "bottom"
){
  viz <- df %>%
    ggplot(
      aes(x = {{ xVar }}, y = {{ yVar }}, fill = {{ fillVar }}, shape = {{ shapeVar }})
    ) +
    facet_wrap(
      eval(expr(~!!enquo(facetVar))),
      strip.position = strip.position
    ) +
    labs(y = yLab)+
    # scale_fill_manual(values = c("white", "black"))+
    addMeanHorizontalBar(width = 0.85, size = 0.4) +
    addMeanSE_vertBar(size = 0.4)+
    # addMedianHorizontalBar(width = 0.85, size = 0.4)+
    jitterGeom_shapeAes()+
    expand_limits(y = 0) +
    theme_pubr()+
    rremove(
      "legend"
    ) +
    textTheme()+
    boxTheme()+
    rremove("xlab")
  
  return(viz)
}