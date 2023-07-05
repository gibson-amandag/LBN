getErrorDF_LMM <- function(
    mixedModel
    , xVarAsChar
    , errorType = "model"
    , ... # other things to afex_plot
) {
  lmmData <- afex_plot(
    mixedModel
    , xVarAsChar
    , error_ci = FALSE
    , return = "data"
    , error = errorType
    , ...
  )
  return(
    lmmData$means %>%
      as_data_frame()
  )
}

plotError_LMM <- function(
    lmmData
    , xVar
    , meanBarWidth = 0.8 # make smaller in the end, but for now to compare
    , barSize = 0.4
    , color = "red" # change in the end, but for now to compare
    , nudgeErrorLine = 0.1 # to offset from current error bar. Remove
) {
  geoms <- list(
    geom_errorbar(
      aes(
        x = {{xVar}}
        , ymin = y
        , ymax = y
      )
      , data = lmmData
      , inherit.aes = FALSE
      , width = meanBarWidth
      , size = barSize
      , color = color
    )
    , geom_linerange(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
      )
      , data = lmmData
      , inherit.aes = FALSE
      , size = barSize
      , color = color
      , position = position_nudge(x = nudgeErrorLine)
    )
  )
  return(geoms)
}

plotError_LMM_meanLine <- function(
    lmmData
    , xVar
    , barSize = 1.2
    , nudgeErrorLine = 0 # to offset from current error bar. Remove
    , ... # into aes
) {
  geoms <- list(
    geom_line(
      aes(
        x = PND
        , y = y
        , ...
      )
      , data = lmmData
      , size = barSize
    ),
    geom_linerange(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
        , ... 
      )
      , data = lmmData
      , inherit.aes = FALSE
      , size = barSize
      , position = position_nudge(x = nudgeErrorLine)
      , show.legend = FALSE
    )
  )
  return(geoms)
}