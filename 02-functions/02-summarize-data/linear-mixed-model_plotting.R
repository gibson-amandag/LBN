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
    , nudgeMeanLine = 0
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
      # , size = barSize
      , linewidth = barSize
      , color = color
      , position = position_nudge(x = nudgeMeanLine)
    )
    , geom_linerange(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
      )
      , data = lmmData
      , inherit.aes = FALSE
      # , size = barSize
      , linewidth = barSize
      , color = color
      , position = position_nudge(x = nudgeErrorLine)
    )
  )
  return(geoms)
}

plotError_LMM_aes <- function(
    lmmData
    , xVar
    , meanBarWidth = 0.8 # make smaller in the end, but for now to compare
    , barSize = 0.4
    , nudgeErrorLine = 0.1 # to offset from current error bar. Remove
    , nudgeMeanLine = 0
    , ...
) {
  geoms <- list(
    geom_errorbar(
      aes(
        x = {{xVar}}
        , ymin = y
        , ymax = y
        , ...
      )
      , data = lmmData
      , inherit.aes = FALSE
      , width = meanBarWidth
      # , size = barSize
      , lineWidth = barSize
      , position = position_nudge(x = nudgeMeanLine)
    )
    , geom_linerange(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
        , ...
      )
      , data = lmmData
      , inherit.aes = FALSE
      # , size = barSize
      , linewidth = barSize
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
      # , size = barSize
      , linewidth = barSize
      , inherit.aes = FALSE # 2023-07-30
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
      # , size = barSize
      , linewidth = barSize
      , position = position_nudge(x = nudgeErrorLine)
      , show.legend = FALSE
    )
  )
  return(geoms)
}

plotError_LMM_meanLine_mass <- function(
    lmmData
    , xVar
    , barSize = 1.2
    , nudgeErrorLine = 0 # to offset from current error bar. Remove
    , ribbonAlpha = 0.2
    , ... # into aes
) {
  geoms <- list(
    geom_line(
      aes(
        x = {{ xVar }}
        , y = y
        , color = earlyLifeTrt
        , ...
      )
      , data = lmmData
      # , size = barSize
      , linewidth = barSize
      , inherit.aes = FALSE # 2023-07-30
    ),
    geom_ribbon(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
        , fill = earlyLifeTrt
        , ... 
      )
      , data = lmmData
      , inherit.aes = FALSE
      # , size = barSize
      , linewidth = barSize
      , position = position_nudge(x = nudgeErrorLine)
      , show.legend = FALSE
      , alpha = ribbonAlpha
    )
  )
  return(geoms)
}
