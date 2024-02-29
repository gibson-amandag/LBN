
# Functions ---------------------------------------------------------------


## ALPS --------------------------------------------------------------------


lmmForComboTrt <- function(df, depVar) {
  # depVar as expr(varName)
  depVarSym <- rlang::sym(depVar)
  
  mixed(
    formula = rlang::expr(!!depVarSym ~ earlyLifeTrt * adultTrt + (1|damID))
    , data = df
    , method = "KR"
  )
}

getErrorDF_LMM_comboTrt <- function(lmm){
  lmm %>%
    getErrorDF_LMM(
      xVar = "earlyLifeTrt"
      , panel = "adultTrt"
    ) %>%
    combineStress()
}

plotError_LMM_comboTrt <- function(lmm_error){
  plotError_LMM(
    lmm_error
    , xVar = comboTrt
  )
}

comboTrt_scatterAndLMM <- function(
    singleVar # as expr()
    , thisYLab = ""
    , thisFontSize = textSize
    , thisDotSize = dotSize
    , twoLineXLabs = TRUE
    , useFacetLabels = FALSE
    , useSpecYLab = TRUE
    , addLegend = FALSE
    , removeXTicks = FALSE
    , alpha = 0.7
    , addMeanSE = FALSE
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
        , dotSize = thisDotSize
        , fontSize = thisFontSize
        , zoom_y = zoom_y
        , ymin = ymin
        , ymax = ymax
        , alpha = alpha
        , addMeanSE = addMeanSE
      )
    
    if(twoLineXLabs){
      # plot <- plot + 
      #   theme(
      #     axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)
      #   )
      plot <- plot + scale_x_discrete(
        labels = c(
          "STD-CON" = "STD\nCON"
          , "STD-ALPS" = "STD\nALPS"
          , "LBN-CON" = "LBN\nCON"
          , "LBN-ALPS" = "LBN\nALPS"
          
        )
      )
      
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
          legend.position = c(0.2, 0.4)
        )
    }
    
    if(removeXTicks){
      plot <- plot +
        theme(
          axis.text.x = element_blank()
        )
    }
    
    lmm <- df %>% lmmForComboTrt(singleVar)

    lmm_error <- getErrorDF_LMM_comboTrt(lmm)

    plot <- plot +
      plotError_LMM_comboTrt(lmm_error)

    return(
      output = list(
        plot = plot
        , lmm = lmm
        , lmm_error = lmm_error
    ))
  }
  return(plotFunc)
}

### mass funcs -------------

statsAndPlot_BodyMassAM_ALPS <- comboTrt_scatterAndLMM(
  expr(Body_mass_AM)
  , thisYLab = "body mass (g)"
  , addLegend = TRUE
)
statsAndPlot_ChangeBodyMass_ALPS <- comboTrt_scatterAndLMM(
  expr(bodyMass_diff)
  , thisYLab = "\u0394 body mass (g)"
)

statsAndPlot_PercChangeBodyMass_ALPS <- comboTrt_scatterAndLMM(
  expr(percChangeBodyMass)
  , thisYLab = "% \u0394 body mass"
)

statsAndPlot_AdrenalMass_ALPS <- comboTrt_scatterAndLMM(
  expr(Adrenal_mass)
  , thisYLab = "adrenal mass (mg)"
)

statsAndPlot_RelAdrenalMass_ALPS <- comboTrt_scatterAndLMM(
  expr(Adrenal_mass_perBodyAM_g)
  , thisYLab = "adrenal mass (mg/g)\nnormalized to AM mass"
)

statsAndPlot_RelAdrenalMassPM_ALPS <- comboTrt_scatterAndLMM(
  expr(Adrenal_mass_perBody_g)
  , thisYLab = "adrenal mass\nnormalized (mg/g)"
)

statsAndPlot_SeminalVesicleMass_ALPS <- comboTrt_scatterAndLMM(
  expr(ReproTract_mass)
  , thisYLab = "seminal vesicle\nmass (mg)"
)

statsAndPlot_RelSeminalVesicleMass_ALPS <- comboTrt_scatterAndLMM(
  expr(ReproTract_mass_perBodyAM_g)
  , thisYLab = "seminal vesicle mass (mg/g)\nnormalized to AM mass"
)

statsAndPlot_RelSeminalVesicleMassPM_ALPS <- comboTrt_scatterAndLMM(
  expr(ReproTract_mass_perBody_g)
  , thisYLab = "seminal vesicle mass\nnormalized (mg/g)"
)

statsAndPlot_TestesMass_ALPS <- comboTrt_scatterAndLMM(
  expr(Gonad_mass)
  , thisYLab = "testicular mass (mg)"
)

statsAndPlot_RelTestesMass_ALPS <- comboTrt_scatterAndLMM(
  expr(Gonad_mass_perBodyAM_g)
  , thisYLab = "testicular mass (mg/g)\nnormalized to AM mass"
)

statsAndPlot_RelTestesMassPM_ALPS <- comboTrt_scatterAndLMM(
  expr(Gonad_mass_perBody_g)
  , thisYLab = "testicular mass\nnormalized (mg/g)"
)


## Corticosterone ----------------------------------------------------------

mixedForCortAdmin <- function(df, depVar){
  depVarSym <- rlang::sym(depVar)
  
  mixed(
    formula = rlang::expr(!!depVarSym ~ dosage + (1|damID))
    , data = df
    , method = "KR"
  )
}

getErrorDF_LMM_dosage <- function(lmm){
  lmm %>%
    getErrorDF_LMM(
      xVar = "dosage"
    )
}

plotError_LMM_dosage <- function(lmm_error){
  plotError_LMM(
    lmm_error
    , xVar = dosage
  )
}

dosage_scatterAndLMM <- function(
    singleVar # as expr()
    , thisYLab = ""
    , thisFontSize = textSize
    , thisDotSize = dotSize
    , twoLineXLabs = FALSE
    , useFacetLabels = FALSE
    , useSpecYLab = TRUE
    , addLegend = FALSE
    , removeXTicks = FALSE
    , alpha = 0.7
    , addMeanSE = FALSE
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
      scatterPlot_general(
        xVar = dosage
        , xLab = "dosage (mg/kg)"
        , yVar = !! singleVar
        , yLab = yLabel
        , dotSize = thisDotSize
        , textSize = thisFontSize
        , fillVar = dosage
        , fillValues = alpha(c("white", "black"), alpha)
        , zoom_y = zoom_y
        , ymin = ymin
        , ymax = ymax
        , fillAlpha = alpha
        , addMean = addMeanSE
        , addSE = addMeanSE
        , hideXAxisLab = FALSE
      )
    
    if(twoLineXLabs){
      plot <- plot +
        theme(
          axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)
        )
      
    }
    
    if(useFacetLabels){
      plot <- plot + facet_wrap(
        ~dosage
        , ncol = 2
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
    
    lmm <- mixedForCortAdmin(df, singleVar)
    
    lmm_error <- getErrorDF_LMM_dosage(lmm)
    
    plot <- plot +
      plotError_LMM_dosage(lmm_error)
    return(
      output = list(
        plot = plot
        , lmm = lmm
        , lmm_error = lmm_error
      ))
  }
  return(plotFunc)
}

### Mass funcs ------------------

statsAndPlot_BodyMassAM_dosage <- dosage_scatterAndLMM(
  expr(Body_mass_AM)
  , thisYLab = "body mass (g)"
)
statsAndPlot_ChangeBodyMass_dosage <- dosage_scatterAndLMM(
  expr(bodyMass_diff)
  , thisYLab = "\u0394 body mass (g)"
)

statsAndPlot_PercChangeBodyMass_dosage <- dosage_scatterAndLMM(
  expr(percChangeBodyMass)
  , thisYLab = "% \u0394 body mass"
)

statsAndPlot_AdrenalMass_dosage <- dosage_scatterAndLMM(
  expr(Adrenal_mass)
  , thisYLab = "adrenal mass (mg)"
)

statsAndPlot_RelAdrenalMass_dosage <- dosage_scatterAndLMM(
  expr(Adrenal_mass_perBodyAM_g)
  , thisYLab = "adrenal mass (mg/g)\nnormalized to AM mass"
)

statsAndPlot_RelAdrenalMassPM_dosage <- dosage_scatterAndLMM(
  expr(Adrenal_mass_perBody_g)
  , thisYLab = "adrenal mass\nnormalized (mg/g)"
)

statsAndPlot_SeminalVesicleMass_dosage <- dosage_scatterAndLMM(
  expr(ReproTract_mass)
  , thisYLab = "seminal vesicle mass (mg)"
)

statsAndPlot_RelSeminalVesicleMass_dosage <- dosage_scatterAndLMM(
  expr(ReproTract_mass_perBodyAM_g)
  , thisYLab = "seminal vesicle mass (mg/g)\nnormalized to AM mass"
)

statsAndPlot_RelSeminalVesicleMassPM_dosage <- dosage_scatterAndLMM(
  expr(ReproTract_mass_perBody_g)
  , thisYLab = "seminal vesicle mass\nnormalized (mg/g)"
)

statsAndPlot_TestesMass_dosage <- dosage_scatterAndLMM(
  expr(Gonad_mass)
  , thisYLab = "testicular mass (mg)"
)

statsAndPlot_RelTestesMass_dosage <- dosage_scatterAndLMM(
  expr(Gonad_mass_perBodyAM_g)
  , thisYLab = "testicular mass (mg/g)\nnormalized to AM mass"
)

statsAndPlot_RelTestesMassPM_dosage <- dosage_scatterAndLMM(
  expr(Gonad_mass_perBody_g)
  , thisYLab = "testicular mass\nnormalized (mg/g)"
)

# ALPS effects ------------------

## AM Body mass ----------

maleBodyMassAM_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_BodyMassAM_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 45
  )

maleBodyMassAM_ALPS_plot <- maleBodyMassAM_ALPS$plot +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40))
maleBodyMassAM_ALPS_lmm <- maleBodyMassAM_ALPS$lmm

# adult trt
maleBodyMassAM_ALPS_lmm_emm_adultTrt <- emmeans(
  maleBodyMassAM_ALPS_lmm
  , "adultTrt"
)

# males that went on to receive ALPS were larger by about a gram
# than those that received CON treatment
maleBodyMassAM_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleBodyMassAM_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

# early-life trt
maleBodyMassAM_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleBodyMassAM_ALPS_lmm
  , "earlyLifeTrt"
)

maleBodyMassAM_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleBodyMassAM_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

## % change body mass ----------
malePercChangeBodyMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_PercChangeBodyMass_ALPS(
    zoom_y = TRUE
    , ymin = -7
    , ymax = 0.5
  )

malePercChangeBodyMass_ALPS_plot <- malePercChangeBodyMass_ALPS$plot
malePercChangeBodyMass_ALPS_lmm <- malePercChangeBodyMass_ALPS$lmm

# early-life trt
malePercChangeBodyMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  malePercChangeBodyMass_ALPS_lmm
  , "earlyLifeTrt"
)

malePercChangeBodyMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  malePercChangeBodyMass_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

# adult trt
malePercChangeBodyMass_ALPS_lmm_emm_adultTrt <- emmeans(
  malePercChangeBodyMass_ALPS_lmm
  , "adultTrt"
)

# ALPS males lose more of their body mass during the paradigm
malePercChangeBodyMass_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  malePercChangeBodyMass_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

# combo trt
malePercChangeBodyMass_ALPS_lmm_emm_comboTrt <- emmeans(
  malePercChangeBodyMass_ALPS_lmm
  , "earlyLifeTrt"
  , by = "adultTrt"
)

malePercChangeBodyMass_ALPS_lmm_emm_comboTrt.pairs <- contrast(
  malePercChangeBodyMass_ALPS_lmm_emm_comboTrt
  , "pairwise"
  , simple = list("earlyLifeTrt")
  , combine = TRUE
  , adjust = "holm"
)

## normalized adrenal mass -----------
maleRelAdrenalMassPM_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelAdrenalMassPM_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = .2
  )

maleRelAdrenalMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelAdrenalMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = .2
  )

maleRelAdrenalMassPM_ALPS_plot <- maleRelAdrenalMassPM_ALPS$plot
maleRelAdrenalMassPM_ALPS_lmm <- maleRelAdrenalMassPM_ALPS$lmm

maleRelAdrenalMass_ALPS_plot <- maleRelAdrenalMass_ALPS$plot
maleRelAdrenalMass_ALPS_lmm <- maleRelAdrenalMass_ALPS$lmm

## AM - early life EMM
maleRelAdrenalMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleRelAdrenalMass_ALPS_lmm
  , "earlyLifeTrt"
)

maleRelAdrenalMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleRelAdrenalMass_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)


## AM - adult EMM
maleRelAdrenalMass_ALPS_lmm_emm_adultTrt <- emmeans(
  maleRelAdrenalMass_ALPS_lmm
  , "adultTrt"
)

# ALPS males have slightly smaller adrenal mass than CON males
# but this may be attributable to the difference in AM body mass
maleRelAdrenalMass_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleRelAdrenalMass_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

## PM - earlyLife EMM
maleRelAdrenalMassPM_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleRelAdrenalMassPM_ALPS_lmm
  , "earlyLifeTrt"
)

maleRelAdrenalMassPM_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleRelAdrenalMassPM_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

## PM - adult EMM
maleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt <- emmeans(
  maleRelAdrenalMassPM_ALPS_lmm
  , "adultTrt"
)

maleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

## absolute adrenal mass -----------
maleAdrenalMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_AdrenalMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 5
  )

maleAdrenalMass_ALPS_plot <- maleAdrenalMass_ALPS$plot
maleAdrenalMass_ALPS_lmm <- maleAdrenalMass_ALPS$lmm

## earlyLife EMM
maleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleAdrenalMass_ALPS_lmm
  , "earlyLifeTrt"
)

maleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

## adult EMM
maleAdrenalMass_ALPS_lmm_emm_adultTrt <- emmeans(
  maleAdrenalMass_ALPS_lmm
  , "adultTrt"
)

maleAdrenalMass_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleAdrenalMass_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

# no differences in absolute mass - is this a better measure because
# we've got all these differences in body mass that make it hard to know how to normalize?


## normalized seminal vesicle mass -----------
maleRelSeminalVesicleMassPM_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelSeminalVesicleMassPM_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 11
  ) 

maleRelSeminalVesicleMassPM_ALPS_plot <- maleRelSeminalVesicleMassPM_ALPS$plot +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
maleRelSeminalVesicleMassPM_ALPS_lmm <- maleRelSeminalVesicleMassPM_ALPS$lmm

maleRelSeminalVesicleMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelSeminalVesicleMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 11
  ) 

maleRelSeminalVesicleMass_ALPS_plot <- maleRelSeminalVesicleMass_ALPS$plot +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
maleRelSeminalVesicleMass_ALPS_lmm <- maleRelSeminalVesicleMass_ALPS$lmm

## AM - early-life

maleRelSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleRelSeminalVesicleMass_ALPS_lmm
  , "earlyLifeTrt"
)

# LBN males have larger seminal vesicle mass when normalized
# to AM body mass
maleRelSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleRelSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

## AM - adult

maleRelSeminalVesicleMass_ALPS_lmm_emm_adultTrt <- emmeans(
  maleRelSeminalVesicleMass_ALPS_lmm
  , "adultTrt"
)

# LBN males have larger seminal vesicle mass when normalized
# to AM body mass
maleRelSeminalVesicleMass_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleRelSeminalVesicleMass_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

## PM - early-life

maleRelSeminalVesicleMassPM_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleRelSeminalVesicleMassPM_ALPS_lmm
  , "earlyLifeTrt"
)

maleRelSeminalVesicleMassPM_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleRelSeminalVesicleMassPM_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

## PM - adult

maleRelSeminalVesicleMassPM_ALPS_lmm_emm_adultTrt <- emmeans(
  maleRelSeminalVesicleMassPM_ALPS_lmm
  , "adultTrt"
)

maleRelSeminalVesicleMassPM_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleRelSeminalVesicleMassPM_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

## absolute seminal vesicle mass -----------
maleSeminalVesicleMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_SeminalVesicleMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 325
  )

maleSeminalVesicleMass_ALPS_plot <- maleSeminalVesicleMass_ALPS$plot
maleSeminalVesicleMass_ALPS_lmm <- maleSeminalVesicleMass_ALPS$lmm

# no differences in absolute mass from early-life treatment
# trend towards increase in seminal vesicle mass from adult stress

maleSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleSeminalVesicleMass_ALPS_lmm
  , "earlyLifeTrt"
)

maleSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

maleSeminalVesicleMass_ALPS_lmm_emm_adultTrt <- emmeans(
  maleSeminalVesicleMass_ALPS_lmm
  , "adultTrt"
)

maleSeminalVesicleMass_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleSeminalVesicleMass_ALPS_lmm_emm_adultTrt
  , "pairwise"
)


## normalized testicular mass -----------
maleRelTestesMassPM_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelTestesMassPM_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 11
  ) 

maleRelTestesMassPM_ALPS_plot <- maleRelTestesMassPM_ALPS$plot + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
maleRelTestesMassPM_ALPS_lmm <- maleRelTestesMassPM_ALPS$lmm

maleRelTestesMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelTestesMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 11
  ) 

maleRelTestesMass_ALPS_plot <- maleRelTestesMass_ALPS$plot + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
maleRelTestesMass_ALPS_lmm <- maleRelTestesMass_ALPS$lmm

# AM - Early-life trt

maleRelTestesMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleRelTestesMass_ALPS_lmm
  , "earlyLifeTrt"
)

maleRelTestesMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleRelTestesMass_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

# AM - Adult trt

maleRelTestesMass_ALPS_lmm_emm_adultTrt <- emmeans(
  maleRelTestesMass_ALPS_lmm
  , "adultTrt"
)

# LBN males have larger testicular mass when normalized
# to AM body mass
maleRelTestesMass_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleRelTestesMass_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

# PM - early-life trt
maleRelTestesMassPM_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleRelTestesMassPM_ALPS_lmm
  , "earlyLifeTrt"
)

maleRelTestesMassPM_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleRelTestesMassPM_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

# PM - adult trt
maleRelTestesMassPM_ALPS_lmm_emm_adultTrt <- emmeans(
  maleRelTestesMassPM_ALPS_lmm
  , "adultTrt"
)

maleRelTestesMassPM_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleRelTestesMassPM_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

## absolute testicular mass -----------
maleTestesMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_TestesMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 325
  )

maleTestesMass_ALPS_plot <- maleTestesMass_ALPS$plot
maleTestesMass_ALPS_lmm <- maleTestesMass_ALPS$lmm

# both types of stress decreased absolute testicular mass

maleTestesMass_ALPS_lmm_emm_adultTrt <- emmeans(
  maleTestesMass_ALPS_lmm
  , "adultTrt"
)

maleTestesMass_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  maleTestesMass_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

maleTestesMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleTestesMass_ALPS_lmm
  , "earlyLifeTrt"
)

maleTestesMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  maleTestesMass_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)

# Cort effects ------------------

## AM Body mass ----------

maleBodyMassAM_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_BodyMassAM_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 45
  )

maleBodyMassAM_dosage_plot <- maleBodyMassAM_dosage$plot +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40))
maleBodyMassAM_dosage_lmm <- maleBodyMassAM_dosage$lmm

## Emmeans 

maleBodyMassAM_dosage_lmm_emm_dosage <- emmeans(
  maleBodyMassAM_dosage_lmm
  , "dosage"
)

maleBodyMassAM_dosage_lmm_emm_dosage.pairs <- contrast(
  maleBodyMassAM_dosage_lmm_emm_dosage
  , "pairwise"
)

## % change body mass ----------
malePercChangeBodyMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_PercChangeBodyMass_dosage(
    zoom_y = TRUE
    , ymin = -7
    , ymax = 0.5
  )

malePercChangeBodyMass_dosage_plot <- malePercChangeBodyMass_dosage$plot
malePercChangeBodyMass_dosage_lmm <- malePercChangeBodyMass_dosage$lmm

# trend to lose more mass with corticosterone


## Emmeans 

malePercChangeBodyMass_dosage_lmm_emm_dosage <- emmeans(
  malePercChangeBodyMass_dosage_lmm
  , "dosage"
)

malePercChangeBodyMass_dosage_lmm_emm_dosage.pairs <- contrast(
  malePercChangeBodyMass_dosage_lmm_emm_dosage
  , "pairwise"
)

## normalized adrenal mass -----------
maleRelAdrenalMassPM_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelAdrenalMassPM_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = .2
  )

maleRelAdrenalMassPM_dosage_plot <- maleRelAdrenalMassPM_dosage$plot
maleRelAdrenalMassPM_dosage_lmm <- maleRelAdrenalMassPM_dosage$lmm

## Emmeans 

maleRelAdrenalMassPM_dosage_lmm_emm_dosage <- emmeans(
  maleRelAdrenalMassPM_dosage_lmm
  , "dosage"
)

maleRelAdrenalMassPM_dosage_lmm_emm_dosage.pairs <- contrast(
  maleRelAdrenalMassPM_dosage_lmm_emm_dosage
  , "pairwise"
)


maleRelAdrenalMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelAdrenalMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = .2
  )

maleRelAdrenalMass_dosage_plot <- maleRelAdrenalMass_dosage$plot
maleRelAdrenalMass_dosage_lmm <- maleRelAdrenalMass_dosage$lmm

## Emmeans 

maleRelAdrenalMass_dosage_lmm_emm_dosage <- emmeans(
  maleRelAdrenalMass_dosage_lmm
  , "dosage"
)

maleRelAdrenalMass_dosage_lmm_emm_dosage.pairs <- contrast(
  maleRelAdrenalMass_dosage_lmm_emm_dosage
  , "pairwise"
)

# no change

## absolute adrenal mass -----------
maleAdrenalMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_AdrenalMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 5
  )

maleAdrenalMass_dosage_plot <- maleAdrenalMass_dosage$plot
maleAdrenalMass_dosage_lmm <- maleAdrenalMass_dosage$lmm

## Emmeans 

maleAdrenalMass_dosage_lmm_emm_dosage <- emmeans(
  maleAdrenalMass_dosage_lmm
  , "dosage"
)

maleAdrenalMass_dosage_lmm_emm_dosage.pairs <- contrast(
  maleAdrenalMass_dosage_lmm_emm_dosage
  , "pairwise"
)

# very weak trend, increased adrenal mass with cort

## normalized seminal vesicle mass -----------
maleRelSeminalVesicleMassPM_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelSeminalVesicleMassPM_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 11
  ) 

maleRelSeminalVesicleMassPM_dosage_plot <- maleRelSeminalVesicleMassPM_dosage$plot + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
maleRelSeminalVesicleMassPM_dosage_lmm <- maleRelSeminalVesicleMassPM_dosage$lmm

## Emmeans 

maleRelSeminalVesicleMassPM_dosage_lmm_emm_dosage <- emmeans(
  maleRelSeminalVesicleMassPM_dosage_lmm
  , "dosage"
)

maleRelSeminalVesicleMassPM_dosage_lmm_emm_dosage.pairs <- contrast(
  maleRelSeminalVesicleMassPM_dosage_lmm_emm_dosage
  , "pairwise"
)

maleRelSeminalVesicleMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelSeminalVesicleMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 11
  ) 

maleRelSeminalVesicleMass_dosage_plot <- maleRelSeminalVesicleMass_dosage$plot + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
maleRelSeminalVesicleMass_dosage_lmm <- maleRelSeminalVesicleMass_dosage$lmm

## Emmeans 

maleRelSeminalVesicleMass_dosage_lmm_emm_dosage <- emmeans(
  maleRelSeminalVesicleMass_dosage_lmm
  , "dosage"
)

maleRelSeminalVesicleMass_dosage_lmm_emm_dosage.pairs <- contrast(
  maleRelSeminalVesicleMass_dosage_lmm_emm_dosage
  , "pairwise"
)

# no change

## absolute seminal vesicle mass -----------
maleSeminalVesicleMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_SeminalVesicleMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 325
  )

maleSeminalVesicleMass_dosage_plot <- maleSeminalVesicleMass_dosage$plot
maleSeminalVesicleMass_dosage_lmm <- maleSeminalVesicleMass_dosage$lmm

## Emmeans 

maleSeminalVesicleMass_dosage_lmm_emm_dosage <- emmeans(
  maleSeminalVesicleMass_dosage_lmm
  , "dosage"
)

maleSeminalVesicleMass_dosage_lmm_emm_dosage.pairs <- contrast(
  maleSeminalVesicleMass_dosage_lmm_emm_dosage
  , "pairwise"
)

# no change

## normalized testicular mass -----------
maleRelTestesMassPM_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelTestesMassPM_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 11
  ) 

maleRelTestesMassPM_dosage_plot <- maleRelTestesMassPM_dosage$plot + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
maleRelTestesMassPM_dosage_lmm <- maleRelTestesMassPM_dosage$lmm

## Emmeans 

maleRelTestesMassPM_dosage_lmm_emm_dosage <- emmeans(
  maleRelTestesMassPM_dosage_lmm
  , "dosage"
)

maleRelTestesMassPM_dosage_lmm_emm_dosage.pairs <- contrast(
  maleRelTestesMassPM_dosage_lmm_emm_dosage
  , "pairwise"
)

maleRelTestesMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelTestesMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 11
  ) 

maleRelTestesMass_dosage_plot <- maleRelTestesMass_dosage$plot + 
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10))
maleRelTestesMass_dosage_lmm <- maleRelTestesMass_dosage$lmm

## Emmeans 

maleRelTestesMass_dosage_lmm_emm_dosage <- emmeans(
  maleRelTestesMass_dosage_lmm
  , "dosage"
)

maleRelTestesMass_dosage_lmm_emm_dosage.pairs <- contrast(
  maleRelTestesMass_dosage_lmm_emm_dosage
  , "pairwise"
)

# weak trend for decrease in testicular mass with cort

## absolute testicular mass -----------
maleTestesMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_TestesMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 325
  )

maleTestesMass_dosage_plot <- maleTestesMass_dosage$plot
maleTestesMass_dosage_lmm <- maleTestesMass_dosage$lmm

## Emmeans 

maleTestesMass_dosage_lmm_emm_dosage <- emmeans(
  maleTestesMass_dosage_lmm
  , "dosage"
)

maleTestesMass_dosage_lmm_emm_dosage.pairs <- contrast(
  maleTestesMass_dosage_lmm_emm_dosage
  , "pairwise"
)

# weak trend for decrease in testicular mass with cort
