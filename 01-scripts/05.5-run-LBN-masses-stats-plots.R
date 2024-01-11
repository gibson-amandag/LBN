
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
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = "magenta"
  )
}

comboTrt_scatterAndLMM <- function(
    singleVar # as expr()
    , thisYLab = ""
    , thisFontSize = textSize
    , thisDotSize = dotSize
    , twoLineXLabs = FALSE
    , useFacetLabels = FALSE
    , useSpecYLab = TRUE
    , addLegend = FALSE
    , removeXTicks = TRUE
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
      plot <- plot + 
        theme(
          axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)
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
          legend.position = "top"
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
  , thisYLab = "normalized to AM mass\nadrenal mass (mg/g)"
)

statsAndPlot_RelAdrenalMassPM_ALPS <- comboTrt_scatterAndLMM(
  expr(Adrenal_mass_perBody_g)
  , thisYLab = "normalized to PM mass\nadrenal mass (mg/g)"
)

statsAndPlot_SeminalVesicleMass_ALPS <- comboTrt_scatterAndLMM(
  expr(ReproTract_mass)
  , thisYLab = "seminal vesicle mass (mg)"
)

statsAndPlot_RelSeminalVesicleMass_ALPS <- comboTrt_scatterAndLMM(
  expr(ReproTract_mass_perBodyAM_g)
  , thisYLab = "normalized to AM mass\nseminal vesicle mass (mg/g)"
)

statsAndPlot_RelSeminalVesicleMassPM_ALPS <- comboTrt_scatterAndLMM(
  expr(ReproTract_mass_perBody_g)
  , thisYLab = "normalized to PM mass\nseminal vesicle mass (mg/g)"
)

statsAndPlot_TestesMass_ALPS <- comboTrt_scatterAndLMM(
  expr(Gonad_mass)
  , thisYLab = "testicular mass (mg)"
)

statsAndPlot_RelTestesMass_ALPS <- comboTrt_scatterAndLMM(
  expr(Gonad_mass_perBodyAM_g)
  , thisYLab = "normalized to AM mass\ntesticular mass (mg/g)"
)

statsAndPlot_RelTestesMassPM_ALPS <- comboTrt_scatterAndLMM(
  expr(Gonad_mass_perBody_g)
  , thisYLab = "normalized to PM mass\ntesticular mass (mg/g)"
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
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
    , meanBarWidth = 0.7
    , color = "magenta"
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
        , fillValues = c("white", "black")
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
  , thisYLab = "normalized to AM mass\nadrenal mass (mg/g)"
)

statsAndPlot_RelAdrenalMassPM_dosage <- dosage_scatterAndLMM(
  expr(Adrenal_mass_perBody_g)
  , thisYLab = "normalized to PM mass\nadrenal mass (mg/g)"
)

statsAndPlot_SeminalVesicleMass_dosage <- dosage_scatterAndLMM(
  expr(ReproTract_mass)
  , thisYLab = "seminal vesicle mass (mg)"
)

statsAndPlot_RelSeminalVesicleMass_dosage <- dosage_scatterAndLMM(
  expr(ReproTract_mass_perBodyAM_g)
  , thisYLab = "normalized to AM mass\nseminal vesicle mass (mg/g)"
)

statsAndPlot_RelSeminalVesicleMassPM_dosage <- dosage_scatterAndLMM(
  expr(ReproTract_mass_perBody_g)
  , thisYLab = "normalized to PM mass\nseminal vesicle mass (mg/g)"
)

statsAndPlot_TestesMass_dosage <- dosage_scatterAndLMM(
  expr(Gonad_mass)
  , thisYLab = "testicular mass (mg)"
)

statsAndPlot_RelTestesMass_dosage <- dosage_scatterAndLMM(
  expr(Gonad_mass_perBodyAM_g)
  , thisYLab = "normalized to AM mass\ntesticular mass (mg/g)"
)

statsAndPlot_RelTestesMassPM_dosage <- dosage_scatterAndLMM(
  expr(Gonad_mass_perBody_g)
  , thisYLab = "normalized to PM mass\ntesticular mass (mg/g)"
)

# ALPS effects ------------------

## AM Body mass ----------

maleBodyMassAM_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_BodyMassAM_ALPS()

maleBodyMassAM_ALPS_plot <- maleBodyMassAM_ALPS$plot
maleBodyMassAM_ALPS_lmm <- maleBodyMassAM_ALPS$lmm

maleBodyMassAM_ALPS_lmm_emm_adultTrt <- emmeans(
  maleBodyMassAM_ALPS_lmm
  , "adultTrt"
)

# males that went on to receive ALPS were larger by about a gram
# than those that received CON treatment
maleBodyMassAM_ALPS_lmm_emm_adultTrt.pairs <- test(
  pairs(maleBodyMassAM_ALPS_lmm_emm_adultTrt)
  , by = NULL
  , adjust = "holm"
)

## % change body mass ----------
malePercChangeBodyMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_PercChangeBodyMass_ALPS()

malePercChangeBodyMass_ALPS_plot <- malePercChangeBodyMass_ALPS$plot
malePercChangeBodyMass_ALPS_lmm <- malePercChangeBodyMass_ALPS$lmm

malePercChangeBodyMass_ALPS_lmm_emm_adultTrt <- emmeans(
  malePercChangeBodyMass_ALPS_lmm
  , "adultTrt"
)

# ALPS males lose more of their body mass during the paradigm
malePercChangeBodyMass_ALPS_lmm_emm_adultTrt.pairs <- test(
  pairs(malePercChangeBodyMass_ALPS_lmm_emm_adultTrt)
  , by = NULL
  , adjust = "holm"
)

## normalized adrenal mass -----------
maleRelAdrenalMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelAdrenalMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = .2
  )

maleRelAdrenalMass_ALPS_plot <- maleRelAdrenalMass_ALPS$plot
maleRelAdrenalMass_ALPS_lmm <- maleRelAdrenalMass_ALPS$lmm

maleRelAdrenalMass_ALPS_lmm_emm_adultTrt <- emmeans(
  maleRelAdrenalMass_ALPS_lmm
  , "adultTrt"
)

# ALPS males have slightly smaller adrenal mass than CON males
# but this may be attributable to the difference in AM body mass
maleRelAdrenalMass_ALPS_lmm_emm_adultTrt.pairs <- test(
  pairs(maleRelAdrenalMass_ALPS_lmm_emm_adultTrt)
  , by = NULL
  , adjust = "holm"
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

# no differences in absolute mass - is this a better measure because
# we've got all these differences in body mass that make it hard to know how to normalize?


## normalized seminal vesicle mass -----------
maleRelSeminalVesicleMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelSeminalVesicleMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  )

maleRelSeminalVesicleMass_ALPS_plot <- maleRelSeminalVesicleMass_ALPS$plot
maleRelSeminalVesicleMass_ALPS_lmm <- maleRelSeminalVesicleMass_ALPS$lmm

maleRelSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleRelSeminalVesicleMass_ALPS_lmm
  , "earlyLifeTrt"
)

# LBN males have larger seminal vesicle mass when normalized
# to AM body mass
maleRelSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- test(
  pairs(maleRelSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt)
  , by = NULL
  , adjust = "holm"
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

## normalized testicular mass -----------
maleRelTestesMass_ALPS <- acuteStressFilteredMales %>%
  statsAndPlot_RelTestesMass_ALPS(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  )

maleRelTestesMass_ALPS_plot <- maleRelTestesMass_ALPS$plot
maleRelTestesMass_ALPS_lmm <- maleRelTestesMass_ALPS$lmm

maleRelTestesMass_ALPS_lmm_emm_adultTrt <- emmeans(
  maleRelTestesMass_ALPS_lmm
  , "adultTrt"
)

# LBN males have larger testicular mass when normalized
# to AM body mass
maleRelTestesMass_ALPS_lmm_emm_adultTrt.pairs <- test(
  pairs(maleRelTestesMass_ALPS_lmm_emm_adultTrt)
  , by = NULL
  , adjust = "holm"
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

maleTestesMass_ALPS_lmm_emm_adultTrt.pairs <- test(
  pairs(maleTestesMass_ALPS_lmm_emm_adultTrt)
  , by = NULL
  , adjust = "holm"
)

maleTestesMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  maleTestesMass_ALPS_lmm
  , "earlyLifeTrt"
)

maleTestesMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- test(
  pairs(maleTestesMass_ALPS_lmm_emm_earlyLifeTrt)
  , by = NULL
  , adjust = "holm"
)

# Cort effects ------------------

## AM Body mass ----------

maleBodyMassAM_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_BodyMassAM_dosage()

maleBodyMassAM_dosage_plot <- maleBodyMassAM_dosage$plot
maleBodyMassAM_dosage_lmm <- maleBodyMassAM_dosage$lmm

## % change body mass ----------
malePercChangeBodyMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_PercChangeBodyMass_dosage()

malePercChangeBodyMass_dosage_plot <- malePercChangeBodyMass_dosage$plot
malePercChangeBodyMass_dosage_lmm <- malePercChangeBodyMass_dosage$lmm

# trend to lose more mass with corticosterone

## normalized adrenal mass -----------
maleRelAdrenalMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelAdrenalMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = .2
  )

maleRelAdrenalMass_dosage_plot <- maleRelAdrenalMass_dosage$plot
maleRelAdrenalMass_dosage_lmm <- maleRelAdrenalMass_dosage$lmm

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

# very weak trend, increased adrenal mass with cort

## normalized seminal vesicle mass -----------
maleRelSeminalVesicleMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelSeminalVesicleMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  )

maleRelSeminalVesicleMass_dosage_plot <- maleRelSeminalVesicleMass_dosage$plot
maleRelSeminalVesicleMass_dosage_lmm <- maleRelSeminalVesicleMass_dosage$lmm

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

# no change

## normalized testicular mass -----------
maleRelTestesMass_dosage <- maleCortAdmin_filtered %>%
  statsAndPlot_RelTestesMass_dosage(
    zoom_y = TRUE
    , ymin = 0
    , ymax = 10
  )

maleRelTestesMass_dosage_plot <- maleRelTestesMass_dosage$plot
maleRelTestesMass_dosage_lmm <- maleRelTestesMass_dosage$lmm

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

# weak trend for decrease in testicular mass with cort
