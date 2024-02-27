
# Functions ---------------------------------------------------------------


## ALPS --------------------------------------------------------------------


lmmForComboTrtCycle <- function(df, depVar) {
  # depVar as expr(varName)
  depVarSym <- rlang::sym(depVar)
  
  mixed(
    formula = rlang::expr(!!depVarSym ~ earlyLifeTrt * adultTrt * Sac_cycle + (1|damID))
    , data = df
    , method = "KR"
  )
}

getErrorDF_LMM_comboTrtCycle <- function(lmm){
  emmeans(
      lmm
      , "earlyLifeTrt"
      , by = c("adultTrt", "Sac_cycle")
    ) %>%
    as_data_frame() %>%
    rename(
      y = emmean
    ) %>%
    mutate(
      lower = y - SE
      , upper = y + SE
    ) %>%
    combineStress()
}

comboTrtCycle_scatterAndLMM <- function(
    singleVar # as expr()
    , thisYLab = ""
    , thisFontSize = textSize
    , thisDotSize = dotSize
    , twoLineXLabs = TRUE
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
      ) +
      facet_wrap(
        ~ Sac_cycle
        , nrow = 1
      )
    
    if(twoLineXLabs){
      # plot <- plot + 
      #   theme(
      #     axis.text.x = element_text(angle = 35, vjust = 1, hjust=1)
      #   )
      plot <- plot + 
        scale_x_discrete(
          labels = c(
            "STD-CON" = "STD\nCON"
            , "STD-ALPS" = "STD\nALPS"
            , "LBN-CON" = "LBN\nCON"
            , "LBN-ALPS" = "LBN\nALPS"

          )
        ) +
        theme( # fully vertical
          axis.text.x = element_text(
            angle = 90
            , vjust = 0.5
            )
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
    
    lmm <- df %>% lmmForComboTrtCycle(singleVar)

    lmm_error <- getErrorDF_LMM_comboTrtCycle(lmm)

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

statsAndPlot_BodyMassAM_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(Body_mass_AM)
  , thisYLab = "body mass (g)"
  , addLegend = TRUE
)
statsAndPlot_ChangeBodyMass_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(bodyMass_diff)
  , thisYLab = "\u0394 body mass (g)"
)

statsAndPlot_PercChangeBodyMass_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(percChangeBodyMass)
  , thisYLab = "% \u0394 body mass"
)

statsAndPlot_AdrenalMass_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(Adrenal_mass)
  , thisYLab = "adrenal mass (mg)"
)

statsAndPlot_RelAdrenalMass_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(Adrenal_mass_perBodyAM_g)
  , thisYLab = "normalized to AM mass\nadrenal mass (mg/g)"
)

statsAndPlot_RelAdrenalMassPM_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(Adrenal_mass_perBody_g)
  , thisYLab = "normalized to PM mass\nadrenal mass (mg/g)"
)

statsAndPlot_UterineMass_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(ReproTract_mass)
  , thisYLab = "uterine mass (mg)"
)

statsAndPlot_RelUterineMass_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(ReproTract_mass_perBodyAM_g)
  , thisYLab = "normalized to AM mass\nuterine mass (mg/g)"
)

statsAndPlot_RelUterineMassPM_ALPS_f <- comboTrtCycle_scatterAndLMM(
  expr(ReproTract_mass_perBody_g)
  , thisYLab = "normalized to PM mass\nsuterine mass (mg/g)"
)
# ALPS effects ------------------

## AM Body mass ----------

femaleBodyMassAM_ALPS <- acuteStressFilteredFemales %>%
  statsAndPlot_BodyMassAM_ALPS_f()

femaleBodyMassAM_ALPS_plot <- femaleBodyMassAM_ALPS$plot
femaleBodyMassAM_ALPS_lmm <- femaleBodyMassAM_ALPS$lmm

# no sig effects

## % change body mass ----------
femalePercChangeBodyMass_ALPS <- acuteStressFilteredFemales %>%
  statsAndPlot_PercChangeBodyMass_ALPS_f()

femalePercChangeBodyMass_ALPS_plot <- femalePercChangeBodyMass_ALPS$plot
femalePercChangeBodyMass_ALPS_lmm <- femalePercChangeBodyMass_ALPS$lmm

femalePercChangeBodyMass_ALPS_lmm_emm_adultTrt <- emmeans(
  femalePercChangeBodyMass_ALPS_lmm
  , "adultTrt"
)

femalePercChangeBodyMass_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  femalePercChangeBodyMass_ALPS_lmm_emm_adultTrt
  , "pairwise"
)

femalePercChangeBodyMass_ALPS_lmm_emm_Sac_cycle <- emmeans(
  femalePercChangeBodyMass_ALPS_lmm
  , "Sac_cycle"
)

femalePercChangeBodyMass_ALPS_lmm_emm_Sac_cycle.pairs <- contrast(
  femalePercChangeBodyMass_ALPS_lmm_emm_Sac_cycle
  , "pairwise"
)

## normalized adrenal mass -----------
femaleRelAdrenalMass_ALPS <- acuteStressFilteredFemales %>%
  statsAndPlot_RelAdrenalMass_ALPS_f()

femaleRelAdrenalMass_ALPS_plot <- femaleRelAdrenalMass_ALPS$plot
femaleRelAdrenalMass_ALPS_lmm <- femaleRelAdrenalMass_ALPS$lmm
# no change in relative adrenal mass

### normalized to PM
femaleRelAdrenalMassPM_ALPS <- acuteStressFilteredFemales %>%
  statsAndPlot_RelAdrenalMassPM_ALPS_f()

femaleRelAdrenalMassPM_ALPS_plot <- femaleRelAdrenalMassPM_ALPS$plot
femaleRelAdrenalMassPM_ALPS_lmm <- femaleRelAdrenalMassPM_ALPS$lmm

#### Trend adult treatment
femaleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt <- emmeans(
  femaleRelAdrenalMassPM_ALPS_lmm
  , "adultTrt"
)

# ALPS males lose more of their body mass during the paradigm
femaleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt.pairs <- contrast(
  femaleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt
  , "pairwise"
)


## absolute adrenal mass -----------
femaleAdrenalMass_ALPS <- acuteStressFilteredFemales %>%
  statsAndPlot_AdrenalMass_ALPS_f()

femaleAdrenalMass_ALPS_plot <- femaleAdrenalMass_ALPS$plot
femaleAdrenalMass_ALPS_lmm <- femaleAdrenalMass_ALPS$lmm

# LBN females higher adrenal mass

femaleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt <- emmeans(
  femaleAdrenalMass_ALPS_lmm
  , "earlyLifeTrt"
)

femaleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt.pairs <- contrast(
  femaleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt
  , "pairwise"
)


## normalized uterine mass -----------
femaleRelUterineMass_ALPS <- acuteStressFilteredFemales %>%
  statsAndPlot_RelUterineMass_ALPS_f()

femaleRelUterineMass_ALPS_plot <- femaleRelUterineMass_ALPS$plot
femaleRelUterineMass_ALPS_lmm <- femaleRelUterineMass_ALPS$lmm

femaleRelUterineMass_ALPS_lmm_emm_Sac_cycle <- emmeans(
  femaleRelUterineMass_ALPS_lmm
  , "Sac_cycle"
)

femaleRelUterineMass_ALPS_lmm_emm_Sac_cycle.pairs <- contrast(
  femaleRelUterineMass_ALPS_lmm_emm_Sac_cycle
  , "pairwise"
)

# p=0.075 interaction of cycle stage and adult treatment

femaleRelUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt <- emmeans(
  femaleRelUterineMass_ALPS_lmm
  , "adultTrt"
  , by = "Sac_cycle"
)

# trend is for proestrous ALPS uteri to be larger
femaleRelUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt.pairs <- contrast(
  femaleRelUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt
  , "pairwise"
  , simple = list("adultTrt")
  # , simple = "each"
  , combine = TRUE
  , adjust = "holm"
)

## Normalized to PM body mass
femaleRelUterineMassPM_ALPS <- acuteStressFilteredFemales %>%
  statsAndPlot_RelUterineMassPM_ALPS_f()

femaleRelUterineMassPM_ALPS_plot <- femaleRelUterineMassPM_ALPS$plot
femaleRelUterineMassPM_ALPS_lmm <- femaleRelUterineMassPM_ALPS$lmm

femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycle <- emmeans(
  femaleRelUterineMassPM_ALPS_lmm
  , "Sac_cycle"
)


femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycle.pairs <- contrast(
  femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycle
  , "pairwise"
)

# p = 0.058 for interaction

femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycleAdultTrt <- emmeans(
  femaleRelUterineMassPM_ALPS_lmm
  , "adultTrt"
  , by = "Sac_cycle"
)

# trend is for proestrous ALPS uteri to be larger
femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycleAdultTrt.pairs <- contrast(
  femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycleAdultTrt
  , "pairwise"
  # , simple = "each"
  , simple = list("adultTrt")
  , adjust = "holm"
  , combine = TRUE
)

## absolute uterine mass -----------
femaleUterineMass_ALPS <- acuteStressFilteredFemales %>%
  statsAndPlot_UterineMass_ALPS_f()

femaleUterineMass_ALPS_plot <- femaleUterineMass_ALPS$plot
femaleUterineMass_ALPS_lmm <- femaleUterineMass_ALPS$lmm

# Proestrous uteri are larger
femaleUterineMass_ALPS_lmm_emm_Sac_cycle <- emmeans(
  femaleUterineMass_ALPS_lmm
  , "Sac_cycle"
)

femaleUterineMass_ALPS_lmm_emm_Sac_cycle.pairs <- contrast(
  femaleUterineMass_ALPS_lmm_emm_Sac_cycle
  , "pairwise"
)

# trend, a bit weaker for interaction
femaleUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt <- emmeans(
  femaleUterineMass_ALPS_lmm
  , "adultTrt"
  , by = "Sac_cycle"
)

# post-hoc, neither is trending
femaleUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt.pairs <- contrast(
  femaleUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt
  , "pairwise"
  # , simple = "each"
  , simple = list("adultTrt")
  , combine = TRUE
  , adjust = "holm"
)
