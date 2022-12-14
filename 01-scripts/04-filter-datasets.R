cohorts <- c(2, 4, 6, 7, 8, 9)
minLitterSize <- 5
damBehaviorTimes <- c(1, 15, 19)

# This is a function that creates another function - makes it clearer what
# is being done for the filtering
filterLBNCohorts <- filterLBN(cohorts, minLitterSize)


# Dam Demo ----------------------------------------------------------------

damFiltered <- Demo_dam %>%
  filterLBNCohorts()


# Dam Behavior ------------------------------------------------------------

damBehaviorFiltered <- dam_behavior %>%
  filterLBNCohorts()

filterBehaviorTimes <- filterDamBehaviorTimeFunc(damBehaviorTimes)

damBehaviorFiltered_ZTs <- damBehaviorFiltered %>%
  filterBehaviorTimes()

damBehaviorFiltered_P5_P6 <- damBehaviorFiltered %>%
  filter(
    PND %in% c(5, 6)
    , ! (PND == 5 & time == 1)
    , ! (PND == 6 & time == 15)
  )
  

# Mass --------------------------------------------------------------------

massFiltered <- Mass_off %>%
  filterLBNCohorts()

mass_byDam <- massFiltered %>%
  getAvgByDam(bySex = TRUE)

mass_long <- massFiltered %>%
  makeOffMassLong()

mass_byDam_long <- mass_byDam %>%
  makeOffMassLong()


# Maturation --------------------------------------------------------------

maturationFiltered <- Maturation_off %>%
  filterLBNCohorts()

maturation_byDam_f <- maturationFiltered %>%
  filter(
    sex == "F"
  ) %>%
  getAvgByDam()

maturation_byDam_m <- maturationFiltered %>%
  filter(
    sex == "M"
  ) %>%
  getAvgByDam()


# Cycles ------------------------------------------------------------------

cyclesFiltered <- Cycles_off %>%
  filterLBNCohorts()

cyclesAllFiltered <- Cycles_off_all %>%
  filterLBNCohorts()

cyclesLong <- cyclesFiltered %>%
  makeCyclesLong()

cyclesPercLong <- cyclesFiltered %>%
  makeCyclesPercLong()


# Acute Stress ------------------------------------------------------------


acuteStressFiltered <- AcuteStress_off %>%
  filterLBNCohorts()

cortFiltered <- Cort_off %>%
  filterLBNCohorts() 

LHFiltered <- LH_off %>%
  filterLBNCohorts()


# Filter acute stress -----------------------------------------------------
proUterineMin = 125
diUterineMax = 100
exclude653 = TRUE

filterAcuteStressAll <- filterAcuteStressFunc(
  c("M", "F")
  , c("proestrus", "diestrus") #cycle stages
  , exclude653 = exclude653
  , filterUterineMass = FALSE
)

filterAcuteStress_M_DiPro <- filterAcuteStressFunc(
  c("M", "F")
  , c("proestrus", "diestrus") #cycle stages
  , exclude653 = exclude653
  , filterUterineMass = TRUE
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
)

filterAcuteStressMales <- filterAcuteStressFunc(
  "M"
  , "" #cycle stages
)

filterAcuteStressFemales_all <- filterAcuteStressFunc(
  "F"
  , c("proestrus", "diestrus")
  , filterUterineMass = FALSE
  , exclude653 = exclude653
)

filterUterineMass <- TRUE

filterAcuteStressFemales <- filterAcuteStressFunc(
  "F"
  , c("proestrus", "diestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
)


filterAcuteStressPro <- filterAcuteStressFunc(
  "F"
  , c("proestrus") #cycle stages
  , exclude653 = exclude653
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
)

filterAcuteStressDi <- filterAcuteStressFunc(
  "F"
  , c("diestrus") #cycle stages
  , exclude653 = exclude653
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
)

acuteStressFilteredAll <- acuteStressFiltered %>%
  filterAcuteStressAll()

acuteStressFiltered_M_DiPro <- acuteStressFiltered %>%
  filterAcuteStress_M_DiPro()

acuteStressFilteredMales <- acuteStressFiltered %>%
  filterAcuteStressMales()

acuteStressFilteredFemales_all <- acuteStressFiltered %>%
  filterAcuteStressFemales_all()

acuteStressFilteredFemales <- acuteStressFiltered %>%
  filterAcuteStressFemales()

acuteStressFilteredPro <- acuteStressFiltered %>%
  filterAcuteStressPro()

acuteStressFilteredDi <- acuteStressFiltered %>%
  filterAcuteStressDi()


## Filter Cort -------------------------------------------------------------
adjMaxMin = TRUE
cortMax = 1000 # changed when corrected the standards for cohort 7+8 and BD plates
cortMin = 1.95

filterCortAll <- filterCortFunc(
  c("M", "F")
  , c("proestrus", "diestrus") #cycle stages
  , exclude653 = exclude653
  , filterUterineMass = FALSE
  , cortMax = cortMax # make everything higher than this 500ng/mL
  , cortMin = cortMin # make everything lower than this 1.95 ng/mL
)

filterCort_M_DiPro <- filterCortFunc(
  c("M", "F")
  , c("proestrus", "diestrus") #cycle stages
  , exclude653 = exclude653
  , filterUterineMass = TRUE
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , cortMax = cortMax # make everything higher than this 500ng/mL
  , cortMin = cortMin # make everything lower than this 1.95 ng/mL
)

filterCortMales <- filterCortFunc(
  "M"
  , "" #cycle stages
  , cortMax = cortMax # make everything higher than this 500ng/mL
  , cortMin = cortMin # make everything lower than this 1.95 ng/mL
)

filterCortFemales_all <- filterCortFunc(
  "F"
  , c("proestrus", "diestrus")
  , filterUterineMass = FALSE
  , exclude653 = exclude653
  , cortMax = cortMax # make everything higher than this 500ng/mL
  , cortMin = cortMin # make everything lower than this 1.95 ng/mL
)

filterUterineMass <- TRUE

filterCortFemales <- filterCortFunc(
  "F"
  , c("proestrus", "diestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
  , cortMax = cortMax # make everything higher than this 500ng/mL
  , cortMin = cortMin # make everything lower than this 1.95 ng/mL
)


filterCortPro <- filterCortFunc(
  "F"
  , c("proestrus") #cycle stages
  , exclude653 = exclude653
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , cortMax = cortMax # make everything higher than this 500ng/mL
  , cortMin = cortMin # make everything lower than this 1.95 ng/mL
)

filterCortDi <- filterCortFunc(
  "F"
  , c("diestrus") #cycle stages
  , exclude653 = exclude653
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , cortMax = cortMax # make everything higher than this 500ng/mL
  , cortMin = cortMin # make everything lower than this 1.95 ng/mL
)

cortFilteredAll <- cortFiltered %>%
  filterCortAll()

cortFiltered_M_DiPro <- cortFiltered %>%
  filterCort_M_DiPro()

cortFilteredMales <- cortFiltered %>%
  filterCortMales()

cortFilteredFemales_all <- cortFiltered %>%
  filterCortFemales_all()

cortFilteredFemales <- cortFiltered %>%
  filterCortFemales()

cortFilteredPro <- cortFiltered %>%
  filterCortPro()

cortFilteredDi <- cortFiltered %>%
  filterCortDi()


## Filter LH ---------------------------------------------------------------

filterLHFemales_all <- filterLHFunc(
  c("proestrus", "diestrus")
  , filterUterineMass = FALSE
  , exclude653 = exclude653
)

filterLHFemales <- filterLHFunc(
  c("proestrus", "diestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
)

filterLHPro <- filterLHFunc(
  c("proestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
)

filterLHDi <- filterLHFunc(
  c("diestrus") #cycle stages
  , filterUterineMass = filterUterineMass
  , proUterineMin = proUterineMin
  , diUterineMax = diUterineMax
  , exclude653 = exclude653
)



LHFilteredFemales_all <- LHFiltered %>%
  filterLHFemales_all()

LHFilteredFemales <- LHFiltered %>%
  filterLHFemales()

LHFilteredPro <- LHFiltered %>%
  filterLHPro()

LHFilteredDi <- LHFiltered %>%
  filterLHDi()


surgedDF <- acuteStressFilteredPro %>%
  mutate(
    surged = maxLH > surgeMin
  )


# GABA PSCs ---------------------------------------------------------------

GABApscsFiltered <- GABApscs %>%
  filterLBNCohorts()

filterByRseries <- TRUE
RseriesMin <- 0
RseriesMax <- 20
filterByRinput <- TRUE
RinputMin <- 500
RinputMax <- 1500
filterByHoldingCurr <- FALSE
holdingCurrMin <- -50
holdingCurrMax <- 10
filterByCapacitance <- TRUE
capacitanceMin <- 5
capacitanceMax <- 20

filterByNumCells <- FALSE
maxCellNum <- 3

filterBySacHr <- TRUE
maxSacHr <- 6 # time since sacrifice
filterByRecHr <- FALSE
recHrMin <- 13 # time since lights on
recHrMax <- 20

removeNoToInclude <- TRUE

filterEphys <- filterEphysFunc(
  filterByRseries = filterByRseries
  , RseriesMin = RseriesMin
  , RseriesMax = RseriesMax
  , filterByRinput = filterByRinput
  , RinputMin = RinputMin
  , RinputMax = RinputMax
  , filterByHoldingCurr = filterByHoldingCurr
  , holdingCurrMin =holdingCurrMin
  , holdingCurrMax = holdingCurrMax
  , filterByCapacitance = filterByCapacitance
  , capacitanceMin = capacitanceMin
  , capacitanceMax = capacitanceMax
  , filterByNumCells = filterByNumCells
  , maxCellNum = maxCellNum
  , filterBySacHr = filterBySacHr
  , maxSacHr =  maxSacHr# time since sacrifice
  , filterByRecHr = filterByRecHr
  , recHrMin =  recHrMin# time since lights on
  , recHrMax = recHrMax
  , removeNoToInclude = removeNoToInclude
)

GABApscsFilteredProps <- GABApscsFiltered %>%
  filterEphys()

filterByFreq <- TRUE

filterByFrequency <- function(df, doFilterByFreq = filterByFreq){
  if(doFilterByFreq){
    df <- df %>%
      filter(
        !is.na(frequency)
      )
  }
  return(df)
}

GABApscsFilteredFiring <- GABApscsFilteredProps %>%
  filterByFrequency()


