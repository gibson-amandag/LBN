
# Figure 1 - Dams ---------------------------------------------------------


## Count of dams -----------------------------------------------------------
damBehavior_count <- damBehavior_byPND %>%
  filter(
    !is.na(Num_exits)
  ) %>%
  count(earlyLifeTrt, PND) %>%
  spread(key = PND, value = n)

damMass_count <- damMassFiltered %>%
  filter(
    !is.na(mass)
  ) %>%
  rename(
    PND = day
  ) %>%
  count(earlyLifeTrt, PND) %>%
  spread(key = PND, value = n)

damCort_count <- damFiltered %>%
  filter(
    !is.na(Cort_dam_P11)
  ) %>%
  mutate(
    PND = 11
  ) %>%
  count(earlyLifeTrt, PND) %>%
  spread(key = PND, value = n)

dam_count <-  bind_rows(
  list(
    "dam mass" = damMass_count
    , "dam corticosterone" = damCort_count
    , "dam behavior" = damBehavior_count
  )
  , .id = "variable"
) %>%
  select(
    variable
    , earlyLifeTrt
    , `4`
    , `5`
    , `6`
    , `7`
    , `8`
    , `9`
    , `10`
    , `11`
    , `21`
  )


dam_count_header <- data.frame(
  col_keys = c("variable", "earlyLifeTrt", "4", "5", "6", "7", "8", "9", "10", "11", "21")
  , line1 = c("", "", rep("# of dam measurements on postnatal day", 9))
  , line2 = c("variable", "treatment", "4", "5", "6", "7", "8", "9", "10", "11", "21")
  , stringsAsFactors = FALSE
)

dam_count_flexTable <- dam_count %>%
  makeManuscriptFlexTable(
    dam_count_header
    , horzLines = c(2, 4)
    , vertMergeCols = c(1)
  )


## Dam mass -----------------------------------------------------------

damMass_table <- damMass_lmm$anova_table %>%
  simplifyLMMOutput()

damMass_header <- data.frame(
  col_keys = c("variable", "F", "df", "p"),
  line1 = c("", rep("dam mass", 3)),
  line2 = c("variable", "F", "df", "p"),
  stringsAsFactors = FALSE
)

damMass_flexTable <- damMass_table %>%
  makeManuscriptFlexTable(
    damMass_header
    , fullWidth = FALSE
  )


### emmeans -----------------------------------------------------------

damMass_lmm_emmTbl.earlyLifeTrt <- damMass_lmm_EMM.earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput()

damMass_lmm_emmTbl.PND <- damMass_lmm_EMM.PND %>%
  as_tibble() %>%
  rename(
    level = PND
  ) %>%
  simplifyEMMOutput()


damMass_lmm_emmTbl <- bind_rows(
  list(
    `early-life treatment` = damMass_lmm_emmTbl.earlyLifeTrt
    , `PND` = damMass_lmm_emmTbl.PND
  )
  , .id = "variable"
)

damMass_lmm_emm_flexTable <- damMass_lmm_emmTbl %>%
  makeManuscriptFlexTable(
    horzLines = c(2)
    , round1Cols = c("df")
    , round2Cols = c("emmean")
    , round3Cols = c("SEM")
    , vertMergeCols = c("variable")
  )


### pairs -----------------------------------------------------------

damMass_emmPairsTbl.earlyLifeTrt <- damMass_lmm_EMM.earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput()

damMass_emmPairsTbl.PND <- damMass_lmm_EMM.PND.pairs %>%
  simplifyEMMPairsOutput()

damMass_emmPairsTbl <- bind_rows(
  list(
    `early-life treatment` = damMass_emmPairsTbl.earlyLifeTrt
    , `PND` = damMass_emmPairsTbl.PND
  )
  , .id = "variable"
)

damMass_emmPairs_flexTable <- damMass_emmPairsTbl %>%
  makeManuscriptFlexTable(
    horzLines = c(1)
    , vertMergeCols = c("variable")
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

## Dam cort -----------------------------------------------------------

damCortT <- damFiltered %>%
  t_test(
    Cort_dam_P11 ~ earlyLifeTrt
    , var.equal = TRUE
    , detailed = TRUE
  )

damCortT <- damCortT %>%
  select(
    `.y.`
    , group1
    , group2
    # , n1
    # , n2
    , statistic
    , df
    , p
    , estimate
    , conf.low
    , conf.high
  )

damCortCohensD <- damFiltered %>%
  cohens_d(
    Cort_dam_P11 ~ earlyLifeTrt
    , var.equal = TRUE
  )

damCort_tbl <- damCortT %>%
  left_join(
    damCortCohensD %>%
      select(
        `.y.`
        , effsize
      )
    , by = ".y."
  ) %>%
  mutate(
    comparison = paste0(group1, " - ", group2)
    , .after = `.y.`
  ) %>%
  mutate(
    `95% CI` = paste0("[", format(round(conf.low, 2), nsmall = 2, trim = TRUE), ", ", format(round(conf.high, 2), nsmall = 2, trim = TRUE), "]")
    , .after = estimate
  ) %>%
  rename(
    t = statistic
    , difference = estimate
    , `Cohen's d` = effsize 
  ) %>%
  select(
    -c(`.y.`, group1, group2, conf.low, conf.high)
  )

damCort_flexTable <- damCort_tbl %>%
  makeManuscriptFlexTable(
    round2Cols = c("t", "difference", "Cohen's d")
    , fullWidth = FALSE
  )



## Dam behavior -----------------------------------------------------------

numExits_table <- numExits_nparLD$ANOVA.test %>% 
  subTrtInRowNames() %>%
  subnParColNames() %>%
  as.data.frame() %>%
  rownames_to_column("variable")

percOffNest_table <- percOffNest_nparLD$ANOVA.test %>% 
  subTrtInRowNames() %>%
  subnParColNames() %>%
  as.data.frame() %>%
  rownames_to_column("variable")

damBehavior_header <- data.frame(
  col_keys = c("variable", "F_model_1", "df_model_1", "p_model_1",
               "F_model_2", "df_model_2", "p_model_2"
  ),
  # line1 = paste0("Table ", tableNum),
  line2 = c("", rep("# of exits", 3), rep("% off nest", 3)),
  line3 = c("variable", "F", "df", "p", "F", "df", "p"),
  stringsAsFactors = FALSE
)

damBehaviorTable <- bind_rows(
  list(model_1 = numExits_table, model_2 = percOffNest_table), .id = "model"
) %>%
  formatPCol() %>%
  pivot_wider(
    id_cols = variable, names_from = model, values_from = c("F", "df", "p")
  )

damBehavior_flexTable <- damBehaviorTable %>%
  makeManuscriptFlexTable(
    headerDF = damBehavior_header
    , round1Cols = c("df_model_1", "df_model_2")
    , round2Cols = c("F_model_1", "F_model_2")
    , vertLines = c(1, 4)
  )


# Figure 2 - Maturation ---------------------------------------------------

## Mass count table --------------
massCountTbl <- massFiltered %>%
  makeOffMassLong() %>%
  filter(!is.na(mass)) %>%
  group_by(sex, earlyLifeTrt, day) %>%
  summarise(
    mice = n(), 
    litters = n_distinct(damID),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(litters, mice), names_to = "Measure", values_to = "Value") %>%
  unite("earlyLifeTrt_Sex_Measure", sex, earlyLifeTrt, Measure, sep = "_") %>%
  pivot_wider(
    names_from = earlyLifeTrt_Sex_Measure,
    values_from = Value
  ) %>%
  filter(
    day %in% c(11, 21, 35, 70)
  )

massCount_header <- data.frame(
  col_keys = colnames(massCountTbl)
  , line1 = c("", rep("female offspring", 4), rep("male offspring", 4))
  , line2 = c("", rep(c(rep("STD", 2), rep("LBN", 2)), 2))
  , line3 = c("PND", rep(c("litters", "mice"), 4))
  , stringsAsFactors = FALSE
)

massCount_flexTable <- massCountTbl %>%
  makeManuscriptFlexTable(
    headerDF = massCount_header
    , vertLines = c(1, 3, 5, 7)
  )

## Offspring mass LMM ------------

femaleMass_tbl <- female_mass_lmm$anova_table %>%
  simplifyLMMOutput()

femaleMass_tbl$variable <- gsub("lspline\\(day, c\\(21, 35\\)\\)", "PND@", femaleMass_tbl$variable)

maleMass_tbl <- male_mass_lmm$anova_table %>%
  simplifyLMMOutput()

maleMass_tbl$variable <- gsub("lspline\\(day, c\\(21, 35\\)\\)", "PND@", maleMass_tbl$variable)

massLMM_header <- data.frame(
  col_keys = c("variable", "F_model_1", "df_model_1", "p_model_1"
               , "F_model_2", "df_model_2", "p_model_2"
  ),
  line2 = c("", rep("females", 3), rep("males", 3)),
  line3 = c("variable", "F", "df", "p"
            , "F", "df", "p"
  ),
  stringsAsFactors = FALSE
)

massLMM_tbl <- bind_rows(
  list(
    model_1 = femaleMass_tbl
    , model_2 = maleMass_tbl
  )
  , .id = "model"
) %>%
  pivot_wider(
    id_cols = variable, names_from = model, values_from = c("F", "df", "p")
  )

massLMM_flexTable <- massLMM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = massLMM_header
    , vertLines = c(1, 4)
    , fullWidth = FALSE
  )

### Male emmeans ----------

maleMass_emm_tbl <- male_mass_lmm_emm %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>% 
  simplifyEMMOutput() %>%
  pivot_wider(
    id_cols = day
    , names_from = level
    , values_from = c("emmean", "SEM", "df", "95% CI")
  )

maleMass_emm_header <- data.frame(
  col_keys = c("day", "emmean_STD", "SEM_STD", "df_STD", "95% CI_STD"
               , "emmean_LBN", "SEM_LBN", "df_LBN", "95% CI_LBN"
  ),
  line1 = c("", rep("STD", 4), rep("LBN", 4)),
  line2 = c("PND", rep(c("emmean", "SEM", "df", "95% CI"), 2)),
  stringsAsFactors = FALSE
)

maleMass_emm_flexTable <- maleMass_emm_tbl %>%
  makeManuscriptFlexTable(
    headerDF = maleMass_emm_header
    , round1Cols = c("df_STD", "df_LBN")
    , round2Cols = c("emmean_STD", "emmean_LBN")
    , round3Cols = c("SEM_STD", "SEM_LBN")
    , vertLines = c(1, 5)
  )

### pairs ----------

maleMass_emm.pairs_flexTable <- male_mass_lmm_emm.pairs %>%
  simplifyEMMPairsOutput() %>%
  rename(
    PND = day
  ) %>%
  makeManuscriptFlexTable(
    round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

## Maturation count -------------

ageCountTbl <- maturationByMouseLong %>%
  filter(!is.na(age)) %>%
  group_by(matType, earlyLifeTrt) %>%
  summarise(
    mice = n(), 
    litters = n_distinct(damID),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(litters, mice), names_to = "Measure", values_to = "Value") %>%
  unite("earlyLifeTrt_Measure", earlyLifeTrt, Measure, sep = "_") %>%
  pivot_wider(
    names_from = earlyLifeTrt_Measure,
    values_from = Value
  )

massMatCountTbl <- maturationByMouseLong %>%
  filter(!is.na(mass)) %>%
  group_by(matType, earlyLifeTrt) %>%
  summarise(
    mice = n(), 
    litters = n_distinct(damID),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(litters, mice), names_to = "Measure", values_to = "Value") %>%
  unite("earlyLifeTrt_Measure", earlyLifeTrt, Measure, sep = "_") %>%
  pivot_wider(
    names_from = earlyLifeTrt_Measure,
    values_from = Value
  )

matCountTbl <- bind_rows(
  list(
    "age" = ageCountTbl
    , "mass" = massMatCountTbl
  )
  , .id = "val"
) %>%
  pivot_wider(
    id_cols = matType
    , names_from = "val"
    , values_from = c("STD_litters", "STD_mice", "LBN_litters", "LBN_mice")
  )
  

matCount_header <- data.frame(
  col_keys = c(
    "matType"
    , "STD_litters_age"
    , "STD_mice_age"
    , "LBN_litters_age"
    , "LBN_mice_age"
    , "STD_litters_mass"
    , "STD_mice_mass"
    , "LBN_litters_mass"
    , "LBN_mice_mass"
  )
  , line1 = c("", rep("age", 4), rep("mass", 4))
  , line2 = c("", rep(c(rep("STD", 2), rep("LBN", 2)), 2))
  , line3 = c("", rep(c("litters", "mice"), 4))
  , stringsAsFactors = FALSE
)

matCount_flexTable <- matCountTbl %>%
  makeManuscriptFlexTable(
    headerDF = matCount_header
    , vertLines = c(1, 3, 5, 7)
  )

## Maturation LMM ------------------

VO_age_lmm_tbl <- VO_age_lmm$anova_table %>%
  simplifyLMMOutput()
Estrus_age_lmm_tbl <- Estrus_age_lmm$anova_table %>%
  simplifyLMMOutput()
PreputialSep_age_lmm_tbl <- PreputialSep_age_lmm$anova_table %>%
  simplifyLMMOutput()

VO_mass_lmm_tbl <- VO_mass_lmm$anova_table %>%
  simplifyLMMOutput()
Estrus_mass_lmm_tbl <- Estrus_mass_lmm$anova_table %>%
  simplifyLMMOutput()
PreputialSep_mass_lmm_tbl <- PreputialSep_mass_lmm$anova_table %>%
  simplifyLMMOutput()

ageTbl <- bind_rows(list(
  "vaginal opening" = VO_age_lmm_tbl
  ,"first estrus" = Estrus_age_lmm_tbl
  ,"preputial separation" = PreputialSep_age_lmm_tbl
)
, .id = "feature"
)

massTbl <- bind_rows(list(
  "vaginal opening" = VO_mass_lmm_tbl
  ,"first estrus" = Estrus_mass_lmm_tbl
  ,"preputial separation" = PreputialSep_mass_lmm_tbl
)
, .id = "feature"
)

matTbl <- bind_rows(
  list(
    "age" = ageTbl
    , "mass" = massTbl
  )
  , .id = "type"
) %>%
  pivot_wider(
    id_cols = feature
    , names_from = type
    , values_from = c("F", "df", "p")
  )

matMass_header <- data.frame(
  col_keys = c("feature", "F_age", "df_age", "p_age"
               , "F_mass", "df_mass", "p_mass"
  )
  , line1 = c("", rep("effect of early-life treatment", 6))
  , line2 = c("", rep("age", 3), rep("mass", 3))
  , line3 = c("", "F", "df", "p"
              , "F", "df", "p"
  )
  , stringsAsFactors = FALSE
)

matMass_lmm_flexTable <- matTbl %>%
  makeManuscriptFlexTable(
    headerDF = matMass_header
    , vertLines = c(1, 4)
    , fullWidth = FALSE
  )


## AGD --------------------------

### Count ---------

maturationFiltered %>%
  filter(
    !is.na(AGD_adult) 
  ) %>%
  group_by(
    earlyLifeTrt
    , sex
  ) %>%
  summarize(
    litters = n_distinct(damID),
    mice = n(), 
    .groups = "drop"
  )

### lmm -------

AGD_tbl <- AGD_lmm$anova_table %>%
  simplifyLMMOutput()

AGD_header <- data.frame(
  col_keys = c("variable", "F", "df", "p"
  ),
  line2 = c("", rep("anogenital distance", 3)),
  line3 = c("variable", "F", "df", "p"),
  stringsAsFactors = FALSE
)

AGD_flexTable <- AGD_tbl %>%
  makeManuscriptFlexTable(
    headerDF = AGD_header
    , fullWidth = FALSE
  )

### emmeans ------

AGD_lmm_emm_sex_tbl <- AGD_lmm_emm_sex %>%
  as_tibble() %>%
  mutate(
    sex = ifelse(sex == "F", "female", ifelse(
      sex == "M", "male", sex
    ))
  ) %>%
  rename(
    level = sex
  ) %>%
  simplifyEMMOutput()

AGD_lmm_emm_sex_flexTable <- AGD_lmm_emm_sex_tbl %>%
  makeManuscriptFlexTable(
    round1Cols = c("df")
    , round2Cols =  c("emmean")
    , round3Cols = c("SEM")
  )

### pairs -----------
AGD_lmm_emm_sex.pairs_flexTable <- AGD_lmm_emm_sex.pairs %>%
  as_tibble () %>%
  mutate(
    contrast = ifelse(contrast == "F - M", "female - male", contrast)
  ) %>%
  simplifyEMMPairsOutput() %>%
  makeManuscriptFlexTable(
    round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

# Figure 3 ---------------

numCycles_table <- numCycles_lmm$anova_table %>%
  simplifyLMMOutput()

cycleLength_table <- lengthCycles_log_lmm$anova_table %>%
  simplifyLMMOutput()

cyclesChiSq_table <- cycles_ChiSq %>%
  select(
    statistic
    , df
    , p
  ) %>%
  mutate(
    variable = "early-life treatment"
    , .before = "statistic"
  ) %>%
  rename(
    `F` = statistic
  ) %>%
  mutate(
    `F` = as.character(format(round(`F`, 2), nsmall = 2))
    , df = as.character(df)
    , p = as.character(p)
  )

cycles_table <- bind_rows(
  list(
    model_1 = numCycles_table
    , model_2 = cycleLength_table
    , model_3 = cyclesChiSq_table
  )
  , .id = "model"
)%>%
  pivot_wider(
    id_cols = variable, names_from = model, values_from = c("F", "df", "p")
  )

cycles_header <- data.frame(
  col_keys = c("variable", "F_model_1", "df_model_1", "p_model_1"
               , "F_model_2", "df_model_2", "p_model_2"
               , "F_model_3", "df_model_3", "p_model_3"
  ),
  line2 = c("", rep("# cycles", 3), rep("mean cycle length", 3), rep("Cycle stage distribution", 3)),
  line3 = c("variable", "F", "df", "p"
            , "F", "df", "p"
            , "Chi-sq", "df", "p"
  ),
  stringsAsFactors = FALSE
)

cycles_flexTable <- cycles_table %>%
  makeManuscriptFlexTable(
    headerDF = cycles_header
    , vertLines = c(1, 4, 7)
  )


# Figure 4 --------------------

## Counts -----------------

cortCount <- cortFiltered_M_DiPro %>%
  filter(
    !is.na(cort)
  ) %>%
  group_by(
    earlyLifeTrt, adultTrt, hormoneStatus, time
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(cols = c(litters, mice), names_to = "measure", values_to = "Value") %>%
  # unite("combined", hormoneStatus, time, measure, sep = "_") %>%
  unite("combined", earlyLifeTrt, adultTrt, measure, sep = "_") %>%
  pivot_wider(
    names_from = combined,
    values_from = Value
  ) %>%
  mutate(
    time = ifelse(time == 0, "pre", "post")
  )

cortCount_header <- data.frame(
  col_keys = colnames(cortCount)
  # , line1 = c("", "", rep("males", 4), rep("diestrous females", 4), rep("proestrous females", 4))
  # , line2 = c("", "", rep(c(rep("pre", 2), rep("post", 2)), 3))
  # , line3 = c("early-life treatment", "adult treatment", rep(c("litters", "mice"), 6))
  , line1 = c("", "", rep("STD", 4), rep("LBN", 4))
  , line2 = c("", "", rep(c(rep("CON", 2), rep("ALPS", 2)), 2))
  , line3 = c("", "time", rep(c("litters", "mice"), 4))
  , stringsAsFactors = FALSE
)

cortCount_flexTable <- cortCount %>%
  makeManuscriptFlexTable(
    headerDF = cortCount_header
    # , vertLines = c(2, 4, 6, 8, 10, 12)
    # , horzLines = c(2)
    # , vertMergeCols = c("earlyLifeTrt")
    , vertLines = c(2, 4, 6, 8)
    , horzLines = c(2, 4)
    , vertMergeCols = c("hormoneStatus")
  )

## lmm -------------

maleCortLMM_tbl <- male_cort_lmm$anova_table %>%
  simplifyLMMOutput()

femaleCortLMM_tbl <- female_cort_lmm$anova_table %>%
  simplifyLMMOutput()

cortLMM_tbl <- bind_rows(
  list(
    females = femaleCortLMM_tbl
    , males = maleCortLMM_tbl
  ), .id = "model"
)  %>%
  pivot_wider(
    id_cols = variable, names_from = model, values_from = c("F", "df", "p")
  )

cort_header <- data.frame(
  col_keys = c("variable", "F_males", "df_males", "p_males"
               , "F_females", "df_females", "p_females"
  )
  , line2 = c("", rep("males", 3), rep("females", 3))
  , line3 = c("variable", "F", "df", "p"
              , "F", "df", "p")
)

cort_flexTable <- cortLMM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = cort_header
    , vertLines = c(4)
  )


### emmeans - adult trt * time -------------

maleCortLMM_emm_tbl <- male_cort_lmm_ALPSTime_emm %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() 

femaleCortLMM_emm_adultTrtTime_tbl <- female_cort_lmm_emm_adultTrtTime %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() 

cortLMM_emm_adultTrtTime_tbl <- bind_rows(
  list(
    males = maleCortLMM_emm_tbl
    ,females = femaleCortLMM_emm_adultTrtTime_tbl
  ), .id = "sex"
)

cortLMM_emm_header <- data.frame(
  col_keys = c("sex", "time", "level" 
               , "response", "SEM", "df", "95% CI"
               )
  , line1 = c("sex", "time", "level",
              "emmean", "SEM", "df", "95% CI"
              )
)

cortLMM_emm_flexTable <- cortLMM_emm_adultTrtTime_tbl %>%
  makeManuscriptFlexTable(
    headerDF = cortLMM_emm_header
    , vertMergeCols = c("sex", "time")
    , horzLines = c(2, 4, 6)
    , round1Cols = c("df")
    , round2Cols = c("response")
    , round3Cols = c("SEM")
  )

## side by side instead of stacked
  # cortLMM_emm_adultTrtTime_tbl <- bind_rows(
  #   list(
  #     females = femaleCortLMM_emm_adultTrtTime_tbl
  #     , males = maleCortLMM_emm_tbl
  #   ), .id = "model"
  # ) %>%
  #   pivot_wider(
  #     id_cols = c("level", "time"), names_from = model, values_from = c("response", "SEM", "df", "95% CI")
  #   )
  # 
  # cortLMM_emm_header <- data.frame(
  #   col_keys = c("time", "level" 
  #                , "response_males", "SEM_males", "df_males", "95% CI_males"
  #                , "response_females", "SEM_females", "df_females", "95% CI_females"
  #                )
  #   , line1 = c("", "", rep("males", 4), rep("females", 4))
  #   , line2 = c("time", "level",
  #               rep(c(
  #                 "emmean", "SEM", "df", "95% CI"
  #               ), 2))
  # )
  # 
  # cortLMM_emm_flexTable <- cortLMM_emm_adultTrtTime_tbl %>%
  #   makeManuscriptFlexTable(
  #     headerDF = cortLMM_emm_header
  #     , vertLines = c(6)
  #     , vertMergeCols = c("time")
  #     , horzLines = c(2)
  #     , round1Cols = c("df_males", "df_females")
  #     , round2Cols = c("response_males", "response_females")
  #     , round3Cols = c("SEM_males", "SEM_females")
  #   )
##

#### pairs - adult trt * time ------------
maleCortLMM_emm.pairs_tbl <- male_cort_lmm_ALPSTime_emm.pairs %>%
  simplifyEMMPairsOutput()

femaleCortLMM_emm_adultTrtTime.pairs_tbl <- female_cort_lmm_emm_adultTrtTime.pairs %>%
  simplifyEMMPairsOutput()

cortLMM_emm_adultTrtTime.pairs_tbl <- bind_rows(
  list(
    males = maleCortLMM_emm.pairs_tbl
    , females = femaleCortLMM_emm_adultTrtTime.pairs_tbl
  ), .id = "sex"
)

cortLMM_emm.pairs_header <- data.frame(
  col_keys = c("sex", "time", "contrast"
               , "ratio", "SEM", "df", "null", "t ratio", "p"
  )
  , line1 = c("sex", "time", "contrast",
              "ratio", "SEM", "df", "null", "t ratio", "p"
              )
)

cortLMM_emm.pairs_flexTable <- cortLMM_emm_adultTrtTime.pairs_tbl %>%
  makeManuscriptFlexTable(
    headerDF = cortLMM_emm.pairs_header
    , horzLines = c(2)
    , vertMergeCols = c("sex")
    , round1Cols = c("df")
    , round2Cols = c("ratio", "t ratio")
    , round3Cols = c("SEM")
  )


## With male and female side by side instead of stacked
# cortLMM_emm_adultTrtTime.pairs_tbl <- bind_rows(
#   list(
#     males = maleCortLMM_emm.pairs_tbl
#     , females = femaleCortLMM_emm_adultTrtTime.pairs_tbl
#   ), .id = "model"
# ) %>%
#   pivot_wider(
#     id_cols = c("contrast", "time"), names_from = model, values_from = c("ratio", "SEM", "df", "null", "t ratio", "p")
#   )
# 
# cortLMM_emm.pairs_header <- data.frame(
#   col_keys = c("time", "contrast" 
#                , "ratio_males", "SEM_males", "df_males", "null_males", "t ratio_males", "p_males"
#                , "ratio_females", "SEM_females", "df_females", "null_females", "t ratio_females", "p_females"
#   )
#   , line1 = c("", "", rep("males", 6), rep("females", 6))
#   , line2 = c("time", "contrast",
#               rep(c(
#                 "ratio", "SEM", "df", "null", "t ratio", "p"
#               ), 2))
# )
# 
# cortLMM_emm.pairs_flexTable <- cortLMM_emm_adultTrtTime.pairs_tbl %>%
#   makeManuscriptFlexTable(
#     headerDF = cortLMM_emm.pairs_header
#     , vertLines = c(8)
#     , vertMergeCols = c("time")
#     , round1Cols = c("df_males", "df_females")
#     , round2Cols = c("ratio_males", "ratio_females")
#     , round3Cols = c("SEM_males", "SEM_females", "t ratio_males", "t ratio_females")
#   )
##

### cycle stage * time emmeans -------------

femaleCortLMM_emm_cycleTime_tbl <- female_cort_lmm_emm_Sac_cycleTime %>%
  as_tibble() %>%
  rename(
    level = Sac_cycle
  ) %>%
  simplifyEMMOutput() 

femaleCortLMM_emm_cycleTime_header <- data.frame(
  col_keys = c("time", "level" 
               , "response", "SEM", "df", "95% CI"
  )
  , line1 = c("time", "level",
              "emmean", "SEM", "df", "95% CI"
  )
)

femaleCortLMM_emm_cycleTime_flexTable <- femaleCortLMM_emm_cycleTime_tbl %>%
  makeManuscriptFlexTable(
    headerDF = femaleCortLMM_emm_cycleTime_header
    , vertMergeCols = c("time")
    , horzLines = c(2)
    , round1Cols = c("df")
    , round2Cols = c("response")
    , round3Cols = c("SEM")
  )

### cycle stage * time pairs ----------

femaleCortLMM_emm_cycleTime.pairs_tbl <- female_cort_lmm_emm_Sac_cycleTime.pairs %>%
  simplifyEMMPairsOutput()

femaleCortLMM_emm_cycleTime.pairs_header <- data.frame(
  col_keys = c("time", "contrast"
               , "ratio", "SEM", "df", "null", "t ratio", "p"
  )
  , line1 = c("time", "contrast",
              "ratio", "SEM", "df", "null", "t ratio", "p"
  )
)

femaleCortLMM_emm_cycleTime.pairs_flexTable <- femaleCortLMM_emm_cycleTime.pairs_tbl %>%
  makeManuscriptFlexTable(
    headerDF = femaleCortLMM_emm_cycleTime.pairs_header
    # , horzLines = c(2)
    , round1Cols = c("df")
    , round2Cols = c("ratio", "t ratio")
    , round3Cols = c("SEM")
  )


# Figure 5 -------------------------------

## Counts ---------------

maleMassALPSPM_counts <- acuteStressFilteredMales %>%
  pivot_longer(
    cols = c(
      Body_mass_AM
      , percChangeBodyMass
      , Adrenal_mass
      , Adrenal_mass_perBody_g
      , ReproTract_mass
      , ReproTract_mass_perBody_g
      , Gonad_mass
      , Gonad_mass_perBody_g
      
    )
    , names_to = "feature"
    , values_to = "mass"
  ) %>%
  select(
    earlyLifeTrt
    , adultTrt
    , mouseID
    , damID
    , feature
    , mass
  ) %>%
  filter(
    !is.na(mass)
  ) %>%
  group_by(
    earlyLifeTrt
    , adultTrt
    , feature
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(litters, mice)
    , names_to = "measure"
    , values_to = "value"
  ) %>%
  unite(
    "combined"
    , earlyLifeTrt, adultTrt, measure, sep = "_"
  ) %>%
  pivot_wider(
    names_from = combined
    , values_from = value
  ) %>%
  mutate(
    feature = case_when(
      feature == "Body_mass_AM" ~ "AM body mass (g)"
      , feature == "percChangeBodyMass" ~ "% change body mass"
      , feature == "Adrenal_mass" ~ "adrenal mass (mg)"
      , feature == "Adrenal_mass_perBody_g" ~ "adrenal mass normalized to PM mass (mg/g)"
      , feature == "ReproTract_mass" ~ "seminal vesicle mass (mg)"
      , feature == "ReproTract_mass_perBody_g" ~ "seminal vesicle mass normalized to PM mass (mg/g)"
      , feature == "Gonad_mass" ~ "testicular mass (mg)"
      , feature == "Gonad_mass_perBody_g" ~ "testicular mass normalized to PM mass (mg/g)"
    )
  ) %>%
  mutate(
    feature = factor(
      feature
      , levels = c(
        "AM body mass (g)"
        ,"% change body mass"
        ,"adrenal mass (mg)"
        ,"adrenal mass normalized to PM mass (mg/g)"
        ,"seminal vesicle mass (mg)"
        ,"seminal vesicle mass normalized to PM mass (mg/g)"
        ,"testicular mass (mg)"
        ,"testicular mass normalized to PM mass (mg/g)"
      )
    )
  ) %>%
  arrange(
    feature
  )

maleMassALPSPM_counts_header <- data.frame(
  col_keys = colnames(maleMassALPSPM_counts)
  , line1 = c("", rep("STD", 4), rep("LBN", 4))
  , line2 = c("", rep(c(rep("CON", 2), rep("ALPS", 2)), 2))
  , line3 = c("feature", rep(c("litters", "mice"), 4))
  , stringsAsFactors = FALSE
)

maleMass_ALPSPM_counts_flexTable <- maleMassALPSPM_counts %>%
  makeManuscriptFlexTable(
    headerDF = maleMassALPSPM_counts_header
    # , vertLines = c(2, 4, 6, 8, 10, 12)
    # , horzLines = c(2)
    # , vertMergeCols = c("earlyLifeTrt")
    , vertLines = c(1, 3, 5, 7)
    , horzLines = c(2, 4, 6)
  )

### AM rel counts ----------
maleMassALPSAM_counts <- acuteStressFilteredMales %>%
  pivot_longer(
    cols = c(
      Body_mass_AM
      , percChangeBodyMass
      , Adrenal_mass
      , Adrenal_mass_perBodyAM_g
      , ReproTract_mass
      , ReproTract_mass_perBodyAM_g
      , Gonad_mass
      , Gonad_mass_perBodyAM_g
      
    )
    , names_to = "feature"
    , values_to = "mass"
  ) %>%
  select(
    earlyLifeTrt
    , adultTrt
    , mouseID
    , damID
    , feature
    , mass
  ) %>%
  filter(
    !is.na(mass)
  ) %>%
  group_by(
    earlyLifeTrt
    , adultTrt
    , feature
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(litters, mice)
    , names_to = "measure"
    , values_to = "value"
  ) %>%
  unite(
    "combined"
    , earlyLifeTrt, adultTrt, measure, sep = "_"
  ) %>%
  pivot_wider(
    names_from = combined
    , values_from = value
  ) %>%
  mutate(
    feature = case_when(
      feature == "Body_mass_AM" ~ "AM body mass (g)"
      , feature == "percChangeBodyMass" ~ "% change body mass"
      , feature == "Adrenal_mass" ~ "adrenal mass (mg)"
      , feature == "Adrenal_mass_perBodyAM_g" ~ "adrenal mass normalized to AM mass (mg/g)"
      , feature == "ReproTract_mass" ~ "seminal vesicle mass (mg)"
      , feature == "ReproTract_mass_perBodyAM_g" ~ "seminal vesicle mass normalized to AM mass (mg/g)"
      , feature == "Gonad_mass" ~ "testicular mass (mg)"
      , feature == "Gonad_mass_perBodyAM_g" ~ "testicular mass normalized to AM mass (mg/g)"
    )
  ) %>%
  mutate(
    feature = factor(
      feature
      , levels = c(
        "AM body mass (g)"
        ,"% change body mass"
        ,"adrenal mass (mg)"
        ,"adrenal mass normalized to AM mass (mg/g)"
        ,"seminal vesicle mass (mg)"
        ,"seminal vesicle mass normalized to AM mass (mg/g)"
        ,"testicular mass (mg)"
        ,"testicular mass normalized to AM mass (mg/g)"
      )
    )
  ) %>%
  arrange(
    feature
  )

maleMassALPSAM_counts_header <- data.frame(
  col_keys = colnames(maleMassALPSAM_counts)
  , line1 = c("", rep("STD", 4), rep("LBN", 4))
  , line2 = c("", rep(c(rep("CON", 2), rep("ALPS", 2)), 2))
  , line3 = c("feature", rep(c("litters", "mice"), 4))
  , stringsAsFactors = FALSE
)

maleMass_ALPSAM_counts_flexTable <- maleMassALPSAM_counts %>%
  makeManuscriptFlexTable(
    headerDF = maleMassALPSAM_counts_header
    , vertLines = c(1, 3, 5, 7)
    , horzLines = c(2, 4, 6)
  )

## LMM ----------------

maleBodyMassAM_ALPS_LMM_tbl <- maleBodyMassAM_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
malePercChangeBodyMass_ALPS_LMM_tbl <- malePercChangeBodyMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleAdrenalMass_ALPS_LMM_tbl <- maleAdrenalMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelAdrenalMass_ALPS_LMM_tbl <- maleRelAdrenalMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelAdrenalMassPM_ALPS_LMM_tbl <- maleRelAdrenalMassPM_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleSeminalVesicleMass_ALPS_LMM_tbl <- maleSeminalVesicleMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelSeminalVesicleMass_ALPS_LMM_tbl <- maleRelSeminalVesicleMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelSeminalVesicleMassPM_ALPS_LMM_tbl <- maleRelSeminalVesicleMassPM_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleTestesMass_ALPS_LMM_tbl <- maleTestesMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelTestesMass_ALPS_LMM_tbl <- maleRelTestesMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelTestesMassPM_ALPS_LMM_tbl <- maleRelTestesMassPM_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()

maleALPS_RelPM_tbl <- bind_rows(
  list(
    `AM body mass (g)` = maleBodyMassAM_ALPS_LMM_tbl
    , `% change body mass` = malePercChangeBodyMass_ALPS_LMM_tbl
    , `adrenal mass (mg)` = maleAdrenalMass_ALPS_LMM_tbl
    , `adrenal mass normalized to PM mass (mg/g)` = maleRelAdrenalMassPM_ALPS_LMM_tbl
    , `seminal vesicle mass (mg)` = maleSeminalVesicleMass_ALPS_LMM_tbl
    , `seminal vesicle mass normalized to PM mass (mg/g)` = maleRelSeminalVesicleMassPM_ALPS_LMM_tbl
    , `testicular mass (mg)` = maleTestesMass_ALPS_LMM_tbl
    , `testicular mass normalized to PM mass (mg/g)` = maleRelTestesMassPM_ALPS_LMM_tbl
  ), .id = "feature"
) %>%
  mutate(
    variable = ifelse(
      variable == "early-life treatment"
      , "earlyLife"
      , ifelse(
        variable == "adult treatment"
        , "adult"
        , "int"
      )
    )
  ) %>%
  pivot_wider(
    id_cols = "feature"
    , names_from = "variable"
    , values_from = c("F", "df", "p")
  )

maleALPS_RelAM_tbl <- bind_rows(
  list(
    `AM body mass (g)` = maleBodyMassAM_ALPS_LMM_tbl
    , `% change body mass` = malePercChangeBodyMass_ALPS_LMM_tbl
    , `adrenal mass (mg)` = maleAdrenalMass_ALPS_LMM_tbl
    , `adrenal mass normalized to AM mass (mg/g)` = maleRelAdrenalMass_ALPS_LMM_tbl
    , `seminal vesicle mass (mg)` = maleSeminalVesicleMass_ALPS_LMM_tbl
    , `seminal vesicle mass normalized to AM mass (mg/g)` = maleRelSeminalVesicleMass_ALPS_LMM_tbl
    , `testicular mass (mg)` = maleTestesMass_ALPS_LMM_tbl
    , `testicular mass normalized to AM mass (mg/g)` = maleRelTestesMass_ALPS_LMM_tbl
  ), .id = "feature"
) %>%
  mutate(
    variable = ifelse(
      variable == "early-life treatment"
      , "earlyLife"
      , ifelse(
        variable == "adult treatment"
        , "adult"
        , "int"
      )
    )
  ) %>%
  pivot_wider(
    id_cols = "feature"
    , names_from = "variable"
    , values_from = c("F", "df", "p")
  )

maleALPS_header <- data.frame(
  col_keys = c("feature"
               , "F_earlyLife", "df_earlyLife", "p_earlyLife"
               , "F_adult", "df_adult", "p_adult"
               , "F_int", "df_int", "p_int"
              )
  , line1 = c(""
              , rep("early-life treatment", 3)
              , rep("adult treatment", 3)
              , rep("early-life treatment * adult treatment", 3)
              )
  , line2 = c("feature"
              , rep(c("F", "df", "p"), 3)
              )
)

maleALPS_RelAM_flexTable <- maleALPS_RelAM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = maleALPS_header
    , round2Cols = c("F_earlyLife", "F_adult", "F_int")
    , vertLines = c(1, 4, 7)
  )

maleALPS_RelPM_flexTable <- maleALPS_RelPM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = maleALPS_header
    , round2Cols = c("F_earlyLife", "F_adult", "F_int")
    , vertLines = c(1, 4, 7)
  )

# if want just one linear long-form table, not wide-form
# maleALPS_RelAM_flexTable <- maleALPS_RelAM_tbl %>%
#   makeManuscriptFlexTable(
#     horzLines = seq(3, 3*8, 3)
#     , vertMergeCols = c("feature")
#     , round2Cols = c("F")
#   )
# 
# maleALPS_RelPM_flexTable <- maleALPS_RelPM_tbl %>%
#   makeManuscriptFlexTable(
#     horzLines = seq(3, 3*8, 3)
#     , vertMergeCols = c("feature")
#     , round2Cols = c("F")
#   )

## EMMs ----------------

maleBodyMassAM_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleBodyMassAM_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "AM body mass (g)"
  )

maleBodyMassAM_ALPS_LMM_EMM_adultTrt_tbl <- maleBodyMassAM_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "AM body mass (g)"
  )

malePercChangeBodyMass_ALPS_LMM_EMM_earlyLifeTrt_tbl <- malePercChangeBodyMass_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "% change body mass"
  )

malePercChangeBodyMass_ALPS_LMM_EMM_adultTrt_tbl <- malePercChangeBodyMass_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "% change body mass"
  )

malePercChangeBodyMass_ALPS_LMM_EMM_comboTrt_tbl <- malePercChangeBodyMass_ALPS_lmm_emm_comboTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife * adult"
    , feature = "% change body mass"
  )

maleAdrenalMass_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "adrenal mass (mg)"
  )

maleAdrenalMass_ALPS_LMM_EMM_adultTrt_tbl <- maleAdrenalMass_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "adrenal mass (mg)"
  )

maleRelAdrenalMass_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleRelAdrenalMass_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "adrenal mass normalized to AM mass (mg/g)"
  )

maleRelAdrenalMass_ALPS_LMM_EMM_adultTrt_tbl <- maleRelAdrenalMass_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "adrenal mass normalized to AM mass (mg/g)"
  )

maleRelAdrenalMassPM_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleRelAdrenalMassPM_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "adrenal mass normalized to PM mass (mg/g)"
  )

maleRelAdrenalMassPM_ALPS_LMM_EMM_adultTrt_tbl <- maleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "adrenal mass normalized to PM mass (mg/g)"
  )

maleSeminalVesicleMass_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "seminal vesicle mass (mg)"
  )

maleSeminalVesicleMass_ALPS_LMM_EMM_adultTrt_tbl <- maleSeminalVesicleMass_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "seminal vesicle mass (mg)"
  )

maleRelSeminalVesicleMass_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleRelSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "seminal vesicle mass normalized to AM mass (mg/g)"
  )

maleRelSeminalVesicleMass_ALPS_LMM_EMM_adultTrt_tbl <- maleRelSeminalVesicleMass_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "seminal vesicle mass normalized to AM mass (mg/g)"
  )

maleRelSeminalVesicleMassPM_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleRelSeminalVesicleMassPM_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "seminal vesicle mass normalized to PM mass (mg/g)"
  )

maleRelSeminalVesicleMassPM_ALPS_LMM_EMM_adultTrt_tbl <- maleRelSeminalVesicleMassPM_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "seminal vesicle mass normalized to PM mass (mg/g)"
  )

maleTestesMass_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleTestesMass_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "testicular mass (mg)"
  )

maleTestesMass_ALPS_LMM_EMM_adultTrt_tbl <- maleTestesMass_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "testicular mass (mg)"
  )

maleRelTestesMass_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleRelTestesMass_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "testicular mass normalized to AM mass (mg/g)"
  )

maleRelTestesMass_ALPS_LMM_EMM_adultTrt_tbl <- maleRelTestesMass_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "testicular mass normalized to AM mass (mg/g)"
  )

maleRelTestesMassPM_ALPS_LMM_EMM_earlyLifeTrt_tbl <- maleRelTestesMassPM_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  rename(
    level = earlyLifeTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "earlyLife"
    , feature = "testicular mass normalized to PM mass (mg/g)"
  )

maleRelTestesMassPM_ALPS_LMM_EMM_adultTrt_tbl <- maleRelTestesMassPM_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  rename(
    level = adultTrt
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "adult"
    , feature = "testicular mass normalized to PM mass (mg/g)"
  )

maleALPS_RelAM_EMM_tbl <- bind_rows(
  list(
    a = maleBodyMassAM_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , b = maleBodyMassAM_ALPS_LMM_EMM_adultTrt_tbl
    # , c = malePercChangeBodyMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , d = malePercChangeBodyMass_ALPS_LMM_EMM_adultTrt_tbl
    # , e = maleAdrenalMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    # , f = maleAdrenalMass_ALPS_LMM_EMM_adultTrt_tbl
    , g = maleRelAdrenalMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , h = maleRelAdrenalMass_ALPS_LMM_EMM_adultTrt_tbl
    # , i = maleSeminalVesicleMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , j = maleSeminalVesicleMass_ALPS_LMM_EMM_adultTrt_tbl
    , k = maleRelSeminalVesicleMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    # , l = maleRelSeminalVesicleMass_ALPS_LMM_EMM_adultTrt_tbl
    , m = maleTestesMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , n = maleTestesMass_ALPS_LMM_EMM_adultTrt_tbl
    # , q = maleRelTestesMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , p = maleRelTestesMass_ALPS_LMM_EMM_adultTrt_tbl
  )
) %>%
  select(
    feature, variable, level, everything()
  ) %>%
  mutate(
    comboLevel = ifelse(
      level %in% c("STD", "CON"),
      "STD-CON"
      , "LBN-ALPS"
    )
  ) %>%
  pivot_wider(
    values_from = c(
      level
      , emmean
      , SEM
      , df
      , `95% CI`
    )
    , names_from = variable
    , id_cols = c(feature, comboLevel)
  ) %>%
  select(
    -comboLevel
  )

maleALPS_RelPM_EMM_tbl <- bind_rows(
  list(
    a = maleBodyMassAM_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , b = maleBodyMassAM_ALPS_LMM_EMM_adultTrt_tbl
    # , c = malePercChangeBodyMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , d = malePercChangeBodyMass_ALPS_LMM_EMM_adultTrt_tbl
    # , e = maleAdrenalMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    # , f = maleAdrenalMass_ALPS_LMM_EMM_adultTrt_tbl
    , g = maleRelAdrenalMassPM_ALPS_LMM_EMM_earlyLifeTrt_tbl
    # , h = maleRelAdrenalMassPM_ALPS_LMM_EMM_adultTrt_tbl
    # , i = maleSeminalVesicleMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , j = maleSeminalVesicleMass_ALPS_LMM_EMM_adultTrt_tbl
    , k = maleRelSeminalVesicleMassPM_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , l = maleRelSeminalVesicleMassPM_ALPS_LMM_EMM_adultTrt_tbl
    , m = maleTestesMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , n = maleTestesMass_ALPS_LMM_EMM_adultTrt_tbl
    # , q = maleRelTestesMassPM_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , p = maleRelTestesMassPM_ALPS_LMM_EMM_adultTrt_tbl
  )
) %>%
  select(
    feature, variable, level, everything()
  ) %>%
  mutate(
    comboLevel = ifelse(
      level %in% c("STD", "CON"),
      "STD-CON"
      , "LBN-ALPS"
    )
  ) %>%
  pivot_wider(
    values_from = c(
      level
      , emmean
      , SEM
      , df
      , `95% CI`
    )
    , names_from = variable
    , id_cols = c(feature, comboLevel)
  ) %>%
  select(
    -comboLevel
  )

maleALPS_EMM_header <- data.frame(
  col_keys = c("feature"
               , "level_earlyLife", "emmean_earlyLife", "SEM_earlyLife", "df_earlyLife", "95% CI_earlyLife"
               , "level_adult", "emmean_adult", "SEM_adult", "df_adult", "95% CI_adult"
              )
  , line1 = c(""
              , rep("early-life treatment", 5)
              , rep("adult treatment", 5)
    
  )
  , line2 = c("feature"
              , rep(c("level", "emmean", "SEM", "df", "95% CI"), 2)
            )
)

maleALPS_RelAM_EMM_flexTable <- maleALPS_RelAM_EMM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = maleALPS_EMM_header
    , vertMergeCols = c("feature")
    , vertLines = c(1, 6)
    , horzLines = c(2, 4, 6, 8, 10, 12, 14)
    , round1Cols = c("df_earlyLife", "df_adult")
    , round2Cols = c("emmean_earlyLife", "emmean_adult")
    , round3Cols = c("SEM_earlyLife", "SEM_adult")
  ) %>%
  padding(
    part = "all"
    , padding.left = 3.6
    , padding.right = 3.6
  )

maleALPS_RelPM_EMM_flexTable <- maleALPS_RelPM_EMM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = maleALPS_EMM_header
    , vertMergeCols = c("feature")
    , vertLines = c(1, 6)
    , horzLines = c(2, 4, 6, 8, 10, 12, 14)
    , round1Cols = c("df_earlyLife", "df_adult")
    , round2Cols = c("emmean_earlyLife", "emmean_adult")
    , round3Cols = c("SEM_earlyLife", "SEM_adult")
  ) %>%
  padding(
    part = "all"
    , padding.left = 3.6
    , padding.right = 3.6
  )

## Pairs ----------------

maleBodyMassAM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleBodyMassAM_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "AM body mass (g)"
  )

maleBodyMassAM_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleBodyMassAM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "AM body mass (g)"
  )

malePercChangeBodyMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- malePercChangeBodyMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "% change body mass"
  )

malePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl <- malePercChangeBodyMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "% change body mass"
  )

# malePercChangeBodyMass_ALPS_LMM_EMM.pairs_comboTrt_tbl <- malePercChangeBodyMass_ALPS_lmm_emm_comboTrt %>%
#   as_tibble() %>%
#   simplifyEMMOutput() %>%
#   mutate(
#     variable = "earlyLife * adult"
#     , feature = "% change body mass"
#   )

maleAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "adrenal mass (mg)"
  )

maleAdrenalMass_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleAdrenalMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "adrenal mass (mg)"
  )

maleRelAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleRelAdrenalMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "adrenal mass normalized to AM mass (mg/g)"
  )

maleRelAdrenalMass_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleRelAdrenalMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "adrenal mass normalized to AM mass (mg/g)"
  )

maleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleRelAdrenalMassPM_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "adrenal mass normalized to PM mass (mg/g)"
  )

maleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "adrenal mass normalized to PM mass (mg/g)"
  )

maleSeminalVesicleMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "seminal vesicle mass (mg)"
  )

maleSeminalVesicleMass_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleSeminalVesicleMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "seminal vesicle mass (mg)"
  )

maleRelSeminalVesicleMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleRelSeminalVesicleMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "seminal vesicle mass normalized to AM mass (mg/g)"
  )

maleRelSeminalVesicleMass_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleRelSeminalVesicleMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "seminal vesicle mass normalized to AM mass (mg/g)"
  )

maleRelSeminalVesicleMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleRelSeminalVesicleMassPM_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "seminal vesicle mass normalized to PM mass (mg/g)"
  )

maleRelSeminalVesicleMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleRelSeminalVesicleMassPM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "seminal vesicle mass normalized to PM mass (mg/g)"
  )

maleTestesMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleTestesMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "testicular mass (mg)"
  )

maleTestesMass_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleTestesMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "testicular mass (mg)"
  )

maleRelTestesMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleRelTestesMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "testicular mass normalized to AM mass (mg/g)"
  )

maleRelTestesMass_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleRelTestesMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "testicular mass normalized to AM mass (mg/g)"
  )

maleRelTestesMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- maleRelTestesMassPM_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "earlyLife"
    feature = "testicular mass normalized to PM mass (mg/g)"
  )

maleRelTestesMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl <- maleRelTestesMassPM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    # variable = "adultTrt"
    feature = "testicular mass normalized to PM mass (mg/g)"
  )

maleALPS_RelAM_EMM.pairs_tbl <- bind_rows(
  list(
    a = maleBodyMassAM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , b = maleBodyMassAM_ALPS_LMM_EMM.pairs_adultTrt_tbl
    # , c = malePercChangeBodyMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , d = malePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    # , e = maleAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    # , f = maleAdrenalMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , g = maleRelAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , h = maleRelAdrenalMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    # , i = maleSeminalVesicleMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , j = maleSeminalVesicleMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , k = maleRelSeminalVesicleMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    # , l = maleRelSeminalVesicleMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , m = maleTestesMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , n = maleTestesMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    # , q = maleRelTestesMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , p = maleRelTestesMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
  )
) %>%
  select(
    feature, contrast, everything()
  )

maleALPS_RelPM_EMM.pairs_tbl <- bind_rows(
  list(
    a = maleBodyMassAM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , b = maleBodyMassAM_ALPS_LMM_EMM.pairs_adultTrt_tbl
    # , c = malePercChangeBodyMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , d = malePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    # , e = maleAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    # , f = maleAdrenalMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , g = maleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    # , h = maleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl
    # , i = maleSeminalVesicleMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , j = maleSeminalVesicleMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , k = maleRelSeminalVesicleMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , l = maleRelSeminalVesicleMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , m = maleTestesMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , n = maleTestesMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    # , q = maleRelTestesMassPM_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , p = maleRelTestesMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl
  )
) %>%
  select(
    feature, contrast, everything()
  )

# maleALPS_EMM.pairs_header <- data.frame(
#   col_keys = c("feature"
#                , "level_earlyLife", "emmean_earlyLife", "SEM_earlyLife", "df_earlyLife", "95% CI_earlyLife"
#                , "level_adult", "emmean_adult", "SEM_adult", "df_adult", "95% CI_adult"
#               )
#   , line1 = c(""
#               , rep("early-life treatment", 5)
#               , rep("adult treatment", 5)
#     
#   )
#   , line2 = c("feature"
#               , rep(c("level", "emmean", "SEM", "df", "95% CI"), 2)
#             )
# )

maleALPS_RelAM_EMM.pairs_flexTable <- maleALPS_RelAM_EMM.pairs_tbl %>%
  makeManuscriptFlexTable(
    vertMergeCols = c("feature")
    , horzLines = c(2, 3, 5, 6, 7, 9)
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

maleALPS_RelPM_EMM.pairs_flexTable <- maleALPS_RelPM_EMM.pairs_tbl %>%
  makeManuscriptFlexTable(
    vertMergeCols = c("feature")
    , horzLines = c(2, 3, 4, 5, 7, 9)
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

# Figure 6 ----------------

## Counts cort vals -----------------
malesCortAdmin_cortCount <- maleCortAdmin_cort_filtered %>%
  filter(
    !is.na(cort)
  ) %>%
  group_by(
    dosage, time
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(cols = c(litters, mice), names_to = "measure", values_to = "Value") %>%
  unite("combined", dosage, measure, sep = "_") %>%
  pivot_wider(
    names_from = combined,
    values_from = Value
  ) %>%
  mutate(
    time = paste0(time,"hr")
  )

malesCortAdmin_cortCount_header <- data.frame(
  col_keys = colnames(malesCortAdmin_cortCount)
  , line1 = c("", rep("0mg/kg", 2), rep("2mg/kg", 2))
  , line2 = c("time", rep(c("litters", "mice"), 2))
  , stringsAsFactors = FALSE
)

malesCortAdmin_cortCount_flexTable <- malesCortAdmin_cortCount %>%
  makeManuscriptFlexTable(
    headerDF = malesCortAdmin_cortCount_header
    , vertLines = c(1, 3)
    , fullWidth = FALSE
  )

## LMM - cort vals ----------------

maleCortAdmin_cortLMM_tbl <- maleCortAdmin_cort_lmm$anova_table %>%
  simplifyLMMOutput()

# maleCortAdmin_cort_header <- data.frame(
#   col_keys = c("variable", "F", "df", "p")
#   , line1 = c("variable", "F", "df", "p")
# )

maleCortAdmin_cort_flexTable <- maleCortAdmin_cortLMM_tbl %>%
  makeManuscriptFlexTable()

### EMM - cort vals ------------

maleCortAdmin_cortLMM_emm_tbl <- maleCortAdmin_cort_lmm_emm %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  pivot_wider(
    id_cols = time
    , names_from = level
    , values_from = c(response, SEM, df, `95% CI`)
  ) %>%
  mutate(
    time = paste0(time, "hr")
  )

maleCortAdmin_cortLMM_emm_header <- data.frame(
  col_keys = c("time" 
               , "response_0", "SEM_0", "df_0", "95% CI_0"
               , "response_2", "SEM_2", "df_2", "95% CI_2"
  )
  , line1 = c(""
              , rep("0mg/kg", 4)
              , rep("2mg/kg", 4)
  )
  , line2 = c("time"
              , rep(c("emmean", "SEM", "df", "95% CI"), 2)
  )
)

maleCortAdmin_cortLMM_emm_flexTable <- maleCortAdmin_cortLMM_emm_tbl %>%
  makeManuscriptFlexTable(
    headerDF = maleCortAdmin_cortLMM_emm_header
    # , horzLines = c(2, 4, 6)
    , round1Cols = c("df_0", "df_2")
    , round2Cols = c("response_0", "response_2")
    , round3Cols = c("SEM_0", "SEM_2")
    , vertLines = c(1, 5)
  )

### Pairs - cort vals ------------

maleCortAdmin_cortLMM_emm.pairs_tbl <- maleCortAdmin_cort_lmm_emm.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    contrast = ifelse(
      contrast == "dosage0 / dosage2"
      , "0mg/kg / 2mg/kg"
      , ""
    )
    , time = paste0(time, "hr")
  )

maleCortAdmin_cortLMM_emm.pairs_header <- data.frame(
  col_keys = c("time", "contrast"
               , "ratio", "SEM", "df", "null", "t ratio", "p"
  )
  , line1 = c("time", "contrast",
              "ratio", "SEM", "df", "null", "t ratio", "p"
  )
)

maleCortAdmin_cortLMM_emm.pairs_flexTable <- maleCortAdmin_cortLMM_emm.pairs_tbl %>%
  makeManuscriptFlexTable(
    headerDF = maleCortAdmin_cortLMM_emm.pairs_header
    , round1Cols = c("df")
    , round2Cols = c("ratio", "t ratio")
    , round3Cols = c("SEM")
  )



## Counts tissue masses --------------

maleMassCortPM_counts <- maleCortAdmin_filtered %>%
  pivot_longer(
    cols = c(
      Body_mass_AM
      , percChangeBodyMass
      , Adrenal_mass
      , Adrenal_mass_perBody_g
      , ReproTract_mass
      , ReproTract_mass_perBody_g
      , Gonad_mass
      , Gonad_mass_perBody_g
      
    )
    , names_to = "feature"
    , values_to = "mass"
  ) %>%
  select(
    dosage
    , mouseID
    , damID
    , feature
    , mass
  ) %>%
  filter(
    !is.na(mass)
  ) %>%
  group_by(
    dosage
    , feature
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(litters, mice)
    , names_to = "measure"
    , values_to = "value"
  ) %>%
  unite(
    "combined"
    , dosage, measure, sep = "_"
  ) %>%
  pivot_wider(
    names_from = combined
    , values_from = value
  ) %>%
  mutate(
    feature = case_when(
      feature == "Body_mass_AM" ~ "AM body mass (g)"
      , feature == "percChangeBodyMass" ~ "% change body mass"
      , feature == "Adrenal_mass" ~ "adrenal mass (mg)"
      , feature == "Adrenal_mass_perBody_g" ~ "adrenal mass normalized to PM mass (mg/g)"
      , feature == "ReproTract_mass" ~ "seminal vesicle mass (mg)"
      , feature == "ReproTract_mass_perBody_g" ~ "seminal vesicle mass normalized to PM mass (mg/g)"
      , feature == "Gonad_mass" ~ "testicular mass (mg)"
      , feature == "Gonad_mass_perBody_g" ~ "testicular mass normalized to PM mass (mg/g)"
    )
  ) %>%
  mutate(
    feature = factor(
      feature
      , levels = c(
        "AM body mass (g)"
        ,"% change body mass"
        ,"adrenal mass (mg)"
        ,"adrenal mass normalized to PM mass (mg/g)"
        ,"seminal vesicle mass (mg)"
        ,"seminal vesicle mass normalized to PM mass (mg/g)"
        ,"testicular mass (mg)"
        ,"testicular mass normalized to PM mass (mg/g)"
      )
    )
  ) %>%
  arrange(
    feature
  )

maleMassCortAM_counts <- maleCortAdmin_filtered %>%
  pivot_longer(
    cols = c(
      Body_mass_AM
      , percChangeBodyMass
      , Adrenal_mass
      , Adrenal_mass_perBodyAM_g
      , ReproTract_mass
      , ReproTract_mass_perBodyAM_g
      , Gonad_mass
      , Gonad_mass_perBodyAM_g
      
    )
    , names_to = "feature"
    , values_to = "mass"
  ) %>%
  select(
    dosage
    , mouseID
    , damID
    , feature
    , mass
  ) %>%
  filter(
    !is.na(mass)
  ) %>%
  group_by(
    dosage
    , feature
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(litters, mice)
    , names_to = "measure"
    , values_to = "value"
  ) %>%
  unite(
    "combined"
    , dosage, measure, sep = "_"
  ) %>%
  pivot_wider(
    names_from = combined
    , values_from = value
  ) %>%
  mutate(
    feature = case_when(
      feature == "Body_mass_AM" ~ "AM body mass (g)"
      , feature == "percChangeBodyMass" ~ "% change body mass"
      , feature == "Adrenal_mass" ~ "adrenal mass (mg)"
      , feature == "Adrenal_mass_perBodyAM_g" ~ "adrenal mass normalized to AM mass (mg/g)"
      , feature == "ReproTract_mass" ~ "seminal vesicle mass (mg)"
      , feature == "ReproTract_mass_perBodyAM_g" ~ "seminal vesicle mass normalized to AM mass (mg/g)"
      , feature == "Gonad_mass" ~ "testicular mass (mg)"
      , feature == "Gonad_mass_perBodyAM_g" ~ "testicular mass normalized to AM mass (mg/g)"
    )
  ) %>%
  mutate(
    feature = factor(
      feature
      , levels = c(
        "AM body mass (g)"
        ,"% change body mass"
        ,"adrenal mass (mg)"
        ,"adrenal mass normalized to AM mass (mg/g)"
        ,"seminal vesicle mass (mg)"
        ,"seminal vesicle mass normalized to AM mass (mg/g)"
        ,"testicular mass (mg)"
        ,"testicular mass normalized to AM mass (mg/g)"
      )
    )
  ) %>%
  arrange(
    feature
  )


maleMassCortPM_counts_header <- data.frame(
  col_keys = colnames(maleMassCortPM_counts)
  , line1 = c("", rep("0mg/kg", 2), rep("2mg/kg", 2))
  , line3 = c("feature", rep(c("litters", "mice"), 2))
  , stringsAsFactors = FALSE
)

maleMassCortPM_counts_flexTable <- maleMassCortPM_counts %>%
  makeManuscriptFlexTable(
    headerDF = maleMassCortPM_counts_header
    , vertLines = c(1, 3)
    , horzLines = c(2, 4, 6)
  )
maleMassCortAM_counts_flexTable <- maleMassCortAM_counts %>%
  makeManuscriptFlexTable(
    headerDF = maleMassCortPM_counts_header
    , vertLines = c(1, 3)
    , horzLines = c(2, 4, 6)
  )

## LMMs tissue masses -------------

maleBodyMassAM_dosage_LMM_tbl <- maleBodyMassAM_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
malePercChangeBodyMass_dosage_LMM_tbl <- malePercChangeBodyMass_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleAdrenalMass_dosage_LMM_tbl <- maleAdrenalMass_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelAdrenalMass_dosage_LMM_tbl <- maleRelAdrenalMass_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelAdrenalMassPM_dosage_LMM_tbl <- maleRelAdrenalMassPM_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleSeminalVesicleMass_dosage_LMM_tbl <- maleSeminalVesicleMass_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelSeminalVesicleMass_dosage_LMM_tbl <- maleRelSeminalVesicleMass_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelSeminalVesicleMassPM_dosage_LMM_tbl <- maleRelSeminalVesicleMassPM_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleTestesMass_dosage_LMM_tbl <- maleTestesMass_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelTestesMass_dosage_LMM_tbl <- maleRelTestesMass_dosage_lmm$anova_table %>%
  simplifyLMMOutput()
maleRelTestesMassPM_dosage_LMM_tbl <- maleRelTestesMassPM_dosage_lmm$anova_table %>%
  simplifyLMMOutput()

male_dosage_RelPM_tbl <- bind_rows(
  list(
    `AM body mass (g)` = maleBodyMassAM_dosage_LMM_tbl
    , `% change body mass` = malePercChangeBodyMass_dosage_LMM_tbl
    , `adrenal mass (mg)` = maleAdrenalMass_dosage_LMM_tbl
    , `adrenal mass normalized to PM mass (mg/g)` = maleRelAdrenalMassPM_dosage_LMM_tbl
    , `seminal vesicle mass (mg)` = maleSeminalVesicleMass_dosage_LMM_tbl
    , `seminal vesicle mass normalized to PM mass (mg/g)` = maleRelSeminalVesicleMassPM_dosage_LMM_tbl
    , `testicular mass (mg)` = maleTestesMass_dosage_LMM_tbl
    , `testicular mass normalized to PM mass (mg/g)` = maleRelTestesMassPM_dosage_LMM_tbl
  ), .id = "feature"
)

male_dosage_RelAM_tbl <- bind_rows(
  list(
    `AM body mass (g)` = maleBodyMassAM_dosage_LMM_tbl
    , `% change body mass` = malePercChangeBodyMass_dosage_LMM_tbl
    , `adrenal mass (mg)` = maleAdrenalMass_dosage_LMM_tbl
    , `adrenal mass normalized to AM mass (mg/g)` = maleRelAdrenalMass_dosage_LMM_tbl
    , `seminal vesicle mass (mg)` = maleSeminalVesicleMass_dosage_LMM_tbl
    , `seminal vesicle mass normalized to AM mass (mg/g)` = maleRelSeminalVesicleMass_dosage_LMM_tbl
    , `testicular mass (mg)` = maleTestesMass_dosage_LMM_tbl
    , `testicular mass normalized to AM mass (mg/g)` = maleRelTestesMass_dosage_LMM_tbl
  ), .id = "feature"
)

male_dosage_header <- data.frame(
  col_keys = c("feature"
               , "F", "df", "p")
  , line1 = c(""
              , rep("dosage", 3)
  )
  , line2 = c("feature"
              , "F", "df", "p"
  )
)

male_dosage_RelAM_flexTable <- male_dosage_RelAM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = male_dosage_header
    , round2Cols = c("F")
    , vertLines = c(1)
    , horzLines = c(2, 4, 6)
  )

male_dosage_RelPM_flexTable <- male_dosage_RelPM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = male_dosage_header
    , round2Cols = c("F")
    , vertLines = c(1)
    , horzLines = c(2, 4, 6)
  )


### EMMs tissues -----------

maleBodyMassAM_dosage_LMM_EMM_dosage_tbl <- maleBodyMassAM_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "AM body mass (g)"
  )

malePercChangeBodyMass_dosage_LMM_EMM_dosage_tbl <- malePercChangeBodyMass_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "% change body mass"
  )

maleAdrenalMass_dosage_LMM_EMM_dosage_tbl <- maleAdrenalMass_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "adrenal mass (mg)"
  )

maleRelAdrenalMass_dosage_LMM_EMM_dosage_tbl <- maleRelAdrenalMass_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "adrenal mass normalized to AM mass (mg/g)"
  )

maleRelAdrenalMassPM_dosage_LMM_EMM_dosage_tbl <- maleRelAdrenalMassPM_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "adrenal mass normalized to PM mass (mg/g)"
  )


maleSeminalVesicleMass_dosage_LMM_EMM_dosage_tbl <- maleSeminalVesicleMass_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "seminal vesicle mass (mg)"
  )

maleRelSeminalVesicleMass_dosage_LMM_EMM_dosage_tbl <- maleRelSeminalVesicleMass_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "seminal vesicle mass normalized to AM mass (mg/g)"
  )

maleRelSeminalVesicleMassPM_dosage_LMM_EMM_dosage_tbl <- maleRelSeminalVesicleMassPM_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "seminal vesicle mass normalized to PM mass (mg/g)"
  )

maleTestesMass_dosage_LMM_EMM_dosage_tbl <- maleTestesMass_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "testicular mass (mg)"
  )

maleRelTestesMass_dosage_LMM_EMM_dosage_tbl <- maleRelTestesMass_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "testicular mass normalized to AM mass (mg/g)"
  )

maleRelTestesMassPM_dosage_LMM_EMM_dosage_tbl <- maleRelTestesMassPM_dosage_lmm_emm_dosage %>%
  as_tibble() %>%
  rename(
    level = dosage
  ) %>%
  simplifyEMMOutput() %>%
  mutate(
    variable = "dosage"
    , feature = "testicular mass normalized to PM mass (mg/g)"
  )

male_dosage_RelAM_EMM_tbl <- bind_rows(
  list(
    # a = maleBodyMassAM_dosage_LMM_EMM_dosage_tbl
    c = malePercChangeBodyMass_dosage_LMM_EMM_dosage_tbl
    # , e = maleAdrenalMass_dosage_LMM_EMM_dosage_tbl
    # , g = maleRelAdrenalMass_dosage_LMM_EMM_dosage_tbl
    # , i = maleSeminalVesicleMass_dosage_LMM_EMM_dosage_tbl
    # , k = maleRelSeminalVesicleMass_dosage_LMM_EMM_dosage_tbl
    , m = maleTestesMass_dosage_LMM_EMM_dosage_tbl
    , q = maleRelTestesMass_dosage_LMM_EMM_dosage_tbl
  )
) %>%
  select(
    feature, level, everything()
    , -variable
  ) %>%
  pivot_wider(
    values_from = c(
      emmean
      , SEM
      , df
      , `95% CI`
    )
    , names_from = level
    , id_cols = c(feature)
  )

male_dosage_RelPM_EMM_tbl <- bind_rows(
  list(
    # a = maleBodyMassAM_dosage_LMM_EMM_dosage_tbl
    c = malePercChangeBodyMass_dosage_LMM_EMM_dosage_tbl
    # , e = maleAdrenalMass_dosage_LMM_EMM_dosage_tbl
    # , g = maleRelAdrenalMassPM_dosage_LMM_EMM_dosage_tbl
    # , i = maleSeminalVesicleMass_dosage_LMM_EMM_dosage_tbl
    # , k = maleRelSeminalVesicleMassPM_dosage_LMM_EMM_dosage_tbl
    , m = maleTestesMass_dosage_LMM_EMM_dosage_tbl
    , q = maleRelTestesMassPM_dosage_LMM_EMM_dosage_tbl
  )
) %>%
  select(
    feature, level, everything()
    , -variable
  ) %>%
  pivot_wider(
    values_from = c(
      emmean
      , SEM
      , df
      , `95% CI`
    )
    , names_from = level
    , id_cols = c(feature)
  )

male_dosage_EMM_header <- data.frame(
  col_keys = c("feature"
               , "emmean_0", "SEM_0", "df_0", "95% CI_0"
               , "emmean_2", "SEM_2", "df_2", "95% CI_2"
  )
  , line1 = c(""
              , rep("0mg/kg", 4)
              , rep("2mg/kg", 4)
              
  )
  , line2 = c("feature"
              , rep(c("emmean", "SEM", "df", "95% CI"), 2)
  )
)

male_dosage_RelAM_EMM_flexTable <- male_dosage_RelAM_EMM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = male_dosage_EMM_header
    , vertMergeCols = c("feature")
    , vertLines = c(1, 5)
    # , horzLines = c(2, 4, 6)
    , round1Cols = c("df_0", "df_2")
    , round2Cols = c("emmean_0", "emmean_2")
    , round3Cols = c("SEM_0", "SEM_2")
  )

male_dosage_RelPM_EMM_flexTable <- male_dosage_RelPM_EMM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = male_dosage_EMM_header
    , vertMergeCols = c("feature")
    , vertLines = c(1, 5)
    , horzLines = c(1)
    , round1Cols = c("df_0", "df_2")
    , round2Cols = c("emmean_0", "emmean_2")
    , round3Cols = c("SEM_0", "SEM_2")
  )

### Pairs ------

maleBodyMassAM_dosage_LMM_EMM.pairs_dosage_tbl <- maleBodyMassAM_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "AM body mass (g)"
  )

malePercChangeBodyMass_dosage_LMM_EMM.pairs_dosage_tbl <- malePercChangeBodyMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "% change body mass"
  )

maleAdrenalMass_dosage_LMM_EMM.pairs_dosage_tbl <- maleAdrenalMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "adrenal mass (mg)"
  )

maleRelAdrenalMass_dosage_LMM_EMM.pairs_dosage_tbl <- maleRelAdrenalMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "adrenal mass normalized to AM mass (mg/g)"
  )

maleRelAdrenalMassPM_dosage_LMM_EMM.pairs_dosage_tbl <- maleRelAdrenalMassPM_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "adrenal mass normalized to PM mass (mg/g)"
  )

maleSeminalVesicleMass_dosage_LMM_EMM.pairs_dosage_tbl <- maleSeminalVesicleMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "seminal vesicle mass (mg)"
  )

maleRelSeminalVesicleMass_dosage_LMM_EMM.pairs_dosage_tbl <- maleRelSeminalVesicleMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "seminal vesicle mass normalized to AM mass (mg/g)"
  )

maleRelSeminalVesicleMassPM_dosage_LMM_EMM.pairs_dosage_tbl <- maleRelSeminalVesicleMassPM_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "seminal vesicle mass normalized to PM mass (mg/g)"
  )

maleTestesMass_dosage_LMM_EMM.pairs_dosage_tbl <- maleTestesMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "testicular mass (mg)"
  )

maleRelTestesMass_dosage_LMM_EMM.pairs_dosage_tbl <- maleRelTestesMass_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "testicular mass normalized to AM mass (mg/g)"
  )

maleRelTestesMassPM_dosage_LMM_EMM.pairs_dosage_tbl <- maleRelTestesMassPM_dosage_lmm_emm_dosage.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "testicular mass normalized to PM mass (mg/g)"
  )

male_dosage_RelAM_EMM.pairs_tbl <- bind_rows(
  list(
    # a = maleBodyMassAM_dosage_LMM_EMM.pairs_dosage_tbl
    c = malePercChangeBodyMass_dosage_LMM_EMM.pairs_dosage_tbl
    # , e = maleAdrenalMass_dosage_LMM_EMM.pairs_dosage_tbl
    # , g = maleRelAdrenalMass_dosage_LMM_EMM.pairs_dosage_tbl
    # , i = maleSeminalVesicleMass_dosage_LMM_EMM.pairs_dosage_tbl
    # , k = maleRelSeminalVesicleMass_dosage_LMM_EMM.pairs_dosage_tbl
    , m = maleTestesMass_dosage_LMM_EMM.pairs_dosage_tbl
    , q = maleRelTestesMass_dosage_LMM_EMM.pairs_dosage_tbl
  )
) %>%
  select(
    feature, contrast, everything()
  )

male_dosage_RelPM_EMM.pairs_tbl <- bind_rows(
  list(
    # a = maleBodyMassAM_dosage_LMM_EMM.pairs_dosage_tbl
    c = malePercChangeBodyMass_dosage_LMM_EMM.pairs_dosage_tbl
    # , e = maleAdrenalMass_dosage_LMM_EMM.pairs_dosage_tbl
    # , g = maleRelAdrenalMassPM_dosage_LMM_EMM.pairs_dosage_tbl
    # , i = maleSeminalVesicleMass_dosage_LMM_EMM.pairs_dosage_tbl
    # , k = maleRelSeminalVesicleMassPM_dosage_LMM_EMM.pairs_dosage_tbl
    , m = maleTestesMass_dosage_LMM_EMM.pairs_dosage_tbl
    , q = maleRelTestesMassPM_dosage_LMM_EMM.pairs_dosage_tbl
  )
) %>%
  select(
    feature, contrast, everything()
  )

# male_dosage_EMM.pairs_header <- data.frame(
#   col_keys = c("feature"
#                , "level_earlyLife", "emmean_earlyLife", "SEM_earlyLife", "df_earlyLife", "95% CI_earlyLife"
#                , "level_adult", "emmean_adult", "SEM_adult", "df_adult", "95% CI_adult"
#               )
#   , line1 = c(""
#               , rep("early-life treatment", 5)
#               , rep("adult treatment", 5)
#     
#   )
#   , line2 = c("feature"
#               , rep(c("level", "emmean", "SEM", "df", "95% CI"), 2)
#             )
# )

male_dosage_RelAM_EMM.pairs_flexTable <- male_dosage_RelAM_EMM.pairs_tbl %>%
  makeManuscriptFlexTable(
    vertMergeCols = c("feature")
    # , horzLines = c(2, 3, 5, 6, 7, 9)
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

male_dosage_RelPM_EMM.pairs_flexTable <- male_dosage_RelPM_EMM.pairs_tbl %>%
  makeManuscriptFlexTable(
    vertMergeCols = c("feature")
    , horzLines = c(1)
    , vertLines = c(1)
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

# Figure 7------------------

## Counts ---------------
femaleMassALPSPM_count_di <- acuteStressFilteredDi%>%
  pivot_longer(
    cols = c(
      Body_mass_AM
      , percChangeBodyMass
      , Adrenal_mass
      , Adrenal_mass_perBody_g
      , ReproTract_mass
      , ReproTract_mass_perBody_g
    )
    , names_to = "feature"
    , values_to = "mass"
  ) %>%
  select(
    earlyLifeTrt
    , adultTrt
    , mouseID
    , damID
    , feature
    , mass
  ) %>%
  filter(
    !is.na(mass)
  ) %>%
  group_by(
    earlyLifeTrt
    , adultTrt
    , feature
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(litters, mice)
    , names_to = "measure"
    , values_to = "value"
  ) %>%
  unite(
    "combined"
    , earlyLifeTrt, adultTrt, measure, sep = "_"
  ) %>%
  pivot_wider(
    names_from = combined
    , values_from = value
  ) %>%
  mutate(
    feature = case_when(
      feature == "Body_mass_AM" ~ "AM body mass (g)"
      , feature == "percChangeBodyMass" ~ "% change body mass"
      , feature == "Adrenal_mass" ~ "adrenal mass (mg)"
      , feature == "Adrenal_mass_perBody_g" ~ "adrenal mass normalized to PM mass (mg/g)"
      , feature == "ReproTract_mass" ~ "uterine mass (mg)"
      , feature == "ReproTract_mass_perBody_g" ~ "uterine mass normalized to PM mass (mg/g)"
    )
  ) %>%
  mutate(
    feature = factor(
      feature
      , levels = c(
        "AM body mass (g)"
        ,"% change body mass"
        ,"adrenal mass (mg)"
        ,"adrenal mass normalized to PM mass (mg/g)"
        ,"uterine mass (mg)"
        ,"uterine mass normalized to PM mass (mg/g)"
      )
    )
  ) %>%
  arrange(
    feature
  )
femaleMassALPSPM_count_pro <- acuteStressFilteredPro%>%
  pivot_longer(
    cols = c(
      Body_mass_AM
      , percChangeBodyMass
      , Adrenal_mass
      , Adrenal_mass_perBody_g
      , ReproTract_mass
      , ReproTract_mass_perBody_g
    )
    , names_to = "feature"
    , values_to = "mass"
  ) %>%
  select(
    earlyLifeTrt
    , adultTrt
    , mouseID
    , damID
    , feature
    , mass
  ) %>%
  filter(
    !is.na(mass)
  ) %>%
  group_by(
    earlyLifeTrt
    , adultTrt
    , feature
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(litters, mice)
    , names_to = "measure"
    , values_to = "value"
  ) %>%
  unite(
    "combined"
    , earlyLifeTrt, adultTrt, measure, sep = "_"
  ) %>%
  pivot_wider(
    names_from = combined
    , values_from = value
  ) %>%
  mutate(
    feature = case_when(
      feature == "Body_mass_AM" ~ "AM body mass (g)"
      , feature == "percChangeBodyMass" ~ "% change body mass"
      , feature == "Adrenal_mass" ~ "adrenal mass (mg)"
      , feature == "Adrenal_mass_perBody_g" ~ "adrenal mass normalized to PM mass (mg/g)"
      , feature == "ReproTract_mass" ~ "uterine mass (mg)"
      , feature == "ReproTract_mass_perBody_g" ~ "uterine mass normalized to PM mass (mg/g)"
    )
  ) %>%
  mutate(
    feature = factor(
      feature
      , levels = c(
        "AM body mass (g)"
        ,"% change body mass"
        ,"adrenal mass (mg)"
        ,"adrenal mass normalized to PM mass (mg/g)"
        ,"uterine mass (mg)"
        ,"uterine mass normalized to PM mass (mg/g)"
      )
    )
  ) %>%
  arrange(
    feature
  )

femaleMassALPSPM_counts <- bind_rows(
  list(
    "diestrus" = femaleMassALPSPM_count_di
    , "proestrus" = femaleMassALPSPM_count_pro
  )
  , .id = "cycle stage"
)

femaleMassALPSPM_counts_header <- data.frame(
  col_keys = colnames(femaleMassALPSPM_counts)
  , line1 = c("", "", rep("STD", 4), rep("LBN", 4))
  , line2 = c("", "", rep(c(rep("CON", 2), rep("ALPS", 2)), 2))
  , line3 = c("cycle stage", "feature", rep(c("litters", "mice"), 4))
  , stringsAsFactors = FALSE
)

femaleMass_ALPSPM_counts_flexTable <- femaleMassALPSPM_counts %>%
  makeManuscriptFlexTable(
    headerDF = femaleMassALPSPM_counts_header
    , vertMergeCols = c("cycle stage")
    , vertLines = c(2, 4, 6, 8)
    , horzLines = c(2, 4, 6, 8, 10, 12)
  )

### AM rel counts ----------
femaleMassALPSAM_count_di <- acuteStressFilteredDi%>%
  pivot_longer(
    cols = c(
      Body_mass_AM
      , percChangeBodyMass
      , Adrenal_mass
      , Adrenal_mass_perBodyAM_g
      , ReproTract_mass
      , ReproTract_mass_perBodyAM_g
    )
    , names_to = "feature"
    , values_to = "mass"
  ) %>%
  select(
    earlyLifeTrt
    , adultTrt
    , mouseID
    , damID
    , feature
    , mass
  ) %>%
  filter(
    !is.na(mass)
  ) %>%
  group_by(
    earlyLifeTrt
    , adultTrt
    , feature
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(litters, mice)
    , names_to = "measure"
    , values_to = "value"
  ) %>%
  unite(
    "combined"
    , earlyLifeTrt, adultTrt, measure, sep = "_"
  ) %>%
  pivot_wider(
    names_from = combined
    , values_from = value
  ) %>%
  mutate(
    feature = case_when(
      feature == "Body_mass_AM" ~ "AM body mass (g)"
      , feature == "percChangeBodyMass" ~ "% change body mass"
      , feature == "Adrenal_mass" ~ "adrenal mass (mg)"
      , feature == "Adrenal_mass_perBodyAM_g" ~ "adrenal mass normalized to AM mass (mg/g)"
      , feature == "ReproTract_mass" ~ "uterine mass (mg)"
      , feature == "ReproTract_mass_perBodyAM_g" ~ "uterine mass normalized to AM mass (mg/g)"
    )
  ) %>%
  mutate(
    feature = factor(
      feature
      , levels = c(
        "AM body mass (g)"
        ,"% change body mass"
        ,"adrenal mass (mg)"
        ,"adrenal mass normalized to AM mass (mg/g)"
        ,"uterine mass (mg)"
        ,"uterine mass normalized to AM mass (mg/g)"
      )
    )
  ) %>%
  arrange(
    feature
  )
femaleMassALPSAM_count_pro <- acuteStressFilteredPro%>%
  pivot_longer(
    cols = c(
      Body_mass_AM
      , percChangeBodyMass
      , Adrenal_mass
      , Adrenal_mass_perBodyAM_g
      , ReproTract_mass
      , ReproTract_mass_perBodyAM_g
    )
    , names_to = "feature"
    , values_to = "mass"
  ) %>%
  select(
    earlyLifeTrt
    , adultTrt
    , mouseID
    , damID
    , feature
    , mass
  ) %>%
  filter(
    !is.na(mass)
  ) %>%
  group_by(
    earlyLifeTrt
    , adultTrt
    , feature
  ) %>%
  summarize(
    mice = n()
    , litters = n_distinct(damID)
    , .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(litters, mice)
    , names_to = "measure"
    , values_to = "value"
  ) %>%
  unite(
    "combined"
    , earlyLifeTrt, adultTrt, measure, sep = "_"
  ) %>%
  pivot_wider(
    names_from = combined
    , values_from = value
  ) %>%
  mutate(
    feature = case_when(
      feature == "Body_mass_AM" ~ "AM body mass (g)"
      , feature == "percChangeBodyMass" ~ "% change body mass"
      , feature == "Adrenal_mass" ~ "adrenal mass (mg)"
      , feature == "Adrenal_mass_perBodyAM_g" ~ "adrenal mass normalized to AM mass (mg/g)"
      , feature == "ReproTract_mass" ~ "uterine mass (mg)"
      , feature == "ReproTract_mass_perBodyAM_g" ~ "uterine mass normalized to AM mass (mg/g)"
    )
  ) %>%
  mutate(
    feature = factor(
      feature
      , levels = c(
        "AM body mass (g)"
        ,"% change body mass"
        ,"adrenal mass (mg)"
        ,"adrenal mass normalized to AM mass (mg/g)"
        ,"uterine mass (mg)"
        ,"uterine mass normalized to AM mass (mg/g)"
      )
    )
  ) %>%
  arrange(
    feature
  )

femaleMassALPSAM_counts <- bind_rows(
  list(
    "diestrus" = femaleMassALPSAM_count_di
    , "proestrus" = femaleMassALPSAM_count_pro
  )
  , .id = "cycle stage"
)

femaleMassALPSAM_counts_header <- data.frame(
  col_keys = colnames(femaleMassALPSAM_counts)
  , line1 = c("", "", rep("STD", 4), rep("LBN", 4))
  , line2 = c("", "", rep(c(rep("CON", 2), rep("ALPS", 2)), 2))
  , line3 = c("cycle stage", "feature", rep(c("litters", "mice"), 4))
  , stringsAsFactors = FALSE
)

femaleMass_ALPSAM_counts_flexTable <- femaleMassALPSAM_counts %>%
  makeManuscriptFlexTable(
    headerDF = femaleMassALPSAM_counts_header
    , vertMergeCols = c("cycle stage")
    , vertLines = c(2, 4, 6, 8)
    , horzLines = c(2, 4, 6, 8, 10, 12)
  )

## LMM ----------------

femaleBodyMassAM_ALPS_LMM_tbl <- femaleBodyMassAM_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
femalePercChangeBodyMass_ALPS_LMM_tbl <- femalePercChangeBodyMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
femaleAdrenalMass_ALPS_LMM_tbl <- femaleAdrenalMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
femaleRelAdrenalMass_ALPS_LMM_tbl <- femaleRelAdrenalMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
femaleRelAdrenalMassPM_ALPS_LMM_tbl <- femaleRelAdrenalMassPM_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
femaleUterineMass_ALPS_LMM_tbl <- femaleUterineMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
femaleRelUterineMass_ALPS_LMM_tbl <- femaleRelUterineMass_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()
femaleRelUterineMassPM_ALPS_LMM_tbl <- femaleRelUterineMassPM_ALPS_lmm$anova_table %>%
  simplifyLMMOutput()

femaleALPS_RelPM_tbl <- bind_rows(
  list(
    `AM body mass (g)` = femaleBodyMassAM_ALPS_LMM_tbl
    , `% change body mass` = femalePercChangeBodyMass_ALPS_LMM_tbl
    , `adrenal mass (mg)` = femaleAdrenalMass_ALPS_LMM_tbl
    , `adrenal mass normalized to PM mass (mg/g)` = femaleRelAdrenalMassPM_ALPS_LMM_tbl
    , `uterine mass (mg)` = femaleUterineMass_ALPS_LMM_tbl
    , `uterine mass normalized to PM mass (mg/g)` = femaleRelUterineMassPM_ALPS_LMM_tbl
  ), .id = "feature"
)

femaleALPS_RelAM_tbl <- bind_rows(
  list(
    `AM body mass (g)` = femaleBodyMassAM_ALPS_LMM_tbl
    , `% change body mass` = femalePercChangeBodyMass_ALPS_LMM_tbl
    , `adrenal mass (mg)` = femaleAdrenalMass_ALPS_LMM_tbl
    , `adrenal mass normalized to AM mass (mg/g)` = femaleRelAdrenalMass_ALPS_LMM_tbl
    , `uterine mass (mg)` = femaleUterineMass_ALPS_LMM_tbl
    , `uterine mass normalized to AM mass (mg/g)` = femaleRelUterineMass_ALPS_LMM_tbl
  ), .id = "feature"
)

femaleALPS_RelAM_flexTable <- femaleALPS_RelAM_tbl %>%
  makeManuscriptFlexTable(
    round2Cols = c("F")
    , vertLines = c(2)
    , vertMergeCols = c("feature")
    , horzLines = seq(7, 7*6, 7)
  )

femaleALPS_RelPM_flexTable <- femaleALPS_RelPM_tbl %>%
  makeManuscriptFlexTable(
    round2Cols = c("F")
    , vertLines = c(2)
    , vertMergeCols = c("feature")
    , horzLines = seq(7, 7*6, 7)
  )

### EMMs -------

femalePercChangeBodyMass_ALPS_LMM_EMM_Sac_cycle_tbl <- femalePercChangeBodyMass_ALPS_lmm_emm_Sac_cycle %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "% change body mass"
  )

femalePercChangeBodyMass_ALPS_LMM_EMM_adultTrt_tbl <- femalePercChangeBodyMass_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "% change body mass"
  )

femaleAdrenalMass_ALPS_LMM_EMM_earlyLifeTrt_tbl <- femaleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "adrenal mass (mg)"
  )

femaleRelAdrenalMassPM_ALPS_LMM_EMM_adultTrt_tbl <- femaleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "adrenal mass normalized to PM mass (mg/g)"
  )

femaleUterineMass_ALPS_LMM_EMM_Sac_cycle_tbl <- femaleUterineMass_ALPS_lmm_emm_Sac_cycle %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "uterine mass (mg)"
  )

femaleUterineMass_ALPS_LMM_EMM_Sac_cycleAdultTrt_tbl <- femaleUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "uterine mass (mg)"
  )

femaleRelUterineMass_ALPS_LMM_EMM_Sac_cycle_tbl <- femaleRelUterineMass_ALPS_lmm_emm_Sac_cycle %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "uterine mass normalized to AM mass (mg/g)"
  )

femaleRelUterineMass_ALPS_LMM_EMM_Sac_cycleAdultTrt_tbl <- femaleRelUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "uterine mass normalized to AM mass (mg/g)"
  )

femaleRelUterineMassPM_ALPS_LMM_EMM_Sac_cycle_tbl <- femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycle %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "uterine mass normalized to PM mass (mg/g)"
  )

femaleRelUterineMassPM_ALPS_LMM_EMM_Sac_cycleAdultTrt_tbl <- femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycleAdultTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    feature = "uterine mass normalized to PM mass (mg/g)"
  )

femaleALPS_RelAM_EMM_tbl <- bind_rows(
  list(
    a = femalePercChangeBodyMass_ALPS_LMM_EMM_Sac_cycle_tbl
    , b = femalePercChangeBodyMass_ALPS_LMM_EMM_adultTrt_tbl
    , c = femaleAdrenalMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , d = femaleUterineMass_ALPS_LMM_EMM_Sac_cycle_tbl
    , e = femaleUterineMass_ALPS_LMM_EMM_Sac_cycleAdultTrt_tbl
    , f = femaleRelUterineMass_ALPS_LMM_EMM_Sac_cycle_tbl
    , g = femaleRelUterineMass_ALPS_LMM_EMM_Sac_cycleAdultTrt_tbl
  )
) %>%
  select(
    feature, earlyLifeTrt, adultTrt, Sac_cycle, everything()
  )

femaleALPS_RelPM_EMM_tbl <- bind_rows(
  list(
    a = femalePercChangeBodyMass_ALPS_LMM_EMM_Sac_cycle_tbl
    , b = femalePercChangeBodyMass_ALPS_LMM_EMM_adultTrt_tbl
    , c = femaleAdrenalMass_ALPS_LMM_EMM_earlyLifeTrt_tbl
    , c2 = femaleRelAdrenalMassPM_ALPS_LMM_EMM_adultTrt_tbl
    , d = femaleUterineMass_ALPS_LMM_EMM_Sac_cycle_tbl
    , e = femaleUterineMass_ALPS_LMM_EMM_Sac_cycleAdultTrt_tbl
    # , f = femaleRelUterineMassPM_ALPS_LMM_EMM_Sac_cycle_tbl
    , g = femaleRelUterineMassPM_ALPS_LMM_EMM_Sac_cycleAdultTrt_tbl
  )
) %>%
  select(
    feature, earlyLifeTrt, adultTrt, Sac_cycle, everything()
  )

femaleALPS_EMM_header <- data.frame(
  col_keys = colnames(femaleALPS_RelAM_EMM_tbl)
  , line1 = c("feature", "early-life treatment", "adult treatment", "cycle stage"
              , "emmean", "SEM", "df", "95% CI"
              
  )
)

femaleALPS_RelAM_EMM_flexTable <- femaleALPS_RelAM_EMM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = femaleALPS_EMM_header
    , vertMergeCols = c("feature"
                        # , "earlyLifeTrt", "adultTrt", "Sac_cycle"
                        )
    , vertLines = c(4)
    , horzLines = c(2, 4, 6, 8, 10, 12, 14, 16)
    , round1Cols = c("df")
    , round2Cols = c("emmean")
    , round3Cols = c("SEM")
  )

femaleALPS_RelPM_EMM_flexTable <- femaleALPS_RelPM_EMM_tbl %>%
  makeManuscriptFlexTable(
    headerDF = femaleALPS_EMM_header
    , vertMergeCols = c("feature"
                        # , "earlyLifeTrt", "adultTrt", "Sac_cycle"
                        )
    , vertLines = c(4)
    , horzLines = c(2, 4, 6, 8, 10, 12, 14, 16)
    , round1Cols = c("df")
    , round2Cols = c("emmean")
    , round3Cols = c("SEM")
  )
### Pairs --------

femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl <- femalePercChangeBodyMass_ALPS_lmm_emm_Sac_cycle.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "% change body mass"
  )

femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl <- femalePercChangeBodyMass_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "% change body mass"
  )

femaleAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl <- femaleAdrenalMass_ALPS_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "adrenal mass (mg)"
  )

femaleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl <- femaleRelAdrenalMassPM_ALPS_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "adrenal mass normalized to PM mass (mg/g)"
  )

femaleUterineMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl <- femaleUterineMass_ALPS_lmm_emm_Sac_cycle.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "uterine mass (mg)"
  )

femaleUterineMass_ALPS_LMM_EMM.pairs_Sac_cycleAdultTrt_tbl <- femaleUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "uterine mass (mg)"
  )

femaleRelUterineMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl <- femaleRelUterineMass_ALPS_lmm_emm_Sac_cycle.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "uterine mass normalized to AM mass (mg/g)"
  )

femaleRelUterineMass_ALPS_LMM_EMM.pairs_Sac_cycleAdultTrt_tbl <- femaleRelUterineMass_ALPS_lmm_emm_Sac_cycleAdultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "uterine mass normalized to AM mass (mg/g)"
  )

femaleRelUterineMassPM_ALPS_LMM_EMM.pairs_Sac_cycle_tbl <- femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycle.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "uterine mass normalized to PM mass (mg/g)"
  )

femaleRelUterineMassPM_ALPS_LMM_EMM.pairs_Sac_cycleAdultTrt_tbl <- femaleRelUterineMassPM_ALPS_lmm_emm_Sac_cycleAdultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    feature = "uterine mass normalized to PM mass (mg/g)"
  )

femaleALPS_RelAM_EMM.pairs_tbl <- bind_rows(
  list(
    a = femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl
    , b = femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , c = femaleAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , d = femaleUterineMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl
    , e = femaleUterineMass_ALPS_LMM_EMM.pairs_Sac_cycleAdultTrt_tbl
    , f = femaleRelUterineMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl
    , g = femaleRelUterineMass_ALPS_LMM_EMM.pairs_Sac_cycleAdultTrt_tbl
  )
) %>%
  select(
    feature, Sac_cycle, contrast, everything()
  )

femaleALPS_RelPM_EMM.pairs_tbl <- bind_rows(
  list(
    a = femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl
    , b = femalePercChangeBodyMass_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , c = femaleAdrenalMass_ALPS_LMM_EMM.pairs_earlyLifeTrt_tbl
    , c2 = femaleRelAdrenalMassPM_ALPS_LMM_EMM.pairs_adultTrt_tbl
    , d = femaleUterineMass_ALPS_LMM_EMM.pairs_Sac_cycle_tbl
    , e = femaleUterineMass_ALPS_LMM_EMM.pairs_Sac_cycleAdultTrt_tbl
    # , f = femaleRelUterineMassPM_ALPS_LMM_EMM.pairs_Sac_cycle_tbl
    , g = femaleRelUterineMassPM_ALPS_LMM_EMM.pairs_Sac_cycleAdultTrt_tbl
  )
) %>%
  select(
    feature, Sac_cycle, contrast, everything()
  )

femaleALPS_EMM.pairs_header <- data.frame(
  col_keys = colnames(femaleALPS_RelAM_EMM.pairs_tbl)
  , line1 = c("feature", "by cycle stage", "contrast"
              , "estimate", "SEM", "df", "t ratio", "p"
              
  )
)

femaleALPS_RelAM_EMM.pairs_flexTable <- femaleALPS_RelAM_EMM.pairs_tbl %>%
  makeManuscriptFlexTable(
    headerDF = femaleALPS_EMM.pairs_header
    , vertMergeCols = c("feature"
                        # "Sac_cycle"
                        )
    , vertLines = c(2)
    , horzLines = c(1, 2, 3, 4, 6, 7)
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

femaleALPS_RelPM_EMM.pairs_flexTable <- femaleALPS_RelPM_EMM.pairs_tbl %>%
  makeManuscriptFlexTable(
    headerDF = femaleALPS_EMM.pairs_header
    , vertMergeCols = c("feature"
                        # , "Sac_cycle"
                        )
    , vertLines = c(2)
    , horzLines = c(1, 2, 3, 4, 5, 7)
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )

# Figure 8 ----------------

## Counts -----------

LHcount_di <- acuteStressFilteredDi %>%
  group_by(
    earlyLifeTrt, adultTrt
  ) %>%
  summarize(
    litters = n_distinct(damID)
    , mice = n()
    , .groups = "drop"
  )

LHcount_ephys <- acuteStressFilteredPro_ephys %>%
  group_by(
    earlyLifeTrt, adultTrt
  ) %>%
  summarize(
    litters = n_distinct(damID)
    , mice = n()
    , .groups = "drop"
  )

LHcount_sampling <- acuteStressFilteredPro_sampling %>%
  group_by(
    earlyLifeTrt, adultTrt
  ) %>%
  summarize(
    litters = n_distinct(damID)
    , mice = n()
    , .groups = "drop"
  )

LHcount <- bind_rows(
  list(
    "diestrus" = LHcount_di
    , "proestrus: ephys" = LHcount_ephys
    , "proestrus: sampling" = LHcount_sampling
  ),
  .id = "usage"
) %>%
  pivot_longer(cols = c(litters, mice), names_to = "Measure", values_to = "Value") %>%
  unite("unitedName", earlyLifeTrt, adultTrt, Measure, sep = "_") %>%
  pivot_wider(
    names_from = unitedName,
    values_from = Value
  )

LHCount_header <- data.frame(
  col_keys = colnames(LHcount)
  , line1 = c("", rep("STD", 4), rep("LBN", 4))
  , line2 = c("", rep(c(rep("CON", 2), rep("ALPS", 2)), 2))
  , line3 = c("usage", rep(c("litters", "mice"), 4))
  , stringsAsFactors = FALSE
)

LHCount_flexTable <- LHcount %>%
  makeManuscriptFlexTable(
    headerDF = LHCount_header
    # , vertLines = c(2, 4, 6, 8, 10, 12)
    # , horzLines = c(2)
    # , vertMergeCols = c("earlyLifeTrt")
    , vertLines = c(1, 3, 5, 7)
    , horzLines = c(1, 2)
    , vertMergeCols = c("usage")
  )

## Diestrous - avg evening ------------

LH_diAfternoonLMM_tbl <- LH_diAfternoon_lmm$anova_table %>%
  simplifyLMMOutput()

LH_diAfternoon_flexTable <- LH_diAfternoonLMM_tbl %>%
  makeManuscriptFlexTable(
    vertLines = c(1)
    , fullWidth = FALSE
  )

## Proestrous - max evening ----------

LH_proEphysLMM_tbl <-  LH_proEphys_lmm$anova_table %>%
  simplifyLMMOutput()

LH_proSamplingLMM_tbl <-  LH_proSampling_lmm$anova_table %>%
  simplifyLMMOutput()

LH_pro_tbl <- bind_rows(
  list(
    "electrophysiology" = LH_proEphysLMM_tbl
    , "sampling" = LH_proSamplingLMM_tbl
  )
  , .id = "model"
) %>%
  pivot_wider(
    id_cols = variable, names_from = model, values_from = c("F", "df", "p")
  )

LH_pro_header <- data.frame(
  col_keys = c("variable", "F_electrophysiology", "df_electrophysiology", "p_electrophysiology"
               , "F_sampling", "df_sampling", "p_sampling"
  )
  , line2 = c("", rep("electrophysiology", 3), rep("sampling", 3))
  , line3 = c("variable", "F", "df", "p"
              , "F", "df", "p")
)

LH_pro_flexTable <- LH_pro_tbl %>%
  makeManuscriptFlexTable(
    headerDF = LH_pro_header
    , vertLines = c(1, 4)
  )

### EMMs -----------------
LH_proEphysLMM_emm_earlyLifeTrt_tbl <-  LH_proEphys_lmm_emm_earlyLifeTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    usage = "electrophysiology"
  )

LH_proEphysLMM_emm_adultTrt_tbl <-  LH_proEphys_lmm_emm_adultTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    usage = "electrophysiology"
  )

LH_proSamplingLMM_emm_adultTrt_tbl <-  LH_proSampling_lmm_emm_adultTrt %>%
  as_tibble() %>%
  simplifyEMMOutput() %>%
  mutate(
    usage = "sampling"
  )

LH_proLMM_EMM <- bind_rows(
  LH_proEphysLMM_emm_earlyLifeTrt_tbl
  , LH_proEphysLMM_emm_adultTrt_tbl
  , LH_proSamplingLMM_emm_adultTrt_tbl
) %>%
  relocate(
    adultTrt
    , .after = earlyLifeTrt
  ) %>%
  relocate(
    usage
    , .before = earlyLifeTrt
  )

LH_proLMM_EMM_flexTable <- LH_proLMM_EMM %>%
  makeManuscriptFlexTable(
    vertLines = c(3)
    , horzLines = c(2, 4)
    , vertMergeCols = "usage"
    , round1Cols = c("df")
    , round2Cols = c("emmean")
    , round3Cols = c("SEM")
  )

### Pairs ---------------

LH_proEphysLMM_emm_earlyLifeTrt.pairs_tbl <-  LH_proEphys_lmm_emm_earlyLifeTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    usage = "electrophysiology"
  )

LH_proEphysLMM_emm_adultTrt.pairs_tbl <-  LH_proEphys_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    usage = "electrophysiology"
  )

LH_proSamplingLMM_emm_adultTrt.pairs_tbl <-  LH_proSampling_lmm_emm_adultTrt.pairs %>%
  simplifyEMMPairsOutput() %>%
  mutate(
    usage = "sampling"
  )

LH_proLMM_EMM.pairs <- bind_rows(
  LH_proEphysLMM_emm_earlyLifeTrt.pairs_tbl
  , LH_proEphysLMM_emm_adultTrt.pairs_tbl
  , LH_proSamplingLMM_emm_adultTrt.pairs_tbl
) %>%
  relocate(
    usage
    , .before = contrast
  )

LH_proLMM_EMM.pairs_flexTable <- LH_proLMM_EMM.pairs %>%
  makeManuscriptFlexTable(
    vertLines = c(2)
    , horzLines = c(1, 2)
    , vertMergeCols = "usage"
    , round1Cols = c("df")
    , round2Cols = c("estimate", "t ratio")
    , round3Cols = c("SEM")
  )


### Chi-sq ------------

propSurgedChiSq_tbl <- propSurged.Chi.Sq.res %>%
  select(
    statistic
    , df
    , p
  ) %>%
  formatPCol() %>%
  mutate(
    variable = "early-life treatment"
    , .before = "statistic"
  ) %>%
  rename(
    `Chi-sq` = statistic
  )

propSurgedChiSq_flexTable <- propSurgedChiSq_tbl %>%
  makeManuscriptFlexTable(
    round2Cols = c("Chi-sq")
    , vertLines = c(1)
    , fullWidth = FALSE
  )


# Figure 9 -----------------------------------------

## Counts -------------------

GABApsc_cellCounts_flexTable <- GABApscs_240Filtered %>%
  group_by(
    earlyLifeTrt, adultTrt
  ) %>%
  summarize(
    litters = n_distinct(damID)
    , mice = n_distinct(mouseID)
    , cells = n()
    , .groups = "drop"
  ) %>%
  makeManuscriptFlexTable(
    vertLines = c(2)
    , horzLines = c(1, 2, 3)
    , fullWidth = FALSE
    , vertMergeCols = "earlyLifeTrt"
  )
GABApscs_240Filtered %>%
  filter(
    !is.na(relPeak)
  ) %>%
  group_by(
    earlyLifeTrt, adultTrt
  ) %>%
  summarize(
    litters = n_distinct(damID)
    , mice = n_distinct(mouseID)
    , cells = n()
    , .groups = "drop"
  ) %>%
  makeManuscriptFlexTable(
    vertLines = c(2)
    , horzLines = c(1, 2, 3)
    , fullWidth = FALSE
    , vertMergeCols = "earlyLifeTrt"
  )


pscProps %>%
  group_by(
    earlyLifeTrt
    , adultTrt
  ) %>%
  summarize(
    events = n()
    , .groups = "drop"
  )




## Quantiles ---------------------

# logAmplitude_models_sum_tbl <- logAmplitude_models_sum$tTable %>%
#   simplifyAllQuartilesOutput()
# 
# logRiseTime_models_sum_tbl <- logRiseTime_models_sum$tTable %>%
#   simplifyAllQuartilesOutput()
# 
# logDecayTime_models_sum_tbl <- logDecayTime_models_sum$tTable %>%
#   simplifyAllQuartilesOutput()
# 
# logFWHM_models_sum_tbl <- logFWHM_models_sum$tTable %>%
#   simplifyAllQuartilesOutput()
# 
# logModels_tbl <- bind_rows(
#   list(
#     "amplitude (pA)" = logAmplitude_models_sum_tbl
#     , "rise time (ms)" = logRiseTime_models_sum_tbl
#     , "decay time (ms)" = logDecayTime_models_sum_tbl
#     , "FWHM (ms)" = logFWHM_models_sum_tbl
#   )
#   , .id = "feature"
# ) %>%
#   pivot_wider(
#     id_cols = c(feature, `fixed effect`), names_from = quartile, values_from = c("Value", "SEM", "95% CI", "p")
#   )
# 
# logModels_header <- data.frame(
#   col_keys = c("feature", "fixed effect"
#                , "Value_0.25", "SEM_0.25", "95% CI_0.25", "p_0.25"
#                , "Value_0.5", "SEM_0.5", "95% CI_0.5", "p_0.5"
#                , "Value_0.75", "SEM_0.75", "95% CI_0.75", "p_0.75"
#   )
#   , line2 = c("", ""
#               , rep("25th percentile", 4)
#               , rep("50th percentile", 4)
#               , rep("75th percentile", 4)
#               )
#   , line3 = c("feature", "fixed effect"
#               , "Value", "SEM", "95% CI", "p"
#               , "Value", "SEM", "95% CI", "p"
#               , "Value", "SEM", "95% CI", "p"
#               )
# )
# 
# logModels_flexTable <- logModels_tbl %>%
#   makeManuscriptFlexTable(
#     headerDF = logModels_header
#     , vertLines = c(2, 6, 10)
#     , horzLines = c(4, 8, 12, 16)
#     , round2Cols = c("Value_0.25", "Value_0.5", "Value_0.75")
#     , round3Cols = c("SEM_0.25", "SEM_0.5", "SEM_0.75")
#     , vertMergeCols = c("feature")
#   )
# 



