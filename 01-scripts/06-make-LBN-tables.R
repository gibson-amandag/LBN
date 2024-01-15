
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
    comparison = paste0(group2, " - ", group1)
    , .after = `.y.`
  ) %>%
  mutate(
    `95% CI` = paste0("[", format(round(conf.low, 2), nsmall = 2), ", ", format(round(conf.high, 2), nsmall = 2), "]")
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
  col_keys = colnames(matCountTbl)
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
  unite("combined", hormoneStatus, time, measure, sep = "_") %>%
  pivot_wider(
    names_from = combined,
    values_from = Value
  )

cortCount_header <- data.frame(
  col_keys = colnames(cortCount)
  , line1 = c("", "", rep("males", 4), rep("diestrous females", 4), rep("proestrous females", 4))
  , line2 = c("", "", rep(c(rep("pre", 2), rep("post", 2)), 3))
  , line3 = c("early-life treatment", "adult treatment", rep(c("litters", "mice"), 6))
  , stringsAsFactors = FALSE
)

cortCount_flexTable <- cortCount %>%
  makeManuscriptFlexTable(
    headerDF = cortCount_header
    , vertLines = c(2, 4, 6, 8, 10, 12)
    , horzLines = c(2)
    , vertMergeCols = c("earlyLifeTrt")
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
    # , horzLines = c(7)
    # , vertMergeCols = c(1)
  )