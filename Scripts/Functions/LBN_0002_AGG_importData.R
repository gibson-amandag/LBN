#### FUNCTIONS TO IMPORT DATA ###########################################################################

# Tables have been re-written so that both paradigm dates are within the same table
# There's a column "ParaType" - 2 = P2-P9; 4 = P4-P11
# Mass dates are labeled with their appropriate dates. 
# There will just be NAs when there is not a value for that day because of different procedures
# Some other refernces specific to start or end of paradigm have been changed to
# _startPara or _endPara

##Load datasets ------------------------

load_LBN_data <- function(
  dataFolder,
  excelName,
  NameDemo_dam = "Demo_dam",
  NameDemo_off ="Demo_off",
  NameMass_off = "Mass_off",
  NameMaturation_off = "Maturation_off",
  NameEndPara_off = "EndParadigm_off",
  NameCycles_off = "Cycles_off",
  NameChronicStress_off = "ChronicStress_off",
  NameAcuteStress_off = "AcuteStress_off",
  NameDam_litter1 = "Dam_litter1",
  NameMaturation_litter1 = "Maturation_litter1",
  NameCRH_dam = "CRH_dam"
){
  
  
  Demo_dam <- myXLSX_func(dataFolder, excelName, NameDemo_dam) #replaced Dam_info
  Demo_off <- myXLSX_func(dataFolder, excelName, NameDemo_off) #replaced Offspring_demo
  Mass_off <- myXLSX_func(dataFolder, excelName, NameMass_off) #replaced Offspring_mass
  Maturation_off <- myXLSX_func(dataFolder, excelName, NameMaturation_off) #replaced Offspring_maturation
  EndPara_off <- myXLSX_func(dataFolder, excelName, NameEndPara_off) #replaced Offspring_post_para
  Cycles_off <- myXLSX_func(dataFolder, excelName, NameCycles_off)
  AcuteStress_off <- myXLSX_func(dataFolder, excelName, NameAcuteStress_off) #replaced Offspring_acute_stress
  ChronicStress_off <- myXLSX_func(dataFolder, excelName, NameChronicStress_off) #replaced Offspring_chronic_stress
  Dam_litter1 <- myXLSX_func(dataFolder, excelName, NameDam_litter1)
  Maturation_litter1 <- myXLSX_func(dataFolder, excelName, NameMaturation_litter1)
  CRH_dam <- myXLSX_func(dataFolder, excelName, NameCRH_dam)
  
  #Format IDs as characters
  Demo_dam$Dam_ID <- as.character(Demo_dam$Dam_ID)
  Demo_dam$Dam <- as.character(Demo_dam$Dam)
  Demo_off$Dam_ID <- as.character(Demo_off$Dam_ID)
  
  Demo_off$Mouse_ID <- as.character(Demo_off$Mouse_ID)
  Mass_off$Mouse_ID <- as.character(Mass_off$Mouse_ID)
  Maturation_off$Mouse_ID <- as.character(Maturation_off$Mouse_ID)
  EndPara_off$Mouse_ID <- as.character(EndPara_off$Mouse_ID)
  Cycles_off$Mouse_ID <- as.character(Cycles_off$Mouse_ID)
  AcuteStress_off$Mouse_ID <- as.character(AcuteStress_off$Mouse_ID)
  ChronicStress_off$Mouse_ID <- as.character(ChronicStress_off$Mouse_ID)
  
  Dam_litter1$Dam_ID <- as.character(Dam_litter1$Dam_ID)
  Dam_litter1$Dam <- as.character(Dam_litter1$Dam)
  Maturation_litter1$Dam_ID <- as.character(Maturation_litter1$Dam_ID)
  Maturation_litter1$Mouse_ID <- as.character(Maturation_litter1$Mouse_ID)
  
  CRH_dam$Dam_ID <- as.character(CRH_dam$Dam_ID)
  CRH_dam$Dam <- as.character(CRH_dam$Dam)
  
  #Make the paradigm type a factor variable
  Demo_dam$ParaType <- as.factor(Demo_dam$ParaType)
  
  #Make the litter number a factor variable
  Demo_dam$Litter_num <- as.factor(Demo_dam$Litter_num)
  Dam_litter1$Litter_num <- as.factor(Dam_litter1$Litter_num)
  
  #Add a "pupLoss" column to Demo_dam
  Demo_dam <- Demo_dam %>%
    mutate(pupLoss = Litter_size_startPara - Litter_size_endPara)
  
  
    
  
  #Combine into a single dataframe - to be used for variable names
  LBN_all <- Demo_off %>%
    left_join(Demo_dam, by = "Dam_ID") %>%
    full_join(select(Mass_off, -ParaType), by = "Mouse_ID") %>%
    full_join(Maturation_off, by = "Mouse_ID") %>%
    full_join(EndPara_off, by = "Mouse_ID") %>%
    full_join(Cycles_off, by = "Mouse_ID") %>%
    full_join(AcuteStress_off, by = "Mouse_ID") %>%
    full_join(ChronicStress_off, by = "Mouse_ID")
  
  Litter1_all <- Maturation_litter1 %>%
    full_join(Dam_litter1, by = "Dam_ID")
  
  #Create a new dataframe with the demographic info from the dam sheet that is relevant for pups
  
  Demo_dam_for_offspring <- Demo_dam %>%
    select(
      Dam_ID,
      Dam,
      Dam_cage, 
      Treatment,
      Litter_num,
      Dam_Strain,
      Strain,
      ParaType,
      Sire, 
      DOB, 
      Avg_litter_mass_startPara, 
      Litter_size_startPara, 
      Litter_size_endPara,
      pupLoss
    )
  
  #Create a new dataframe with the demographic info from the dam_litter1 sheet that is relevant for pups
  
  Dam_litter1_for_offspring <- Dam_litter1 %>%
    select(
      Dam_ID,
      Dam,
      Litter_num,
      Dam_cage, 
      Dam_Strain,
      Strain,
      Sire, 
      DOB, 
      Pups_through_wean,
      Litter_size_wean,
      Rebreed_date
    )
  
  #Add the demographic info, join by Dam_ID
  Demo_off <- Demo_off %>%
    left_join(Demo_dam_for_offspring, by = "Dam_ID") %>%
    select(
      Mouse_ID:Dam_ID, 
      Dam:Litter_num,
      DOB, 
      Treatment:ParaType,
      Wean_Cage_Number:Dam_cage,
      Sire,
      Avg_litter_mass_startPara:pupLoss
    )
  
  #Add the dam demo info to litter 1 maturation table
  Maturation_litter1 <- Maturation_litter1 %>%
    left_join(Dam_litter1_for_offspring, by = "Dam_ID") %>%
    mutate(
      Litter_num = "undisturbed",
      Treatment = "Control"
    )
  
  
  #Combine all of the data into a single dataframe. Will add NAs where there isn't data
  #can add ,by = ... to tell what column to join on
  #not including dam info
  
  LBN_data <- Demo_off %>%
    left_join(select(Mass_off, -ParaType), by = "Mouse_ID") %>%
    left_join(Maturation_off, by = "Mouse_ID") %>%
    left_join(EndPara_off, by = "Mouse_ID") %>%
    left_join(Cycles_off, by = "Mouse_ID") %>%
    left_join(AcuteStress_off, by = "Mouse_ID") %>%
    left_join(ChronicStress_off, by = "Mouse_ID")
  
  #Add demographic information to the smaller datasets
  Mass_off <- LBN_data %>%
    select(all_of(demoVars_forOff_quo), Avg_litter_mass_startPara) %>%
    left_join(select(Mass_off, -ParaType), by = "Mouse_ID") %>%
    select( #reorder
      Mouse_ID,
      Sex,
      Treatment,
      Avg_litter_mass_startPara:Mass_P72,
      Dam_ID,
      Dam,
      DOB,
      Litter_num:pupLoss
    )
  
  #Format Mat Dates as date
  Maturation_off$VO_day = as_date(Maturation_off$VO_day)
  Maturation_off$Estrus_day = as_date(Maturation_off$Estrus_day)
  Maturation_off$PreputialSep_day = as_date(Maturation_off$PreputialSep_day)
  Maturation_litter1$VO_day = as_date(Maturation_litter1$VO_day)
  Maturation_litter1$Estrus_day = as_date(Maturation_litter1$Estrus_day)
  Maturation_litter1$PreputialSep_day = as_date(Maturation_litter1$PreputialSep_day)
  
  Maturation_off <- LBN_data %>%
    select(
      all_of(demoVars_forOff_quo),
      Mass_P22, 
      Mass_P23, 
      Mass_P24, 
      Mass_P70, 
      Mass_P71, 
      Mass_P72
    ) %>%
    left_join(Maturation_off, by = "Mouse_ID") %>%
    select( #reorder
      Mouse_ID,
      Sex,
      Treatment,
      VO_day:AGD_P72,
      Mass_P22:Mass_P72,
      Dam_ID,
      DOB,
      Dam,
      Litter_num:pupLoss
    )
  
  Maturation_off <- Maturation_off %>%
    mutate(
      AGD_wean = (AGD_P22 + AGD_P23 + AGD_P24) / 3,
      AGD_adult = (AGD_P70 + AGD_P71 + AGD_P72) / 3,
      Mass_wean = (Mass_P22 + Mass_P23 + Mass_P24) / 3,
      Mass_adult = (Mass_P70 + Mass_P71 + Mass_P72) / 3,
      AGD_wean_by_mass = AGD_wean / Mass_wean,
      AGD_adult_by_mass = AGD_adult / Mass_adult,
      VO_age = as.numeric(VO_day - DOB), #if don't include as.numeric it will output in days
      Estrus_age = as.numeric(Estrus_day - DOB),
      PreputialSep_age = as.numeric(PreputialSep_day - DOB)
    ) %>%
    select(
      Mouse_ID:Treatment,
      AGD_wean:PreputialSep_age, 
      VO_day:pupLoss
    )
  
  EndPara_off <- LBN_data %>%
    select(all_of(demoVars_forOff_quo)) %>%
    left_join(EndPara_off, by = "Mouse_ID") %>%
    select(
      Mouse_ID, #reorder
      Sex,
      Treatment,
      Cort_endPara:CRH_endPara,
      Dam_ID,
      DOB,
      Dam,
      Litter_num:pupLoss
    )
  
  Cycles_off <- LBN_data %>%
    select(all_of(demoVars_forOff_quo)) %>%
    left_join(Cycles_off, by = "Mouse_ID") %>%
    select(  #reorder
      Mouse_ID,
      Sex,
      Treatment,
      Day1:Day21,
      Cycle_length:Proestrus_days,
      Dam_ID,
      DOB,
      Litter_num:pupLoss
    )
  
  AcuteStress_off <- LBN_data %>%
    select(all_of(demoVars_forOff_quo)) %>%
    left_join(AcuteStress_off, by = "Mouse_ID") %>%
    select( #reorder
      Mouse_ID,
      Sex,
      Treatment,
      Stress_cycle:LH_5.5,
      Dam_ID,
      DOB,
      Dam,
      Litter_num:pupLoss
    )
  
  ChronicStress_off <- LBN_data %>%
    select(all_of(demoVars_forOff_quo)) %>%
    left_join(ChronicStress_off, by = "Mouse_ID") %>%
    select( #reorder
      Mouse_ID,
      Sex,
      Treatment,
      Chronic_stress_treatment:Stress_proestrus,
      Dam_ID,
      DOB,
      Dam,
      Litter_num:pupLoss
    )
  
  
  LBN_all <- LBN_all %>%
    left_join(
      Maturation_off %>% 
        select(
          Mouse_ID,
          AGD_wean:PreputialSep_age
        ), 
      by = "Mouse_ID"
    )
  
  LBN_data <- LBN_data %>%
    left_join(
      Maturation_off %>%
        select(
          Mouse_ID,
          AGD_wean:PreputialSep_age
        ), 
      by = "Mouse_ID"
    )
  
  
  #Assign this function to an object. Then call the specific items as df$Demo_dam or df[[Demo_dam]] for example. 
  #All the dataframes will be within this one list that is the output of the function
  return(
    list(
      "Demo_dam" = Demo_dam,
      "Demo_off" = Demo_off,
      "Mass_off" = Mass_off,
      "Maturation_off" = Maturation_off,
      "EndPara_off" = EndPara_off,
      "Cycles_off" = Cycles_off,
      "AcuteStress_off" = AcuteStress_off,
      "ChronicStress_off" = ChronicStress_off,
      "LBN_all" = LBN_all,
      "LBN_data" = LBN_data,
      "Litter1_all" = Litter1_all,
      "Dam_litter1" = Dam_litter1,
      "Maturation_litter1" = Maturation_litter1,
      "Dam_CRH" = CRH_dam
    )
  )
}
