##### VARIABLE NAMES AND GROUPS ###############################################

#Create a new data table that contains a more descriptive name for each variable

#Full variable names for plots


LBN_varNames_func <- function(
  dataFrame
)
{
  #Create a matrix with one row, the length of the dataframe
  LBN_VarNames <- matrix(nrow = 1, ncol = length(dataFrame))
  colnames(LBN_VarNames) <- colnames(dataFrame) #make the column names the same as the column names in dataframe
  LBN_VarNames <- as.data.frame(LBN_VarNames) #Make it a data frame
  
  
  LBN_VarNames$Mouse_ID = "Mouse ID"
  LBN_VarNames$Ear_tag = "Ear Tag"
  LBN_VarNames$Sex = "Sex"
  LBN_VarNames$Dam_ID = "Dam ID"
  
  LBN_VarNames$Wean_Cage_Number = "Offspring Cage #"
  LBN_VarNames$Changed_cage = "Moved cages post-weaning"
  LBN_VarNames$New_cage_mate = "Added mouse to cage"
  LBN_VarNames$Lost_cage_mate = "Removed mouse from cage"
  
  LBN_VarNames$Dam_cage = "Dam Cage #"
  
  LBN_VarNames$Treatment = "Treatment"
  LBN_VarNames$Dam_Strain = "Dam Strain"
  LBN_VarNames$Strain = "Offspring Strain"
  
  LBN_VarNames$Dam_Generation = "Dam Generation"
  LBN_VarNames$Dam_DOB = "Dam DOB"
  LBN_VarNames$Breed_date = "Breed Date"
  LBN_VarNames$Sire = "Sire"
  LBN_VarNames$Plug_date = "Plug Date"
  
  LBN_VarNames$DOB = "Date of Birth"
  LBN_VarNames$ParaType = "Paradigm Start Day"
  
  LBN_VarNames$Dam_Mass_P2 = "Dam Mass P2"
  LBN_VarNames$Dam_Mass_P9 = "Dam Mass P9"
  LBN_VarNames$Dam_Mass_P4 = "Dam Mass P4"
  LBN_VarNames$Dam_Mass_P11 = "Dam Mass P11"
  LBN_VarNames$Dam_Mass_P21 = "Dam Mass P21"
  
  LBN_VarNames$Avg_litter_mass_startPara = "Avg Litter Mass Paradigm Start"
  LBN_VarNames$Litter_size_startPara = "Litter Size Paradigm Start"
  LBN_VarNames$Litter_size_endPara = "Litter Size Paradigm End"
  LBN_VarNames$pupLoss = "# Pups Lost During Paradigm"
  
  LBN_VarNames$Camera_number = "Camera Number"
  LBN_VarNames$Sac_or_stop = "Sacrificed or Stop tracking"
  
  LBN_VarNames$Perc_on_nest_block1 = "Percent on Nest 1"
  LBN_VarNames$Perc_on_nest_block2 = "Percent on Nest 2"
  LBN_VarNames$Perc_on_nest_block3 = "Percent on Nest 3"
  LBN_VarNames$Perc_on_nest_block4 = "Percent on Nest 4"
  
  LBN_VarNames$Nest_entries_block1 = "Nest Entries 1"
  LBN_VarNames$Nest_entries_block2 = "Nest Entries 2"
  LBN_VarNames$Nest_entries_block3 = "Nest Entries 3"
  LBN_VarNames$Nest_entries_block4 = "Nest Entries 4"
  
  LBN_VarNames$Dist_traveled_block1 = "Distanced Traveled 1"
  LBN_VarNames$Dist_traveled_block2 = "Distanced Traveled 2"
  LBN_VarNames$Dist_traveled_block3 = "Distanced Traveled 3"
  LBN_VarNames$Dist_traveled_block4 = "Distanced Traveled 4"
  
  LBN_VarNames$Cort_dam_endPara = "Dam Cort Paradigm End"
  LBN_VarNames$CRH_dam_endPara = "Dam CRH Paradigm End"
  LBN_VarNames$Cort_dam_P21 = "Dam Cort on P21"
  
  LBN_VarNames$Mass_P9 = "Offspring Mass at P9"
  LBN_VarNames$Mass_P10 = "Offspring Mass at P10"
  LBN_VarNames$Mass_P11 = "Offspring Mass at P11"
  LBN_VarNames$Mass_P12 = "Offspring Mass at P12"
  LBN_VarNames$Mass_P13 = "Offspring Mass at P13"
  LBN_VarNames$Mass_P14 = "Offspring Mass at P14"
  LBN_VarNames$Mass_P15 = "Offspring Mass at P15"
  LBN_VarNames$Mass_P16 = "Offspring Mass at P16"
  LBN_VarNames$Mass_P17 = "Offspring Mass at P17"
  LBN_VarNames$Mass_P19 = "Offspring Mass at P19"
  LBN_VarNames$Mass_P21 = "Offspring Mass at P21"
  LBN_VarNames$Mass_P22 = "Offspring Mass at P22"
  LBN_VarNames$Mass_P23 = "Offspring Mass at P23"
  LBN_VarNames$Mass_P24 = "Offspring Mass at P24"
  LBN_VarNames$Mass_P28 = "Offspring Mass at P28"
  LBN_VarNames$Mass_P35 = "Offspring Mass at P35"
  LBN_VarNames$Mass_P42 = "Offspring Mass at P42"
  LBN_VarNames$Mass_P49 = "Offspring Mass at P49"
  LBN_VarNames$Mass_P56 = "Offspring Mass at P56"
  LBN_VarNames$Mass_P63 = "Offspring Mass at P63"
  LBN_VarNames$Mass_P70 = "Offspring Mass at P70"
  LBN_VarNames$Mass_P71 = "Offspring Mass at P71"
  LBN_VarNames$Mass_P72 = "Offspring Mass at P72"
  
  LBN_VarNames$VO_day = "Date of Vaginal Opening"
  LBN_VarNames$VO_mass = "Mass at Vaginal Opening"
  LBN_VarNames$Estrus_day = "Date of First Estrus"
  LBN_VarNames$Estrus_mass = "Mass at First Estrus"
  LBN_VarNames$PreputialSep_day = "Date of Preputial Sep"
  LBN_VarNames$PreputialSep_mass = "Mass at Preputial Sep"
  
  LBN_VarNames$AGD_P22 = "AGD at P22"
  LBN_VarNames$AGD_P23 = "AGD at P23"
  LBN_VarNames$AGD_P24 = "AGD at P24"
  LBN_VarNames$AGD_P70 = "AGD at P70"
  LBN_VarNames$AGD_P71 = "AGD at P71"
  LBN_VarNames$AGD_P72 = "AGD at P72"
  
  LBN_VarNames$AGD_wean = "Avg Juvenile AGD"
  LBN_VarNames$AGD_adult = "Avg Adult AGD"
  LBN_VarNames$Mass_wean = "Avg Juvenile Mass"
  LBN_VarNames$Mass_adult = "Avg Adult Mass"
  LBN_VarNames$AGD_wean_by_mass = "Avg Juvenile AGD by Mass"
  LBN_VarNames$AGD_adult_by_mass = "Avg Adult AGD by Mass"
  LBN_VarNames$VO_age = "Age at Vaginal Opening"
  LBN_VarNames$Estrus_age = "Age at First Estrus"
  LBN_VarNames$PreputialSep_age = "Age at Preputial Sep"
  
  LBN_VarNames$Cort_endPara = "Offspring Cort at Paradigm End"
  LBN_VarNames$CRH_endPara = "Offspring CRH at Paradigm End"
  
  LBN_VarNames$Cycle_length = "Cycle Length"
  LBN_VarNames$Cycle_num = "# of Cycles"
  LBN_VarNames$Diestrus_days = "# Days in Diestrus"
  LBN_VarNames$Estrus_days = "# Days in Estrus"
  LBN_VarNames$Proestrus_days = "# Days in Proestrus"
  
  LBN_VarNames$Stress_cycle = "Cycle Stage at Stress Txt"
  LBN_VarNames$Stress_date = "Date of Acute Stress"
  LBN_VarNames$Uterine_mass = "Uterine Mass after Acute Stress"
  LBN_VarNames$Stress_treatment = "Stress Treatment"
  LBN_VarNames$Cort_pre = "Pre-treatment Cort"
  LBN_VarNames$Cort_post = "Post-treatment Cort"
  LBN_VarNames$LH_0 = "LH at time 0"
  LBN_VarNames$LH_5 = "LH at 5hr"
  LBN_VarNames$LH_5.5 = "LH at 5.5hr"
  
  LBN_VarNames$Chronic_stress_treatment = "Chronic Stress Treatment"
  LBN_VarNames$Stress_Cycle_length = "Cycle Lenth"
  LBN_VarNames$Stress_Cycle_num = "# of Cycles"
  LBN_VarNames$Stress_diestrus = "# Days in Diestrus"
  LBN_VarNames$Stress_estrus = "# Days in Estrus"
  LBN_VarNames$Stress_proestrus = "# Days in Proestrus"
  
  return(LBN_VarNames)
}

#create a list with different groupings of variables. These can be use to build other dataframes or to analyze certain variables
demoVars_forOff_quo = c("Mouse_ID",
                        "Dam_ID",
                        "Sex",
                        "DOB",
                        "Treatment",
                        "Dam_Strain",
                        "Strain",
                        "ParaType",
                        "Sire",
                        "Wean_Cage_Number",
                        "Changed_cage",
                        "New_cage_mate",
                        "Lost_cage_mate",
                        "Litter_size_startPara",
                        "Litter_size_endPara",
                        "pupLoss"
)

demoVars_forOff = exprs(Mouse_ID,
                 Dam_ID,
                 Sex,
                 DOB,
                 Treatment,
                 Dam_Strain,
                 Strain,
                 ParaType,
                 Sire,
                 Wean_Cage_Number,
                 Changed_cage,
                 New_cage_mate,
                 Lost_cage_mate,
                 Litter_size_startPara,
                 Litter_size_endPara,
                 pupLoss
                 )


massDates_quo = c(
  "Avg_litter_mass_startPara",
  "Mass_P9",
  "Mass_P10",
  "Mass_P11",
  "Mass_P12",
  "Mass_P13",
  "Mass_P14",
  "Mass_P15",
  "Mass_P16",
  "Mass_P17",
  "Mass_P19",
  "Mass_P21",
  "Mass_P22",
  "Mass_P23",
  "Mass_P24",
  "Mass_P28",
  "Mass_P35",
  "Mass_P42",
  "Mass_P49",
  "Mass_P56",
  "Mass_P63",
  "Mass_P70",
  "Mass_P71",
  "Mass_P72" 
)

massDates = exprs(
  Avg_litter_mass_startPara,
  Mass_P9,
  Mass_P10,
  Mass_P11,
  Mass_P12,
  Mass_P13,
  Mass_P14,
  Mass_P15,
  Mass_P16,
  Mass_P17,
  Mass_P19,
  Mass_P21,
  Mass_P22,
  Mass_P23,
  Mass_P24,
  Mass_P28,
  Mass_P35,
  Mass_P42,
  Mass_P49,
  Mass_P56,
  Mass_P63,
  Mass_P70,
  Mass_P71,
  Mass_P72 
)

damMassDates_quo = c(
  "Dam_Mass_P2",
  "Dam_Mass_P9",
  "Dam_Mass_P4",
  "Dam_Mass_P11",
  "Dam_Mass_P21"
)

damMassDates = exprs(
  Dam_Mass_P2,
  Dam_Mass_P9,
  Dam_Mass_P4,
  Dam_Mass_P11,
  Dam_Mass_P21
)

