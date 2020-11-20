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
  NameCycles_off = "CycleSum_off",
  NameChronicStress_off = "ChronicStress_off",
  NameAcuteStress_off = "AcuteStress_off"
){


  Demo_dam <- myXLSX_func(dataFolder, excelName, NameDemo_dam) #replaced Dam_info
  Demo_off <- myXLSX_func(dataFolder, excelName, NameDemo_off) #replaced Offspring_demo
  Mass_off <- myXLSX_func(dataFolder, excelName, NameMass_off) #replaced Offspring_mass
  Maturation_off <- myXLSX_func(dataFolder, excelName, NameMaturation_off) #replaced Offspring_maturation
  EndPara_off <- myXLSX_func(dataFolder, excelName, NameEndPara_off) #replaced Offspring_post_para
  Cycles_off <- myXLSX_func(dataFolder, excelName, NameCycles_off)
  AcuteStress_off <- myXLSX_func(dataFolder, excelName, NameAcuteStress_off) #replaced Offspring_acute_stress
  ChronicStress_off <- myXLSX_func(dataFolder, excelName, NameChronicStress_off) #replaced Offspring_chronic_stress
  
  #Format IDs as characters
  Demo_dam$Dam_ID <- as.character(Demo_dam$Dam_ID)
  Demo_off$Dam_ID <- as.character(Demo_off$Dam_ID)
  
  Demo_off$Mouse_ID <- as.character(Demo_off$Mouse_ID)
  Mass_off$Mouse_ID <- as.character(Mass_off$Mouse_ID)
  Maturation_off$Mouse_ID <- as.character(Maturation_off$Mouse_ID)
  EndPara_off$Mouse_ID <- as.character(EndPara_off$Mouse_ID)
  Cycles_off$Mouse_ID <- as.character(Cycles_off$Mouse_ID)
  AcuteStress_off$Mouse_ID <- as.character(AcuteStress_off$Mouse_ID)
  ChronicStress_off$Mouse_ID <- as.character(ChronicStress_off$Mouse_ID)
  
  #Combine into a single dataframe - to be used for variable names
  LBN_all <- Demo_off %>%
    left_join(Demo_dam, by = "Dam_ID") %>%
    full_join(select(Mass_off, -ParaType), by = "Mouse_ID") %>%
    full_join(Maturation_off, by = "Mouse_ID") %>%
    full_join(EndPara_off, by = "Mouse_ID") %>%
    full_join(Cycles_off, by = "Mouse_ID") %>%
    full_join(AcuteStress_off, by = "Mouse_ID") %>%
    full_join(ChronicStress_off, by = "Mouse_ID")
  
  #Create a new dataframe with the demographic info from the dam sheet that is relevant for pups
  
  Demo_dam_for_offspring <- Demo_dam %>%
    select(Dam_ID, Dam_cage, Treatment, Dam_Strain, Sire, DOB, Avg_litter_mass_startPara, Litter_size_startPara, Litter_size_endPara)
  
  #Add the demographic info, join by Dam_ID
  Demo_off <- Demo_off %>%
    left_join(Demo_dam_for_offspring, by = "Dam_ID") %>%
    select(Mouse_ID:Dam_ID, DOB, Wean_Cage_Number:Sire, Avg_litter_mass_startPara:Litter_size_endPara)
  
  
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


#If want to join only certain columns from one dataframe, use select() first

#Need to add some of the demographic data to the other data frames to be able to observe. Like DOB, treatment, esp

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
    "LBN_data" = LBN_data
  )
)
}
