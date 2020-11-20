## Get data ready

readRenviron("./.Renviron") #read the .Renviron document from the project root folder

#Data File Name
LBN_DataName <- Sys.getenv("EXCEL_FILE_NAME")

#Functions Script (ends in .R)
FunctionsFileName <- "LBN_0002_AGG_functions_Fall2020.R"

#Info about working directory and R Markdown: https://martinctc.github.io/blog/rstudio-projects-and-working-directories-a-beginner's-guide/#fn1
#More info: https://bookdown.org/yihui/rmarkdown-cookbook/working-directory.html

#Where R Notebook files are saved
ScriptsFolder <- file.path("Scripts")

#Where Function files are saved
FunctionsFolder <- file.path(ScriptsFolder, "Functions")

#Where data files are saved
DataFolder <- Sys.getenv("DATA_FOLDER")

#Where output should be saved
OutputFolder <- Sys.getenv("OUTPUT_FOLDER")

#Where plot output should be saved
PlotFolder <- file.path(OutputFolder, "Plots")

#Where data output should be saved
DataOutFolder <- file.path(OutputFolder, "Data")

#Load the LBN Functions
source(file.path(FunctionsFolder, FunctionsFileName))

##Load datasets

#Sheet names of excel file
NameDemo_dam <- "Demo_dam"
NameDemo_off <- "Demo_off"
NameMass_off <- "Mass_off"
NameMaturation_off <- "Maturation_off"
NameEndPara_off <- "EndParadigm_off"
NameCycles_off <- "Cycles_off"
NameChronicStress_off <- "ChronicStress_off"
NameAcuteStress_off <- "AcuteStress_off"

Demo_off <- myXLSX_func(DataFolder, LBN_DataName, NameDemo_off) #replaced Offspring_demo
Mass_off <- myXLSX_func(DataFolder, LBN_DataName, NameMass_off) #replaced Offspring_mass
Demo_dam <- myXLSX_func(DataFolder, LBN_DataName, NameDemo_dam) #replaced Dam_info
Maturation_off <- myXLSX_func(DataFolder, LBN_DataName, NameMaturation_off) #replaced Offspring_maturation
EndPara_off <- myXLSX_func(DataFolder, LBN_DataName, NameEndPara_off) #replaced Offspring_post_para
AcuteStress_off <- myXLSX_func(DataFolder, LBN_DataName, NameAcuteStress_off) #replaced Offspring_acute_stress
ChronicStress_off <- myXLSX_func(DataFolder, LBN_DataName, NameChronicStress_off) #replaced Offspring_chronic_stress

#Format IDs as characters
Demo_dam$Dam_ID <- as.character(Demo_dam$Dam_ID)
Demo_off$Dam_ID <- as.character(Demo_off$Dam_ID)

AcuteStress_off$Mouse_ID <- as.character(AcuteStress_off$Mouse_ID)
ChronicStress_off$Mouse_ID <- as.character(ChronicStress_off$Mouse_ID)
Demo_off$Mouse_ID <- as.character(Demo_off$Mouse_ID)
Mass_off$Mouse_ID <- as.character(Mass_off$Mouse_ID)
Maturation_off$Mouse_ID <- as.character(Maturation_off$Mouse_ID)
EndPara_off$Mouse_ID <- as.character(EndPara_off$Mouse_ID)

#Create a new dataframe with the demographic info from the dam sheet that is relevant for pups
Demo_dam_for_offspring <- Demo_dam %>%
  select(Dam_ID, Dam_cage, Treatment, Dam_Strain, Sire, DOB, Avg_litter_mass_P2, Litter_size_P2, Litter_size_P9)

#Add the demographic info, join by Dam_ID
Demo_off <- Demo_off %>%
  left_join(Demo_dam_for_offspring, by = "Dam_ID") %>%
  select(Mouse_ID:Dam_ID, DOB, Wean_Cage_Number:Sire, Avg_litter_mass_P2:Litter_size_P9)


#Combine all of the data into a single dataframe. Will add NAs where there isn't data
#can add ,by = ... to tell what column to join on
#not including dam info

LBN_data <- Demo_off %>%
  full_join(AcuteStress_off, by = "Mouse_ID") %>%
  full_join(ChronicStress_off, by = "Mouse_ID") %>%
  full_join(Mass_off, by = "Mouse_ID") %>%
  full_join(Maturation_off, by = "Mouse_ID") %>%
  full_join(EndPara_off, by = "Mouse_ID")

#If want to join only certain columns from one dataframe, use select() first

#Need to add some of the demographic data to the other data frames to be able to observe. Like DOB, treatment, esp