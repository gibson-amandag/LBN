#### Load Libraries ##############################
library(tidyverse)
library(readr)
library(rlang)
library(purrr)
library(scales)
library(knitr)
#library(flextable) #error with data.table
library(officer)
library(GGally)
library(dplyr)
library(ggfortify)
library(openxlsx)
library(lubridate)
library(shiny)
library(ggrepel)
library(ggpubr)
library(rstatix)
library(cowplot)
library(extrafont)
# font_import()
if(! length(fonts()) > 0){
  # have to add fonts to be able to load them into pdfs
  # https://fromthebottomoftheheap.net/2013/09/09/preparing-figures-for-plos-one-with-r/
  loadfonts(dev="pdf")
}

#### SET UP ENVIRONMENT, FOLDER PATHS, SOURCE FUNCTIONS ###########################################################################

readRenviron("./.Renviron") #read the .Renviron document from the project root folder

#Data File name
LBN_DataName <- Sys.getenv("EXCEL_FILE_NAME")

#Test Cyles File Name - I'm not sure that this is actually it's only file now, AGG 12/31/2020
# Cycles_DataName <- Sys.getenv("CYCLES_FILE_NAME")

#Functions Script (ends in .R)
FunctionsFileName <- "LBN_0002_AGG_functions_Fall2020.R"

#import data function (ends in .R)
ImportDataFunctionsFileName <- "LBN_0002_AGG_importData.R"

#Variable Names Function and groups of variables (ends in .R)
VarNamesFunctionsFileName <- "LBN_0002_AGG_varNames.R"

#Task Functions Script (ends in .R)
TaskFunctionsFileName <- "LBN_0002_AGG_taskFunctions.R"

#Graph Functions Script (ends in .R)
GraphFunctionsFileName <- "LBN_0002_AGG_graphFunctions.R"

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

#Load the import data function
source(file.path(FunctionsFolder, ImportDataFunctionsFileName))

#Load the Variable Names Functions
source(file.path(FunctionsFolder, VarNamesFunctionsFileName))

#Load the Task Functions
source(file.path(FunctionsFolder, TaskFunctionsFileName))

#Load the Graph Functions
source(file.path(FunctionsFolder, GraphFunctionsFileName))
