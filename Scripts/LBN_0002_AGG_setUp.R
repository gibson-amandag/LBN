#### SET UP ENVIRONMENT, FOLDER PATHS, SOURCE FUNCTIONS ###########################################################################

readRenviron("./.Renviron") #read the .Renviron document from the project root folder

#Data File name
LBN_DataName <- Sys.getenv("EXCEL_FILE_NAME")

#Functions Script (ends in .R)
FunctionsFileName <- "LBN_0002_AGG_functions_Fall2020.R"

#import data function (ends in .R)
ImportDataFunctionsFileName <- "LBN_0002_AGG_importData.R"

#Variable Names Function and groups of variables (ends in .R)
VarNamesFunctionsFileName <- "LBN_0002_AGG_varNames.R"

#Task Functions Script (ends in .R)
TaskFunctionsFileName <- "LBN_0002_AGG_taskFunctions.R"

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
