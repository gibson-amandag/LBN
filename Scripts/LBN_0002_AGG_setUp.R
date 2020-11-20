#### SET UP ENVIRONMENT, FOLDER PATHS, SOURCE FUNCTIONS ###########################################################################

readRenviron("./.Renviron") #read the .Renviron document from the project root folder

#Functions Script (ends in .R)
FunctionsFileName <- "LBN_0002_AGG_functions_Fall2020.R"

#import data function (ends in .R)
ImportDataFunctionsFileName <- "LBN_0002_AGG_importData.R"

#Task Functions Script (ends in .R)
TaskFunctionsFileName <- "LBN_0002_AGG_taskFunctions.R"

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

#Load the import data function
source(file.path(FunctionsFolder, ImportDataFunctionsFileName))

#Load the Task Functions
source(file.path(FunctionsFolder, TaskFunctionsFileName))
