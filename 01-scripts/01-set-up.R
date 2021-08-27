if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readr)) install.packages('readr')
if (!require(purrr)) install.packages('purrr')
if (!require(rlang)) install.packages('rlang')
if (!require(scales)) install.packages('scales')
if (!require(knitr)) install.packages('knitr')
if (!require(officer)) install.packages('officer')
if (!require(GGally)) install.packages('GGally')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggfortify)) install.packages('ggfortify')
if (!require(openxlsx)) install.packages('openxlsx')
if (!require(lubridate)) install.packages('lubridate')
if (!require(shiny)) install.packages('shiny')
if (!require(ggrepel)) install.packages('ggrepel')
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(rstatix)) install.packages('rstatix')
if (!require(cowplot)) install.packages('cowplot')
if (!require(extrafont)) install.packages('extrafont')
if (!require(flextable)) install.packages('flextable')
if(!require(remotes)) install.packages('remotes')
if(!require(fs)) install.packages('fs')

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
library(flextable)
library(fs)
## 2021-08-17 - had to install older version of Rttf2pt1 for the font_import from extrafont to work appropriately
## https://github.com/wch/extrafont/issues/88
#remotes::install_version("Rttf2pt1", version = "1.3.8")
# font_import()
if(! length(fonts()) > 0){
  # have to add fonts to be able to load them into pdfs
  # https://fromthebottomoftheheap.net/2013/09/09/preparing-figures-for-plos-one-with-r/
  print("Loading fonts")
  loadfonts(dev="pdf")
}



# READ ENVIRONMENT --------------------------------------------------------

readRenviron("./.Renviron") #read the .Renviron document from the project root folder
dateToday <- Sys.Date()

# DEFINE OUTPUT OPTIONS ---------------------------------------------------

# This gets prepended to file names when saving
filePrefix <- Sys.getenv("FILE_PREFIX")

# type of images to save for plots. 
# .png, .svg, or .pdf, for example
imgTypePlots <- Sys.getenv("PLOT_OUTPUT_FORMAT")

# save individual plot files?
savePlots <- Sys.getenv("SAVE_PLOTS")

#Data File name
# LBN_DataName <- Sys.getenv("EXCEL_FILE_NAME")
LBN_DataName <- "LBN_AGG_data.xlsx"

#Where data files are saved
dataFolder <- Sys.getenv("DATA_FOLDER")

LBN_0004_CyclingFolder <- Sys.getenv("LBN_0004_CYCLING_FOLDER")
LBN_0006_CyclingFolder <- Sys.getenv("LBN_0006_CYCLING_FOLDER")

LBN_ServerFolder <- Sys.getenv("LBN_SERVER_FOLDER")
LBN_uterinePicsFolder <- file.path(LBN_ServerFolder, "uterinePics")

#Where output should be saved
outputFolder <- Sys.getenv("OUTPUT_FOLDER")

dataOutputFolder <- file.path(outputFolder, "data")
plotOutputFolder <- file.path(outputFolder, "plots")
reportOutputFolder <- file.path(outputFolder, "reports")

#Where R script files are saved
scriptsFolder <- file.path("01-scripts")

#Where Function files are saved
functionsFolder <- file.path("02-functions")

functionFiles <- list.files(
  functionsFolder, 
  full.names = TRUE,
  recursive = TRUE, 
  pattern = "*.R"
)

sapply(functionFiles, source)

