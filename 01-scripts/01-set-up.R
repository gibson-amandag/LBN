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

