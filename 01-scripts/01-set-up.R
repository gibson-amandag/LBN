if (!require(tidyverse)) install.packages('tidyverse')
if (!require(rlang)) install.packages('rlang')
if (!require(scales)) install.packages('scales') # with tidyverse?
if (!require(knitr)) install.packages('knitr')
if (!require(officer)) install.packages('officer')
if (!require(GGally)) install.packages('GGally')
if (!require(ggfortify)) install.packages('ggfortify')
if (!require(openxlsx)) install.packages('openxlsx')
if (!require(lubridate)) install.packages('lubridate') # with tidyverse?
if (!require(shiny)) install.packages('shiny')
if (!require(ggrepel)) install.packages('ggrepel')
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(rstatix)) install.packages('rstatix')
if (!require(cowplot)) install.packages('cowplot')
if (!require(extrafont)) install.packages('extrafont')
if (!require(flextable)) install.packages('flextable')
if(!require(remotes)) install.packages('remotes')
if(!require(fs)) install.packages('fs') # with tidyverse?
if(!require(DT)) install.packages('DT')
if(!require(Cairo)) install.packages('Cairo')
if (!require(MASS)) install.packages('MASS')
if (!require(drc)) install.packages('drc')
if(!require(shinyjs)) install.packages('shinyjs')
if(!require(colourpicker)) install.packages('colourpicker')
if(!require(plater)) install.packages('plater')
# if(!require(shinyFiles)) install.packages('shinyFiles')
if(!require(tinytex))install.packages('tinytex')

#### Load Libraries ##############################
library(MASS)
library(drc)
library(tidyverse)
library(rlang)
library(scales)
library(knitr)
library(officer)
library(GGally)
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
library(DT)
library(Cairo)
library(shinyjs)
library(colourpicker) # After shinyjs
library(plater)
library(tinytex)
select <- dplyr::select
# Save a PDF - in ggsave, device = cairo_pdf on Windows
# https://r-graphics.org/recipe-output-fonts-pdf - even when following these
# steps (downloaded Ghostscript, added to environment, embedded fonts), the
# pdf text is still overlapping on windows


## 2021-08-17 - had to install older version of Rttf2pt1 for the font_import from extrafont to work appropriately
## https://github.com/wch/extrafont/issues/88
## Download Rtools for Windows: https://cran.r-project.org/bin/windows/Rtools/

## Run these lines once on the computer
# remotes::install_version("Rttf2pt1", version = "1.3.8")
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

#Current Computer Type
currentCompType <- Sys.getenv("COMP_TYPE")

#Data File name
# LBN_DataName <- Sys.getenv("EXCEL_FILE_NAME")
LBN_DataName <- "LBN_AGG_data.xlsx"

#Where data files are saved
dataFolder <- normalizePath(Sys.getenv("DATA_FOLDER"))

LBN_0004_CyclingFolder <- normalizePath(Sys.getenv("LBN_0004_CYCLING_FOLDER"))
LBN_0006_CyclingFolder <- normalizePath(Sys.getenv("LBN_0006_CYCLING_FOLDER"))

LBN_ServerFolder <- normalizePath(Sys.getenv("LBN_SERVER_FOLDER"))
LBN_uterinePicsFolder <- normalizePath(file.path(LBN_ServerFolder, "uterinePics"))

#Where output should be saved
outputFolder <- normalizePath(Sys.getenv("OUTPUT_FOLDER"))

dataOutputFolder <- normalizePath(file.path(outputFolder, "data"))
plotOutputFolder <- normalizePath(file.path(outputFolder, "plots"))
reportOutputFolder <- normalizePath(file.path(outputFolder, "reports"))

#Where R script files are saved
scriptsFolder <- normalizePath(file.path("01-scripts"))
appScriptsFolder <- normalizePath(file.path(scriptsFolder, "appScripts"))

#Where Function files are saved
functionsFolder <- normalizePath(file.path("02-functions"))

functionFiles <- list.files(
  functionsFolder, 
  full.names = TRUE,
  recursive = TRUE, 
  pattern = "*.R"
)

sapply(functionFiles, source)

