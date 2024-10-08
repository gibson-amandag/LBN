if (!require(tidyverse)) install.packages('tidyverse')
if (!require(clock)) install.packages('clock')
if (!require(rlang)) install.packages('rlang')
if (!require(scales)) install.packages('scales') # with tidyverse?
if (!require(knitr)) install.packages('knitr')
if (!require(officer)) install.packages('officer')
if (!require(GGally)) install.packages('GGally')
if (!require(ggfortify)) install.packages('ggfortify')
if (!require(openxlsx)) install.packages('openxlsx')
if (!require(lubridate)) install.packages('lubridate') # with tidyverse?
if(!require(DescTools))install.packages('DescTools')
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
if(!require(lemon))install.packages('lemon')
if(!require(rvg))install.packages('rvg')
if(!require(googledrive))install.packages('googledrive')
if(!require(googlesheets4))install.packages('googlesheets4')
if(!require(lme4))install.packages('lme4') 
if(!require(lqmm))install.packages('lqmm') 
# 2024-01-11 install.packages("lme4", type = "source")
# Had problems with compatibility with new Matrix
# https://stackoverflow.com/questions/77481539/error-in-initializeptr-function-cholmod-factor-ldeta-not-provided-by-pack
if(!require(lmerTest))install.packages('lmerTest')
if(!require(afex))install.packages('afex')
if(!require(emmeans))install.packages('emmeans')
if(!require(ggbeeswarm))install.packages('ggbeeswarm')
if(!require(nparLD))install.packages('nparLD')
if(!require(lspline))install.packages('lspline')
if(!require(kSamples))install.packages('kSamples')
if(!require(viridis))install.packages('viridis')
# if(!require(gt))install.packages('gt')



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
library(DescTools)
library(shiny)
library(ggrepel)
library(ggpubr)
library(rstatix)
library(cowplot)
library(extrafont)
library(fs)
library(DT)
library(Cairo)
library(shinyjs)
library(colourpicker) # After shinyjs
library(plater)
library(tinytex)
library(clock)
library(lemon)
library(rvg)
library(googledrive)
library(googlesheets4)
library(lme4)
library(lmerTest)
library(afex)
library(emmeans)
library(ggbeeswarm)
library(nparLD)
library(lspline)
library(kSamples)
library(viridis)

drive_auth(email = "*@umich.edu")
gs4_auth(token = drive_token())

select <- dplyr::select
# Save a PDF - in ggsave, device = cairo_pdf on Windows
# https://r-graphics.org/recipe-output-fonts-pdf - even when following these
# steps (downloaded Ghostscript, added to environment, embedded fonts), the
# pdf text is still overlapping on windows


# ## 2021-08-17 - had to install older version of Rttf2pt1 for the font_import from extrafont to work appropriately
# ## https://github.com/wch/extrafont/issues/88
# ## Download Rtools for Windows: https://cran.r-project.org/bin/windows/Rtools/
# 
# ## Run these lines once on the computer
# remotes::install_version("Rttf2pt1", version = "1.3.8") # 2024-01-11: Don't use

# 2024-01-11: New error about corrupt version when using that version. Installed just the normal
# install.packages("Rttf2pt1")
# font_import()

if(! length(fonts()) > 0){
  # have to add fonts to be able to load them into pdfs
  # https://fromthebottomoftheheap.net/2013/09/09/preparing-figures-for-plos-one-with-r/
  print("Loading fonts")
  loadfonts(dev="pdf")
}

library(flextable) # 2024-01-11, maybe some problems if loading before fonts are loaded

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
LBN_DataName <- Sys.getenv("EXCEL_FILE_NAME")
# LBN_DataName <- "LBN_AGG_data.xlsx"

#Where data files are saved
dataFolder <- normalizePath(Sys.getenv("DATA_FOLDER"))

LBN_0004_CyclingFolder <- normalizePath(Sys.getenv("LBN_0004_CYCLING_FOLDER"))
LBN_0006_CyclingFolder <- normalizePath(Sys.getenv("LBN_0006_CYCLING_FOLDER"))

LBN_ServerFolder <- normalizePath(Sys.getenv("LBN_SERVER_FOLDER"))
LBN_uterinePicsFolder <- normalizePath(file.path(LBN_ServerFolder, "uterinePics"))

summer2024_ServerFolder <- normalizePath(Sys.getenv("SUMMER2024_SERVER_FOLDER"))
#Where output should be saved
outputFolder <- normalizePath(Sys.getenv("OUTPUT_FOLDER"))

dataOutputFolder <- normalizePath(file.path(outputFolder, "data"))
plotOutputFolder <- normalizePath(file.path(outputFolder, "plots"))
reportOutputFolder <- normalizePath(file.path(outputFolder, "reports"))


#Where cort admin files are saved
cortAdminFolder <- normalizePath(Sys.getenv("CORT_ADMIN_FOLDER"))
cortAdminFileName <- Sys.getenv("CORT_ADMIN_FILE_NAME")

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

# Dam images folder
damImgsFolder <- normalizePath(Sys.getenv("DAM_IMG_FOLDER"))

