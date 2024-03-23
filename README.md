<!-- omit in toc -->
AGG: General Igor Functions
=====================

**Author:** *Amanda Gibson*

**Updated:** *March 23, 2024*

* [Heading 1](#heading-1)
* [Heading 2](#heading-2)

# Renviron

This file defines the `DATA_FOLDER`, `OUTPUT_FOLDER`, and `EXCEL_FILE_NAME`, among other locations for the local system that is running the project. Use the [Renviron.example](./Renviron.example) file as a template

# Analysis

The files used for the manuscript are in the [manuscriptSubset](./manuscriptSubset/) folder.

## Main file

[AGG_LBN_manuscriptFigures_supplemental.Rmd](./manuscriptSubset/03-analysis-notebooks/AGG_LBN_manuscriptFigures_supplemental.Rmd) 

This file calls the other primary scripts to analyze the data, create figures for later editing in Illustrator, and outputs tables.

### Set-up

[01-set-up.R](./manuscriptSubset/01-scripts/01-set-up.R)

### Load datasets

[02-get-datasets.R](./manuscriptSubset/01-scripts/02-get-datasets.R)

### Filter datasets

[04-filter-datasets.R](./manuscriptSubset/01-scripts/04-filter-datasets.R)

### Run statistics

[06-run-LBN-stats.R](./manuscriptSubset/01-scripts/06-run-LBN-stats.R)

### Make plots 

[05-make-LBN-plots-modelError-manuscript.R](./manuscriptSubset/01-scripts/05-make-LBN-plots-modelError-manuscript.R)

- [05.5-run-LBN-masses-stats-plots.R](./manuscriptSubset/01-scripts/05.5-run-LBN-masses-stats-plots.R)
- [05.5-run-LBN-female-masses-stats-plots.R](./manuscriptSubset/01-scripts/05.5-run-LBN-female-masses-stats-plots.R)

### Make tables

[06-make-LBN-tables.R](./manuscriptSubset/01-scripts/06-make-LBN-tables.R)

- [06-make-LBN-table-pairs.R](/manuscriptSubset/01-scripts/06-make-LBN-table-pairs.R)

### GABA PSC distribution analysis

[05.5-run-GABA-dist-analysis.R](./manuscriptSubset//01-scripts/05.5-run-GABA-dist-analysis.R)

This file includes bootstrapping analysis. Due to the length of time to re-run, separated from other scripts.

# app.R

[Read about the Shiny app here](HelpDocs/appR.md) * note, not up-to-date. For questions, contact Amanda. 

Includes generic tabs for estrous cycle image viewing and corticosterone EIA analysis.

The `app.R` file is a Shiny App that allows for data visualization. It uses the same core functions as the Rmd analysis file described here. 

For the app, ui (user interface) and server functions are defined. I have modularized the code so that chunks for different panels are defined in separate R scripts, each again defining a ui component and a server component. 

# Functions

[02-functions](./manuscriptSubset/02-functions/) folder