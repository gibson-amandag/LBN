# LBN_0002
LBN Cohort 2

<h1>Table of Contents</h1>
<ul>
    <li><a href="#Renviron">.Renviron</a></li>
    <li><a href="#setUp">LBN_0002_AGG_setUp.R</a></li>
    <li><a href="#app">app.R</a></li>
    <li><a href="#functionsFall2020">LBN_0002_AGG_functions_Fall2020.R</a></li>
    <li><a href="#importData">LBN_0002_AGG_importData.R</a></li>
    <li><a href="#varNames">LBN_0002_AGG_varNames.R</a></li>
    <li><a href="#graphFunctions">LBN_0002_AGG_graphFunctions.R</a></li>
    <li><a href="#taskFunctions">LBN_0002_AGG_taskFunctions.R</a></li>
</ul>

<h1><a id="Renviron" name="Renviron"></a>.Renviron</h1>

<p>This file defines the <code>DATA_FOLDER</code>, <code>OUTPUT_FOLDER</code>, and <code>EXCEL_FILE_NAME</code> for the local system that is running the project</p>

<h1><a id="setUp" name="setUp"></a>LBN_0002_AGG_setUp.R</h1>

<p>
    <code>setUp</code> reads the <code>.Renviron</code> file, and then gets the <code>LBN_DataName</code>, <code>DataFolder</code>, and <code>OutputFolder</code> from the system environment (<code>Sys.getenv()</code>). 
    This file also defines the:
</p>
<ul>
    <li><code>FunctionsFileName</code></li>
    <li><code>ImportDataFunctionsFileName</code></li>
    <li><code>VarNamesFunctionFileName</code></li>
    <li><code>TaskFunctionsFileName</code></li>
    <li><code>GraphFunctionsFileName</code></li>
    <li><code>ScriptsFolder</code></li>
    <li><code>FunctionsFolder</code></li>
    <li><code>PlotFolder</code></li>
    <li><code>DataOutFolder</code></li>
</ul>

<p>The file then sources the following function files:</p>

<ul>
    <li><code>LBN_0002_AGG_functions_Fall2020.R</code></li>
    <li><code>LBN_0002_AGG_importData.R</code></li>
    <li><code>LBN_0002_AGG_varNames.R</code></li>
    <li><code>LBN_0002_AGG_taskFunctions</code></li>
    <li><code>LBN_0002_AGG_graphFunctions.R</code></li>
</ul>

<h1><a id="app" name="app"></a>app.R</h1>

[Read about the Shiny app here](HelpDocs/appR.md)

The `app.R` file is a Shiny App that allows for data visualization. It uses the same core functions as the Rmd analysis file described here. 

For the app, ui (user interface) and server functions are defined. I have modularized the code so that chunks for different panels are defined in separate R scripts, each again defining a ui component and a server component. 

<h1><a id="functionsFall2020" name="functionsFall2020"></a>LBN_0002_AGG_functions_Fall2020.R</h1>

[Read about the functions file here](HelpDocs/LBN_0002_AGG_functions_Fall2020.md)

This file contains functions for the following tasks
* Loading data sets
* Writing to excel files
* Summarizing data frames
* Grouping dataframes by dam ID

<h1><a id="importData" name="importData"></a>LBN_0002_AGG_importData.R</h1>

[Read about the importData file here](HelpDocs/LBN_0002_AGG_importData.md)

This file contains a `load_LBN_data` function that takes the `dataFolder`, `excelName`, and sheet names as parameters. After generating and processing dataframes from each sheet, it generates a list of dataframes as the output.

<h1><a id="varNames" name="varNames"></a>LBN_0002_AGG_varNames.R</h1>

[Read about the varNames file here](HelpDocs/LBN_0002_AGG_varNames.md)

This file contains a `LBN_varNames_func` function that takes the `LBN_data` dataframe as a parameter and then creates a new dataframe that contains nicer strings of variable names to be used in plots, for example. The column names remain the same as the original dataframe, making it easy to reference the appropriate "pretty" string.

This file also contains functions to create lists of different groupings of variable names, which can then be used to construct other dataframes, or to run functions with certain groups of variables.

<h1><a id="graphFunctions" name="graphFunctions"></a>LBN_0002_AGG_graphFunctions.R</h1>

[Read about the graphFunctions file here](HelpDocs/LBN_0002_AGG_graphFunctions.md)

This file contains the series of functions that are used to create plots in R. It also defines `my_theme` for plots. Many of the plots are constructed using `geoms` which are layers that are combined to construct the plots. The file also contains helper functions to reorganize the dataframes when necessary for plotting.

<h1><a id="taskFunctions" name="taskFunctions"></a>LBN_0002_AGG_taskFunctions.R</h1>

[Read about the taskFunctions file here](HelpDocs/LBN_0002_AGG_taskFunctions.md)

This file contains the functions that are used to output which tasks need to be completed on certain days, such as taking the mass of mice or starting the treatment paradigm. 
