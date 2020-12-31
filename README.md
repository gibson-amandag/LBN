# LBN_0002
LBN Cohort 2

<h1>Table of Contents</h1>
<ul>
    <li><a href="#Renviron">.Renviron</a></li>
    <li><a href="#setUp">LBN_0002_AGG_setUp.R</a></li>
    <li><a href="#functionsFall2020">LBN_0002_AGG_functions_Fall2020</a></li>
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

<h1><a id="functionsFall2020" name="functionsFall2020"></a>LBN_0002_AGG_functions_Fall2020.R</h1>

[Read about the functions file here](HelpDocs/LBN_0002_AGG_functions_Fall2020.md)



