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

<h2>Loading Data Sets</h2>
<h3><code>myXLSX_func(folderPath, fileName, sheetName)</code></h3>
<p>This function reads in the appropriate sheet of an excel file into an R dataframe. It uses the column names, and detects dates. It skips empty rows and columns. It uses "NA" for the empty data string</p>

<h2>Write to Excel Files</h2>
<h3><code>writeToWorkbook(sheetName, df, wb, tableStyle)</code></h3>
<p>
    This function writes a dataframe <code>df</code> to an existing workbook <code>wb</code>. It adds this dataframe to a new sheet <code>sheetName</code> and uses the defined <code>tableStyle</code>. This uses the <code>openxlsx</code> package.
</p>
<p>The following fill functions create a style with the specified background color for use with creating conditional formatting of the Excel sheets</p>
<ul>
        <li>
            <h3>
                <code>greenFill()</code>
            </h3>
        </li>
        <li>
            <h3>
                <code>yellowFill()</code>
            </h3>
        </li>
        <li>
            <h3>
                <code>redFill()</code>
            </h3>
        </li>
        <li>
            <h3>
                <code>greyFill()</code>
            </h3>
        </li>
        <li>
            <h3>
                <code>blueFill()</code>
            </h3>
        </li>
        <li>
            <h3>
                <code>pinkFill()</code>
            </h3>
        </li>
</ul>


<h2>Summarize Functions</h2>
<h3><code>LBN_summary_byGroup_quo(var_toSummarize, df, group_vars)</code></h3>
<p>
    This function allow you to group a dataframe <code>df</code> by a grouping variable <code>group_vars</code>, which can be a multiple variables provided in a <code>c()</code> list. The <code>var_toSummarize</code> is then summarized for each of the different specified group. This provides the mean, standard deviation, number, and SEM for the variable.
</p>
<p>
    For this function, both the <code>var_toSummarize</code> and the <code>group_vars</code> should be provided as strings. For example: <code>LBN_summary_byGroup_quo("Mass_P21", Mass_off, c("Treatment", "Strain"))</code>.
</p>
<h3><code>LBN_sumamry_byGroup(var_toSummarize, df, group_vars)</code></h3>
<p>
    This function is similar to <code>LBN_summary_byGroup_quo</code>except the the <code>var_toSummarize</code> and <code>group_vars</code> should be provided as expressions. For example, <code>LBN_summary_byGroup(expr(Mass_P21), Mass_off, exprs(Treatment, Strain))</code>.
</p> 
<p>
    When calling these functions, my recommendations was to use <code>map_dfr</code> which will combine all of the summaries into a single dataframe by binding the rows. When calling this, you must use a single set of grouping variables. For example, <code>map_dfr(exprs(Mass_P21, Mass_P22), LBN_summary_byGroup, Mass_off, exprs(Treatment, Strain))</code>
</p>

<h2>Group by Dam</h2>
<h3><code>groupByDam(df)</code></h3>
<p>
    This function takes a dataframe <code>df</code> and groups it by the <code>Dam_ID</code> column.
</p>
<h3><code>AvgByDam_func(df, demo_df)</code></h3>
<p>
    This function takes a dataframe <code>df</code> and summarizes only the numeric columns. Then, the <code>demo_df</code> (default is <code>Demo_dam</code>) dataframe is used to select the <code>DamID, Treatment, Dam_Strain, DOB, ParaType</code> columns to add back in the non-numeric demographic information and joins this to the dataframe that was averaged. 
</p>
<p>This function and <code>groupByDam</code> are both called in the third function, <code>getAvgByDam</code> so that the dataframe is grouped before this function is called.</p>
<h3><code>getAvgByDam(df, demo_df)</code></h3>
<p>This function first groups a dataframe by dam and then gets the average of numerical variables for each dam, using the two previous functions</p>

