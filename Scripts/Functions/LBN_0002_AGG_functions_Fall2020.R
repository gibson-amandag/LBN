### Loading Data Sets-----------------------------------------------------------------------------------------
myXLSX_func = function(folderPath, fileName, sheetName){
  read.xlsx(
    file.path(folderPath, fileName),
    sheet = sheetName,
    colNames = TRUE,
    rowNames = FALSE,
    detectDates = TRUE,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    na.strings = "NA"
  )
}


#### Write to Excel Files ----------------------------------------------------------------------------------
#uses openxslx

writeToWorkbook = function(sheetName, df, wb, tableStyle = "TableStyleMedium2"){
  addWorksheet(wb, sheetName)
  writeDataTable(wb, sheetName, df, tableStyle = tableStyle)
}

greenFill <- function(){
  createStyle(bgFill = "#C6EFCE")
}

yellowFill <- function(){
  createStyle(bgFill = "#ffff00")
}

redFill <- function(){
  createStyle(bgFill = "#FFC7CE")
}

greyFill <- function(){
  createStyle(bgFill = 	"#D3D3D3")
}

blueFill <- function(){
  createStyle(bgFill = "#9edaff")
}

pinkFill <- function(){
  createStyle(bgFill = "#f3b4f6")
}



### Summarize Functions -----------------------------------------------------------------

#Provide string name for columns for both var_toSummarize and group_vars. Can group on multiple variables
#Example : LBN_summary_byGroup_quo("Mass_P21", Mass_off, c("Treatment", "Strain"))

LBN_summary_byGroup_quo = function(var_toSummarize, df, group_vars){
  var_name = LBN_varNames[, var_toSummarize] #get the long name
  df = df %>%
    filter(!is.na(!!! sym(var_toSummarize)))%>%
    group_by(!!!syms(group_vars)) %>% #this evaluates the string name to the symbols here
    summarize(Mean = mean(!!!syms(var_toSummarize), na.rm = TRUE),
              SD = sd(!!!syms(var_toSummarize), na.rm = TRUE),
              n = n(),
              SEM = SD/sqrt(n),
              .groups = 'drop') %>%
    mutate(Variable = var_toSummarize, #add columns to indicate what is summarized in each row
           VarName = var_name)
  return(df)
}


#Provide expr(var_toSummarize) and exprs(group_vars). Can group on multiple variables
# Example : LBN_summary_byGroup(expr(Mass_P21), Mass_off, exprs(Treatment, Strain))

LBN_summary_byGroup <- function(var_toSummarize, df, group_vars){
  var_toSumName <- as.character(var_toSummarize)
  var_name = LBN_varNames[, var_toSumName] #get the long name
  df = df %>%
    filter(!is.na(!! var_toSummarize))%>%
    group_by(!!! group_vars) %>% #this evaluates the string name to the symbols here
    summarize(Mean = mean(!! var_toSummarize, na.rm = TRUE),
              SD = sd(!! var_toSummarize, na.rm = TRUE),
              n = n(),
              SEM = SD/sqrt(n),
              .groups = 'drop') %>%
    mutate(Variable = var_toSumName, #add columns to indicate what is summarized in each row
           VarName = var_name)
  return(df)
}

# Use map_dfr which will combine all of the summaries into a single data frame by binding the rows. 
# Use a single set of grouping variables
# Example : map_dfr(exprs(Mass_P21, Mass_P22), LBN_summary_byGroup, Mass_off, exprs(Treatment, Strain))


### Group by Dam ------------------------------------------------------------------------
groupByDam <- function(df){
  df_byDam <- df %>%
    group_by(Dam_ID)
  return(df_byDam)
}

#Summarize by dam
AvgByDam_func <- function(df, demo_df = Demo_dam){
  AvgByDam <- df %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  AvgByDam <- demo_df %>%
    select(Dam_ID, Treatment, Dam_Strain, Strain, DOB) %>% #add back in the non-numeric demo information
    right_join(AvgByDam, by = "Dam_ID")
  return(AvgByDam)
}

#Evaluate both in one function
getAvgByDam <- function(df, demo_df = Demo_dam){
  grouped_df <- groupByDam(df)
  avg_df <- AvgByDam_func(grouped_df, demo_df)
  return(avg_df)
  
}
