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