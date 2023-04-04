writeToWorkbook = function(sheetName, df, wb, tableStyle = "TableStyleMedium2"){
  addWorksheet(wb, sheetName)
  writeDataTable(wb, sheetName, df, tableStyle = tableStyle)
}

saveDFsToExcel <- function(
  fileBaseName,
  prefix = filePrefix,
  addDate = Sys.Date(),
  saveFolder = dataOutputFolder,
  ... # use sheetName = df, sheetName2 = df2, sheetName3 = df3 for each df you want to add to a new sheet
){
  dfList <- list(...)
  
  wb <- createWorkbook()
  
  for(sheetName in names(dfList)){
    writeToWorkbook(sheetName, dfList[[sheetName]], wb)
  }
  
  fileName <- paste0(prefix, fileBaseName, "_", addDate, ".xlsx")
  filePath <- file.path(saveFolder, fileName)
  print(filePath)
  
  saveWorkbook(wb, filePath, overwrite = TRUE)
}

saveDFsToCSV <- function(
  prefix = filePrefix,
  addDate = Sys.Date(),
  saveFolder = dataOutputFolder,
  ... # use sheetName = df, sheetName2 = df2, sheetName3 = df3 for each df you want to add to a new sheet
){
  dfList <- list(...)
  
  for(sheetName in names(dfList)){
    fileName <- paste0(prefix, sheetName, ".csv")
    write.csv(dfList[[sheetName]], file.path(saveFolder, fileName), row.names=FALSE)
  }
}

saveDFsToExcel_shiny <- function(
  filePath,
  ... # use sheetName = df, sheetName2 = df2, sheetName3 = df3 for each df you want to add to a new sheet
){
  dfList <- list(...)
  
  wb <- createWorkbook()
  
  for(sheetName in names(dfList)){
    writeToWorkbook(sheetName, dfList[[sheetName]], wb)
  }
  
  saveWorkbook(wb, filePath, overwrite = TRUE)
}

saveUseInfoDFsToExcel <- function(
    fileBaseName,
    prefix = filePrefix,
    addDate = dateToday,
    saveFolder = dataOutputFolder,
    ...
){
  dfList <- list(...)
  
  wb <- createWorkbook()
  
  for(sheetName in names(dfList)){
    writeToWorkbook(
      sheetName, 
      dfList[[sheetName]], 
      wb,
      tableStyle = "TableStyleLight8"
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="earlyLifeTrt"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "LBN",
      type = "contains",
      style = LBNFill
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultTrt"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "ALPS",
      type = "contains",
      style = ALPSFill
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultTrt"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "CON",
      type = "contains",
      style = CONFill
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="Sac_cycle"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "diestrus",
      type = "contains",
      style = diestrusFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="Sac_cycle"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "proestrus",
      type = "contains",
      style = proestrusFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultDi"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="moved"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultProLH"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultProEphys"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="hitCycle"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="litterNum"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "1",
      type = "contains",
      style = litter1Fill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="litterNum"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "2",
      type = "contains",
      style = litter2Fill
    )
  }
  
  fileName <- paste0(prefix, fileBaseName, "_", addDate, ".xlsx")
  filePath <- file.path(saveFolder, fileName)
  print(filePath)
  
  
  
  saveWorkbook(wb, filePath, overwrite = TRUE)
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

LBNFill <- createStyle(bgFill = "#33CCCC")

ALPSFill <- createStyle(bgFill = "#000000", fontColour = "#FFFFFF")

CONFill <- createStyle(bgFill = "#D9D9D9")

diestrusFill <- createStyle(bgFill = "#DCE6F1")

proestrusFill <- createStyle(bgFill = "#E4DFEC")

trueFill <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")

litter1Fill <- createStyle(bgFill = "#CCECFF")
litter2Fill <- createStyle(bgFill = "#FFCCFF")
