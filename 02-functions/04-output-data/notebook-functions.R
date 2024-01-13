myDisplay <- function(df, docType = doc.type){
  rownames(df) <- gsub("trt","treatment", rownames(df)) # replace 'trt' with 'treatment' in the row names of dataframe 
  if(!is.null(docType) && docType == "docx"){
    return(flextable(df))
  } else {
    return(df)
  }
}

subTrtInRowNames <- function(tbl, sub = "early-life treatment") {
  row.names(tbl) <- gsub("early-life trt", sub, row.names(tbl))
  return(tbl)
}

subEarlyLifeTrtInRowNames <- function(tbl, sub = "early-life treatment") {
  row.names(tbl) <- gsub("earlyLifeTrt", sub, row.names(tbl))
  return(tbl)
}

subAdultTrtInRowNames <- function(tbl) {
  row.names(tbl) <- gsub("adultTrt", "adult treatment", row.names(tbl))
  return(tbl)
}

subnParColNames <- function(tbl) {
  if("Statistic" %in% colnames(tbl)) {
    colnames(tbl)[colnames(tbl) == 'Statistic'] <- 'F'
  }
  if("p-value" %in% colnames(tbl)) {
    colnames(tbl)[colnames(tbl) == 'p-value'] <- 'p'
  }
  return(tbl)
}

formatPCol <- function(df){
  df <- df %>%
    mutate(
      p = ifelse(p < 0.001, "<0.001", format(round(p, 3), nsmall = 3))
    )
  return(df)
}

addTableLegend <- function(doc, tableNum, tableLegend){
  fp_bold <- fp_text(font.size = 11, bold = TRUE)
  fp_normal <- fp_text(font.size = 11, bold = FALSE)
  
  fpar_element <- fpar(ftext(paste0("Table ", tableNum, ". "), prop = fp_bold),
                       ftext(tableLegend, prop = fp_normal))
  
  doc <- body_add_fpar(doc, fpar_element, style = "Normal")
  
  return(doc)
}

simplifyLMMOutput <- function(anovaTable){
  tbl <- anovaTable %>%
    subEarlyLifeTrtInRowNames() %>%
    subAdultTrtInRowNames() %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    mutate(
      `F` = format(round(`F`, 2), nsmall = 2)
      , `df` = paste0(round(`num Df`), ", ", format(round(`den Df`, 1), nsmall = 1))
      , .after = `F`
    ) %>%
    rename(
      p = `Pr(>F)`
    ) %>%
    formatPCol() %>%
    select(
      -c(`num Df`, `den Df`)
    )
  return(tbl)
}

simplifyEMMOutput <- function(emmTbl){
  tbl <- emmTbl %>%
    rename(
      SEM = SE
    ) %>%
    mutate(
      `95% CI` = paste0("[", format(round(lower.CL, 2), nsmall = 2), ", ", format(round(upper.CL, 2), nsmall = 2), "]")
      , level = as.character(level)
    ) %>%
    select(
      -c(lower.CL, upper.CL)
    )
  return(tbl)
}

simplifyEMMPairsOutput <- function(pairsTbl){
  tbl <- pairsTbl %>%
    as_tibble() %>%
    rename(
      SEM = SE
      , `t ratio` = t.ratio
      , p = p.value
    ) %>%
    formatPCol()
  return(tbl)
}

addTableAndCaptionToDoc <- function(tbl, tableCaption, tableNum, doc){
  doc <- doc %>%
    addTableLegend(tableNum, tableCaption) %>%
    body_add_flextable(tbl) %>%
    body_add_par("")
  return(doc)
}
