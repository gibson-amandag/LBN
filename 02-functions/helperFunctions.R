

getSymAndString <- function(colVar){
  colSym <- ensym(colVar)
  colName <- as.character(colSym)
  return(
    list(
      "symbol" = colSym,
      "string" = colName
    )
  )
}

# demoUseSymAndString <- function(var){
#   colVar <- enquo(var)
#   getSymAndString(!! colVar)
# }
# # demoUseSymAndString(mouseID)

selectIfExists <- function(
  df,
  colVar
){
  var <- enquo(colVar)
  info <- getSymAndString(!! var)
  info$string
  df %>%
    select_if(
      info$string == names(.)
    )
}
# AcuteStress_off %>%
#   selectIfExists(
#     mouseID
#   )

selectAllIfExist <- function(
    df,
    colVars #c()
){
  df %>%
    select(
      any_of(colVars)
    )
}


#' Demonstrate one approach to getting the name of an unquoted expression
#' Don't use this function within another function, though, just use
#' as.character and substitute
#' makeExprCharacter
#'
#' @param var - an unquoted expression, such as a column name
#'
#' @return a string of the column name
#'
#' @examples makeExprCharacter(mouseID) -> "mouseID"
#' Note - this can't be used as is within another function
#' That takes a variable name as a parameter without being quoted
#' for example, testFunc <- function(df, colVar){
#' newString <- makeExprCharacter(colVar)
#' } doesn't work
#' 
#' Could also use all.names()
makeExprCharacter <- function (var){
  charString <- as.character(substitute(var))
  return(charString)
}


getValWhereOtherValTrue <- function(
    df
    , matchCol
    , matchVal
    , valCol
){
  matchColText <- as.character(matchCol)
  valColText <- as.character(valCol)
  val <- df[[valColText]][which(df[matchColText] == matchVal)]
  return(val)
}

getMouseInfoForSlicing <- function(df, thisMouseID){
  df %>%
    filter(
      mouseID == thisMouseID
    ) %>%
    selectAllIfExist(c(
      "mouseID"
      , "strain"
      , "damID"
      , "sire"
      , "weanCage"
      , "damCage"
      , "DOB"
      , "sex"
    ))
}

