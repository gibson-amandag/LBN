

anovaComboTrtFunc <- function(
  dvCol #expr()
  , fontSize = 11
){
  anovaFunc <- function(df){
    anova <- df %>%
      filter(
        !is.na(!! dvCol )
        , !is.na(earlyLifeTrt)
        , !is.na(adultTrt)
      ) %>%
      anova_test(
        dv = {{ dvCol }}
        , wid = mouseID
        , between = c(earlyLifeTrt, adultTrt)
        
      )
    flxTbl <- formatAnova(anova, fontSize = fontSize)
    return(flxTbl)
  }
  return(anovaFunc)
}