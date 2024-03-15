

anovaComboTrtFunc <- function(
  dvCol #expr()
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
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
    flxTbl <- formatAnova(anova, fontSize = fontSize, addWVal = addWVal, addHVal = addHVal)
    return(list(
      anova = anova %>% get_anova_table() %>% # added 2021-10-23
        as_tibble() # replaced as_data_frame()
      , flxTbl = flxTbl
      )
    )
  }
  return(anovaFunc)
}

anovaTrtLitterFunc <- function(
  dvCol #expr()
  , idVar = expr(mouseID)
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
){
  anovaFunc <- function(df){
    anova <- df %>%
      filter(
        !is.na(!! dvCol )
        , !is.na(earlyLifeTrt)
        , !is.na(litterNum)
      ) %>%
      anova_test(
        dv = {{ dvCol }}
        , wid = {{ idVar }}
        , between = c(earlyLifeTrt, litterNum)
        
      )
    flxTbl <- formatAnova(anova, fontSize = fontSize, addWVal = addWVal, addHVal = addHVal)
    return(list(
      anova = anova %>% get_anova_table() %>% # added 2021-10-23
        as_tibble() # replaced as_data_frame()
      , flxTbl = flxTbl
      )
    )
  }
  return(anovaFunc)
}

anovaTrtFunc <- function(
  dvCol #expr()
  , idVar = expr(mouseID)
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
){
  anovaFunc <- function(df){
    anova <- df %>%
      filter(
        !is.na(!! dvCol )
        , !is.na(earlyLifeTrt)
      ) %>%
      anova_test(
        dv = {{ dvCol }}
        , wid = {{ idVar }}
        , between = c(earlyLifeTrt)
        
      )
    flxTbl <- formatAnova(anova, fontSize = fontSize, addWVal = addWVal, addHVal = addHVal)
    return(list(
      anova = anova %>% get_anova_table() %>% # added 2021-10-23
        as_tibble() # replaced as_data_frame()
      , flxTbl = flxTbl
      )
    )
  }
  return(anovaFunc)
}

anovaDayTrtLitterFunc <- function(
  dvCol #expr()
  , idVar = expr(mouseID)
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
){
  anovaFunc <- function(df){
    anova <- df %>%
      filter(
        !is.na(!! dvCol )
        , !is.na(earlyLifeTrt)
        , !is.na(litterNum)
      ) %>%
      anova_test(
        dv = {{ dvCol }}
        , wid = {{ idVar }}
        , between = c(earlyLifeTrt, litterNum)
        , within = day
        
      )
    flxTbl <- formatAnova(anova, fontSize = fontSize, addWVal = addWVal, addHVal = addHVal)
    return(list(
      anova = anova %>% get_anova_table() %>% # added 2021-10-23
        as_tibble() # replaced as_data_frame()
      , flxTbl = flxTbl
      )
    )
  }
  return(anovaFunc)
}
anovaDayTrtFunc <- function(
  dvCol #expr()
  , idVar = expr(mouseID)
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
){
  anovaFunc <- function(df){
    anova <- df %>%
      filter(
        !is.na(!! dvCol )
        , !is.na(earlyLifeTrt)
      ) %>%
      anova_test(
        dv = {{ dvCol }}
        , wid = {{ idVar }}
        , between = c(earlyLifeTrt)
        , within = day
        
      )
    flxTbl <- formatAnova(anova, fontSize = fontSize, addWVal = addWVal, addHVal = addHVal)
    return(list(
      anova = anova %>% get_anova_table() %>% # added 2021-10-23
        as_tibble() # replaced as_data_frame()
      , flxTbl = flxTbl
      )
    )
  }
  return(anovaFunc)
}

#' Format LBN x ALPS stress anova output
#' 
#' takes the rstatix::anova_test() output and makes it a dataframe, then mutates to make the following changes:
#' If p is less than 0.001, it is reported as "<0.001"
#' Makes it a flextable
#' Bolds rows where p is < 0.05
#' Font size is 11
#'
#' @param anovaDF 
#'
#' @return
#' @export
#'
#' @examples
formatAnova <- function(
    anovaDF,
    fontSize = 11,
    addWVal = 0.1,
    addHVal = 0.1
){
  flxTbl <- anovaDF %>%
    get_anova_table() %>% # added 2021-10-23
    as_tibble() %>% # replaced as_data_frame()
    mutate(
      p = case_when(
        p < 0.001 ~ as.character("<0.001"),
        TRUE ~ as.character(p)
        # ),
        # Effect = case_when(
        #   Effect == "earlyLifeTrt" ~ "early life trt",
        #   Effect == "adultTrt" ~ "adult trt",
        #   Effect == "earlyLifeTrt:adultTrt" ~ "early life x adult trt",
        #   Effect == "earlyLifeTrt:time" ~ "early life trt x time",
        #   Effect == "adultTrt:time" ~ "adult trt x time",
        #   Effect == "earlyLifeTrt:adultTrt:time" ~ "early life x adult trt x time",
        #   TRUE ~ Effect
      )
    ) %>%
    flextable() %>%
    bold(
      i = ~ `p<.05` == "*"
    ) %>%
    colformat_double(digits = 3) %>%
    fontsize(
      size = fontSize
    ) %>%
    autofit(add_w = addWVal, add_h = addHVal)
  
  return(flxTbl)
}

formatAdjAnova <- function(
    anovaDF
    , fontSize = 11
    , addWVal = 0.1
    , addHVal = 0.1
){
  flxTbl <- anovaDF %>%
    get_anova_table() %>%
    as_tibble() %>% # replaced as_data_frame()
    mutate(
      p = case_when(
        p < 0.001 ~ as.character("<0.001"),
        TRUE ~ as.character(p)
      ),
      'p.adj<.05' = case_when(
        p.adj < 0.05 ~ "*",
        TRUE ~ ""
      ),
      p.adj = case_when(
        p.adj < 0.001 ~ as.character("<0.001"),
        p.adj > 0.999 ~ as.character(">0.999"),
        TRUE ~ as.character(round(p.adj, 3))
      )
    ) %>%
    select(-`p<.05`) %>%
    flextable() %>%
    bold(
      i = ~ `p.adj<.05` == "*"
    ) %>%
    fontsize(
      size = fontSize
    )%>%
    autofit(add_w = addWVal, add_h = addHVal)
  return(flxTbl)
}


getPAdjVal <- function(df, effectName){
  val <- getValWhereOtherValTrue(df, "Effect", effectName, "p.adj")
  return(val)
}

getPVal <- function(df, effectName){
  val <- getValWhereOtherValTrue(df, "Effect", effectName, "p")
  return(val)
}

getFVal <- function(df, effectName){
  val <- getValWhereOtherValTrue(df, "Effect", effectName, "F")
  return(val)
}

getDFnVal <- function(df, effectName){
  val <- getValWhereOtherValTrue(df, "Effect", effectName, "DFn")
  return(val)
}

getDFdVal <- function(df, effectName){
  val <- getValWhereOtherValTrue(df, "Effect", effectName, "DFd")
  return(val)
}

getFText <- function(df, effectName, doAdj = FALSE){
  Fval <- getFVal(df, effectName)
  if(!doAdj){
    pVal <- getPVal(df, effectName)
  } else {
    pVal <- getPAdjVal(df, effectName)
  }
  if(pVal < 0.001){
    pValText <-  "<0.001"
  }else {
    pValText <- paste0("=", pVal)
  }
  DFn <- getDFnVal(df, effectName)
  DFd <- getDFdVal(df, effectName)
  
  text <- paste0("F(", DFn, ",", DFd, ")=", Fval, ", p", pValText)
  
  return(text)
}

# getFText <- function(df, effectName, fontSize = 14, asFText = TRUE, sigVal = 0.05){
#   Fval <- getFVal(df, effectName)
#   pVal <- getPVal(df, effectName)
#   if(pVal < 0.001){
#     pValText <-  "<0.001"
#   }else {
#     pValText <- paste0("=", pVal)
#   }
#   DFn <- getDFnVal(df, effectName)
#   DFd <- getDFdVal(df, effectName)
#   
#   if(asFText){
#     fp_italic <- fp_text(italic = TRUE, font.size = fontSize)
#     fp_normal <- fp_text(font.size = fontSize)
#     fp_bold <- fp_text(bold = TRUE, font.size = fontSize)
#     
#     text <- list(
#       ftext(paste0("F(", DFn, ",", DFd, ")=", Fval, ", "), fp_normal),
#       ftext(paste0("p", pValText), fp_normal)
#     )
#   } else {
#     text <- paste0("F(", DFn, ",", DFd, ")=", Fval, ", p", pValText)
#   }
#   return(text)
# }