# all ph_label can be read here
# layout_properties(doc, layout = "Title and Content")

addSlide_oneGraph <- function(
  ppt,
  title,
  plot,
  makeEditable = FALSE
){
  if(makeEditable){
    plot <- dml(ggobj=plot)
  }
  
  ppt <- add_slide(ppt, "Title and Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot,
    location = ph_location_label("Content Placeholder 2")
  )
  return(ppt)
}

addSlide_twoGraph <- function(
  ppt,
  title,
  plot1,
  plot2,
  makeEditable = FALSE
){
  if(makeEditable){
    plot1 <- dml(ggobj = plot1)
    plot2 <- dml(ggobj = plot2)
  }
  ppt <- add_slide(ppt, "Two Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot1,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = plot2,
    location = ph_location_label("Content Placeholder 3")
  )
  return(ppt)
}

addSlide_threeGraph <- function(
  ppt,
  title,
  plot1,
  plot2,
  plot3,
  makeEditable = FALSE
){
  if(makeEditable){
    plot1 <- dml(ggobj = plot1)
    plot2 <- dml(ggobj = plot2)
    plot3 <- dml(ggobj = plot3)
  }
  ppt <- add_slide(ppt, "Title_threeContent")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot1,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = plot2,
    location = ph_location_label("Content Placeholder 3")
  )
  ppt <- ph_with(
    ppt,
    value = plot3,
    location = ph_location_label("Content Placeholder 4")
  )
  return(ppt)
}

addSlide_fourGraph <- function(
  ppt,
  title,
  plot1,
  plot2,
  plot3,
  plot4,
  makeEditable = FALSE
){
  if(makeEditable){
    plot1 <- dml(ggobj = plot1)
    plot2 <- dml(ggobj = plot2)
    plot3 <- dml(ggobj = plot3)
    plot4 <- dml(ggobj = plot4)
  }
  ppt <- add_slide(ppt, "Title_fourContent")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot1,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = plot2,
    location = ph_location_label("Content Placeholder 3")
  )
  ppt <- ph_with(
    ppt,
    value = plot3,
    location = ph_location_label("Content Placeholder 4")
  )
  ppt <- ph_with(
    ppt,
    value = plot4,
    location = ph_location_label("Content Placeholder 5")
  )
  return(ppt)
}

addSlide_DemoCycles <- function(
  ppt,
  title,
  STDplot,
  LBNplot,
  percDaysPlot,
  makeEditable = FALSE
){
  if(makeEditable){
    STDplot <- dml(ggobj = STDplot)
    LBNplot <- dml(ggobj = LBNplot)
    percDaysPlot <- dml(ggobj = percDaysPlot)
  }
  ppt <- add_slide(ppt, "Title_demoCycles")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = STDplot,
    location = ph_location_label("STD")
  )
  ppt <- ph_with(
    ppt,
    value = LBNplot,
    location = ph_location_label("LBN")
  )
  ppt <- ph_with(
    ppt,
    value = percDaysPlot,
    location = ph_location_label("percDays")
  )
  return(ppt)
}

addSlide_oneTable <- function(
  ppt,
  title,
  table,
  textSize = 16,
  dontFormat = FALSE
){
  ppt <- add_slide(ppt, "Title and Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  
  if(!dontFormat){
    table <- table %>%
      flextable() %>%
      fontsize(size = textSize, part = "all") %>%
      colformat_double(digits = 3) %>%
      autofit()
  } else { ## have to be careful here - needs to already by a flextable
    table <- table %>%
      autofit()
  }
    
  
  ppt <- ph_with(
    ppt,
    value = table,
    location = ph_location_label("Content Placeholder 2")
  )
  return(ppt)
}

addSlide_text <- function(
  ppt,
  title,
  blockList,
  listLevels
){
  ppt <- add_slide(ppt, "Title and Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  
  ppt <- ph_with(
    ppt,
    value = blockList,
    level_list = listLevels,
    location = ph_location_label("Content Placeholder 2")
  )
  return(ppt)
}

makeBullet <- function(theText, fontSize = 22)
  fpar(
    ftext(
      theText,
      fp_text(font.size=fontSize)
    )
  )