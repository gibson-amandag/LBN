my_gg_image <- function(img, imgTitle = NULL){
  viz <- ggplot() +
    background_image(img) +
    labs(title = imgTitle) +
    theme(
      aspect.ratio = dim(img)[2] / dim(img)[1],
      text = element_text(size = 11, family = "Arial")
    )
  return(viz)
}

arrangeCycleImgs <- function(
  mouseName,
  imgsFolderPath,
  outputFolder,
  ncol =5,
  maxRows = 4
){
  imgs <- load.dir(imgsFolderPath, pattern = ".jpg")
  imgNames <- names(imgs)
  numImgs <- length(imgs)
  imgPlots <- list()
  for(i in 1:numImgs){
    imgPlots[[i]] <- my_gg_image(imgs[[i]], imgNames[[i]])
  }
  names(imgPlots) <- imgNames
  
  totalRows <- ceiling(numImgs / ncol)
  
  nRowsPerPage <- ifelse(
    totalRows <= maxRows,
    totalRows,
    maxRows
  )
  
  arrangedPlots <- ggarrange(
    plotlist = imgPlots,
    ncol = ncol,
    nrow = nRowsPerPage
  )
  
  if(totalRows>maxRows){
    asSinglePage <- do.call(marrangeGrob, args = list(grobs = arrangedPlots, ncol=1, nrow=1)
                            )
  }else {
    asSinglePage <- arrangedPlots
  }
  
  ggsave(
    file.path(outputFolder, paste0(mouseName, "_cycles.pdf")),
    plot = asSinglePage,
    device = "pdf",
    width = 11,
    height = 8.5,
    scale = 1,
    units = "in"
  )
  return(arrangedPlots)
}