my_gg_image <- function(img, imgTitle = NULL){
  viz <- ggplot() +
    background_image(img) +
    labs(title = imgTitle) +
    theme(
      aspect.ratio = dim(img)[2] / dim(img)[1]
    )
  return(viz)
}

arrangeCycleImgs <- function(
  mouseName,
  imgsFolderPath,
  outputFolder
){
  imgs <- load.dir(imgsFolderPath, pattern = ".jpg")
  imgNames <- names(imgs)
  numImgs <- length(imgs)
  imgPlots <- list()
  for(i in 1:numImgs){
    imgPlots[[i]] <- my_gg_image(imgs[[i]], imgNames[[i]])
  }
  names(imgPlots) <- imgNames
  
  totalRows <- ceiling(numImgs / 5)
  
  nRowsPerPage <- ifelse(
    totalRows <= 4,
    totalRows,
    4
  )
  
  arrangedPlots <- ggarrange(
    plotlist = imgPlots,
    ncol = 5,
    nrow = nRowsPerPage
  )
  
  ggsave(
    file.path(outputFolder, paste0(mouseName, "_cycles.pdf")),
    plot = arrangedPlots,
    width = 11,
    height = 8.5,
    scale = 1,
    units = "in"
  )
  return(arrangedPlots)
}