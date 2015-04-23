#' @export
fortify.flowFrame <- function(model, data, ...){
  #covert to flowSet
  sn <- identifier(model)
  model <- as(model, "flowSet")
  sampleNames(model) <- sn
  pData(model)[["name"]] <- sn
  #then dispatch to forityf method for flowSet
  fortify(model, ...)
}

#' @export
as.data.frame.flowFrame <- function(x, ...){
  as.data.frame(exprs(x))
}

#' @export
as.data.frame.flowSet <- function(x, ...){
  df.list <- fsApply(x, as.data.frame, simplify = FALSE)
  df <- ldply(df.list)
  df  
}

#' @importFrom plyr ldply
#' @export
fortify.flowSet <- function(model, data, ...){
  #convert to data.frame
  df <- as.data.frame(model)
  #merge with pData
  pd <- pData(model)
  df <- merge(pd, df, by.x = "name", by.y = ".id")
  df
}

#' @importFrom ggplot2 autoplot
#' @export autoplot.flowFrame
autoplot.flowFrame <- function(object, ...){
  sn <- identifier(object)
  object <- as(object, "flowSet")
  sampleNames(object) <- sn
  pData(object)[["name"]] <- sn
  autoplot(object, ...)
}


#' @importFrom RColorBrewer brewer.pal
#' @export autoplot.flowSet
autoplot.flowSet <- function(object, mapping, ..., plotType = "histogram", margin = TRUE){
  
  plotType <- match.arg(plotType, c("histogram", "density", "xyplot"))
  
  # check the dimensions
  if(!missing(mapping)){
    dims <- sapply(mapping,as.character)
    nDims <- length(mapping)
  }else
    stop("mapping must be supplied to ggplot!")
  
  # apply boundary filter to remove outliers
  if(margin){
    g <- boundaryFilter(x = dims, tol = 1e-5)
    object <- Subset(object, g)
  }
  
  df <- fortify(object)
  
  p <- ggplot(df, mapping, ...)
  
  #hide the legend by default
  p <- p + guides(colour = F, fill = F)
  
  #default faceting by sample names
  p <- p + facet_wrap(~name)
  
  #if 2d
  if(nDims == 1){
    if(plotType == "histogram"){
      p <- p + geom_histogram()
    }else{
      p <- p + geom_density(aes(fill = "grey50", colour = "grey50"))
    }
      
  }else if(nDims == 2){
    p <- p + geom_hex() 
    p <- p + scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")))  
  }else
    stop("Only 1d or 2d plots are currently supported!")
  p
}
