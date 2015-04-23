#' @importFrom ggplot2 autoplot
#' @export autoplot.flowFrame
autoplot.flowFrame <- function(object, ...){
  object <- .flowFrame2flowSet(object)
  autoplot(object, ...)
}

#' Plot fluorescence intensity in one or two dimension.
#' 
#' Overloaded autoplot for the cytomertry data structure: flowFrame or flowSet.
#' It plots the cytometry data with geom_histogram, geom_density or geom_hex.
#' 
#' @param object flowFrame or flowSet object
#' @param mapping passed to ggplot, which defines the dimension of the plot
#' @param ... other arguments passed to ggplot
#' @param plotType either "histogram" or "density", determines which plot to be displayed
#' @param margin whether to enable marginal/bounary events filtering. Default is TRUE.
#' 
#' @return a ggplot object
#' 
#' @examples
#' data(GvHD)
#' fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
#'
#' #histogram for raw flow data
#' autoplot(fs, aes(x = `FL1-H`))
#' # add transformation
#' autoplot(fs, aes(x = `FL1-H`)) + scale_x_log10()
#' 
#' # disable marginal events filtering
#' autoplot(fs, aes(x = `FL1-H`), margin = F) + scale_x_log10()
#' 
#' # density
#' autoplot(fs, aes(x = `FL1-H`), plotType = "density") + scale_x_log10()
#' 
#' # customize border colors 
#' ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name) + geom_histogram(colour = "white") + scale_x_log10()
#' 
#' # change the bin width
#' ggplot(fs, aes(x = `FL1-H`)) + facet_wrap(~name, scale = "free") + geom_histogram(colour = "white", binwidth = 1/10) + scale_x_log10()
#' 
#' #2d plot: default geom_hex plot
#' autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) 
#' 
#' # add contour
#' autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + geom_density2d(colour = "black")
#' 
#' # change the faceting
#' autoplot(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + facet_grid(Patient~Visit) 
#' 
#' @aliases autoplot autoplot.flowFrame 
#' @importFrom RColorBrewer brewer.pal
#' @export autoplot.flowSet
autoplot.flowSet <- function(object, mapping, ..., plotType = "histogram", margin = TRUE){
  
  plotType <- match.arg(plotType, c("histogram", "density"))
  
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
