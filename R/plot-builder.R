ggcyto <- function(data = NULL, ...) UseMethod("ggcyto")
ggcyto.flowFrame <- function(data, ...){
  data <- .flowFrame2flowSet(data)
  ggcyto(data, ...)
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
ggcyto.flowSet <- function(data, mapping, ...){
  
    if(!missing(mapping)){
      dims <- sapply(mapping,as.character)
      nDims <- length(mapping)
    }else
      stop("mapping must be supplied to ggplot!")
  
    p <- ggplot(data, mapping =  mapping, ...)
    # add default facetting
    p <- p + facet_wrap(~name) 
#     browser()
    if(nDims == 2){
      # add default fill gradien
      myColor <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
      p <- p + scale_fill_gradientn(colours = myColor)  
    }
    
    class(p) <- c("ggcyto", class(p))
    p
}

#' overloaded '+' method for ggcyto
#' 
#' It tries to pass the copy of pData of fs(supplied in ggplot object) to gate objects
#' so that the gate layer does not need explicit `pd` to be supplied by users.
#' 
#' @param e1 An object of class \code{ggcyto}
#' @param e2 A component to add to \code{e1}
#' 
#' @method + ggcyto
#' @rdname ggcyto-add
#' @export
`+.ggcyto` <- function(e1, e2){
  browser()
  
  e1 + obj
}
