#' Create a new ggcyto plot
#'
#' \code{ggcyto()} initializes a ggcyto object that inherits ggplot class.
#' Similarly the + operator can be used to add layers to the
#' existing ggcyto object. 
#'
#' To invoke \code{ggcyto}:
#' \itemize{
#'    \item \code{ggcyto(fs, aes(x, y, <other aesthetics>))}
#'   }
#' @export
#' @keywords internal
#' @param data default cytometry data set.(flowSet,flowFrame)
#' @param ... other arguments passed to specific methods
#' @examples
#' 
#' #construct the `ggcyto` object (inherits from `ggplot` class)
#' p <- ggcyto(fs, aes(x = `FSC-H`)) 
#' p + geom_histogram() 
#'
#' # display density/area
#' p + geom_density()
#' p + geom_area(stat = "density") 
#' 
#' # 2d scatter plot
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
#' p + stat_binhex(bin = 128)
ggcyto <- function(data = NULL, ...) UseMethod("ggcyto")

#' Reports whether x is a ggcyto object
#' @param x An object to test
#' @export
is.ggcyto <- function(x) inherits(x, "ggcyto")

#' @export
ggcyto.default <- function(data = NULL, mapping = aes(), ...) {
  ggcyto.flowSet(fortify_fs(data, ...), mapping)
}

#' Draw ggcyto on current graphics device.
#'
#' A wrapper for print.ggplot. 
#' @param x ggcyto object to display
#' @param ... other arguments not used by this method
#' @export
#' @method print ggcyto
print.ggcyto <- function(x, ...) {
  
    #fortify plot data here instead
    x <- as.ggplot(x)
    ggplot2:::print.ggplot(x)
}

#' It simply fortifies data and return a regular ggplot object.
#' 
#' The orginal data format is preserved during the ggcyo constructor because they still need to be used during the plot building process.
#' 
#' @param x ggcyto object with the data that has not yet been fortified to data.frame.
#' 
#' @export
as.ggplot <- function(x){
#   browser()
  #lazy-fortifying the plot data
  x$data <- fortify(x$data)
  
  #lazy-breaks and labels setting
  ranges <- x[["axis_inverse_trans"]]
  if(length(ranges) > 0)
  {
    old.scales <- x$scales
    
    for(axis_name in names(ranges))
    {
      
      thisRange <- ranges[[axis_name]]
      
      if(old.scales$has_scale(axis_name)){
        #modifying the exsiting scale instead of adding new ones
        ind <- which(old.scales$find(axis_name))
        old.scales$scales[[ind]]$breaks <- thisRange[["at"]]
        old.scales$scales[[ind]]$labels <- thisRange[["label"]]
      }else{
        #add new one
        new.scale <- switch(axis_name
                         , x = scale_x_continuous(breaks = thisRange[["at"]], labels = thisRange[["label"]])
                         , y = scale_y_continuous(breaks = thisRange[["at"]], labels = thisRange[["label"]])
                         )
        x <- x + new.scale
      }
        
        
    }
    
  }
  
  x
}
#' @rdname print.ggcyto
#' @method plot ggcyto
#' @export
plot.ggcyto <- print.ggcyto
