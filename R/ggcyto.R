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

#' It fortifies the data, fills some default settings and returns a regular ggplot object.
#' 
#' The orginal data format is preserved during the ggcyo constructor because they still need to be used during the plot building process.
#' 
#' @param x ggcyto object with the data that has not yet been fortified to data.frame.
#' 
#' @export
as.ggplot <- function(x){
#   browser()
  #####################
  #lazy-fortifying the plot data
  #####################
  dims <- attr(x$data, "dims")
  aes_names <- dims[, axis]
  chnls <- dims[, name]
#   browser()
  #fortify to fs first in order to get instrument range
  x$data <- fortify_fs(x$data)
  instrument_range <- range(x$data[[1, use.exprs= FALSE]])[, chnls]
  x$data <- fortify(x$data)
  
  #####################
  #update default scales
  #####################
  breaks <- x[["axis_inverse_trans"]]
  
  for(this_aes in aes_names)
  {
    dim <- dims[axis == this_aes, name]
    # set limits
    if(!x$scales$has_scale(this_aes))
    {
      #add new one if not present 
      new.scale <- ggplot2:::make_scale("continuous", this_aes)
      x <- x + new.scale
    }
    ind <- which(x$scales$find(this_aes))
    #set the default limits if it has not been set
    if(is.null(x$scales$scales[[ind]][["limits"]])){
      theme_limits <- x$ggcyto_theme[["limits"]]
      if(is.list(theme_limits)){
        this_limits <- theme_limits[[this_aes]]
      }else if(is.character(theme_limits)){
        if(theme_limits == "instrument")
          this_limits <- instrument_range[, dim]
        else if(theme_limits == "data")#need to scale by flow data only in case gate data screw up the entire scale
          this_limits <- range(x$data[, dim, with = FALSE])
        else
          this_limits <- NULL
      }
      
      if(!is.null(theme_limits))
        x$scales$scales[[ind]][["limits"]] <- this_limits
    }
    #update breaks and labels
    thisBreaks <- breaks[[this_aes]]
    if(!is.null(thisBreaks)){
      x$scales$scales[[ind]]$breaks <- thisBreaks[["at"]]
      x$scales$scales[[ind]]$labels <- thisBreaks[["label"]]  
    }
    
  }
    
  #strip the ggcyto class attributes
  class(x) <- c("gg", "ggplot")
  x
}
#' @rdname print.ggcyto
#' @method plot ggcyto
#' @export
plot.ggcyto <- print.ggcyto
