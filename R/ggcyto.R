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
#'
#'  @return ggcyto object      
#' @import methods ggplot2 flowCore ncdfFlow flowWorkspace
#' @export
#' @keywords internal
#' @param data default cytometry data set.(flowSet,flowFrame)
#' @param ... other arguments passed to specific methods
#' @examples
#' 
#' data(GvHD)
#' fs <- GvHD[1:3]
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
#' p + geom_hex(bins = 128)
ggcyto <- function(data = NULL, ...) UseMethod("ggcyto")


#' Reports whether x is a ggcyto object
#' @param x An object to test
#' @return TRUE/FALSE
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:2]
#' p <- ggcyto(fs, aes(x = `FSC-H`))
#' is.ggcyto(p)
#' @export
is.ggcyto <- function(x) inherits(x, "ggcyto")

#' @export
#' @rdname  ggcyto
ggcyto.default <- function(data = NULL, mapping = aes(), ...) {
  ggcyto.flowSet(fortify_fs(data, ...), mapping)
}

#' Draw ggcyto on current graphics device.
#'
#' A wrapper for print.ggplot. It converts the ggcyto to conventional ggplot object before printing it.
#' This is usually invoked automatically when a ggcyto object is returned to R console.
#' 
#' @return nothing
#' @param x ggcyto object to display
#' @param ... other arguments not used by this method
#' 
#' @export
#' @method print ggcyto
print.ggcyto <- function(x, ...) {
  
    
    x <- ggplot2:::plot_clone(x) #clone plot to avoid tampering original x due to ther referenceClass x$scales
    x <- as.ggplot(x) 
    ggplot2:::print.ggplot(x)
}

#' @rdname print.ggcyto
#' @method plot ggcyto
#' @export
plot.ggcyto <- print.ggcyto

#--------These S4 methods exsits for plotting ggcyto object automatically in R console---------------#
#' @export
#' @rdname print.ggcyto
setMethod("print", c("ggcyto"), print.ggcyto)


#' @param object ggcyto object
#' @rdname print.ggcyto
#' @method show ggcyto
#' @export
show.ggcyto <- function(object){print(object)}

#' @rdname print.ggcyto
#' @method show ggcyto
#' @export
setMethod("show", "ggcyto", show.ggcyto)

#' It fortifies the data, fills some default settings and returns a regular ggplot object.
#' 
#' The orginal data format is preserved during the ggcyo constructor because they still need to be used during the plot building process.
#' This function is usually called automatically in the print/plot method of ggycyto. Sometime it is useful to coerce it to ggplot explictily 
#' by user so that it can be used as a regular ggplot object.
#' 
#' @param x ggcyto object with the data that has not yet been fortified to data.frame.
#' 
#' @return ggplot object
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:3]
#' #construct the `ggcyto` object (inherits from `ggplot` class)
#' p <- ggcyto(fs, aes(x = `FSC-H`)) + geom_histogram() 
#' class(p) # a ggcyto object
#' p$data # data has not been fortified
#' p1 <- as.ggplot(p) # convert it to a ggplot object explictily 
#' class(p1) 
#' p1$data # data is fortified
#' @export
as.ggplot <- function(x){

  #####################
  #lazy-fortifying the plot data
  #####################
  dims <- attr(x[["fs"]], "dims")
  aes_names <- dims[, axis]
  chnls <- dims[, name]
  
  instrument_range <- x[["instrument_range"]]
  
  #data needs to be fortified here if geom_gate was not added
  if(!is(x[["data"]], "data.table")){
    x[["data"]] <- fortify(x[["data"]])
    data_range <- apply(x[["data"]][, chnls, with = FALSE], 2, range)
    rownames(data_range) <- c("min", "max")  
  }else
    data_range <- x[["data_range"]]
  

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
      par_limits <- x$ggcyto_pars[["limits"]]
      if(is.list(par_limits)){
        this_limits <- par_limits[[this_aes]]
      }else if(is.character(par_limits)){
        if(par_limits == "instrument")
          this_limits <- instrument_range[, dim]
        else if(par_limits == "data")#need to scale by flow data only in case gate data screw up the entire scale
          this_limits <- data_range[, dim]
        else
          this_limits <- NULL
      }
      
      if(!is.null(par_limits)){
        
        #trans the given limits if trans is also present
        thisTrans <- x$scales$scales[[ind]][["trans"]]
        if(is(thisTrans, "trans"))
          this_limits <- thisTrans[["transform"]](this_limits)
        x$scales$scales[[ind]][["limits"]] <- this_limits
      }
        
    }
    #update breaks and labels
    thisBreaks <- breaks[[this_aes]]
    if(!is.null(thisBreaks)){
      x$scales$scales[[ind]]$breaks <- thisBreaks[["at"]]
      x$scales$scales[[ind]]$labels <- thisBreaks[["label"]]  
    }
    
  }
  #clear the raw data format
  x[["fs"]] <- NULL
  x[["gs"]] <- NULL 
  #strip the ggcyto class attributes
  asS3(x)
}
