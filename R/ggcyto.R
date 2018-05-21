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
#' # do it programatically through aes_string and variables
#' col1 <- "`FSC-H`" #note that the dimension names with special characters needs to be quoted by backticks
#' col2 <- "`SSC-H`"
#' ggcyto(fs, aes_string(col1,col2)) + geom_hex()
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
  stats_limits <- list()
  trans <- list()
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
        {
          trans[[dim]] <- thisTrans
          this_limits <- thisTrans[["transform"]](this_limits)
        }
          
        x$scales$scales[[ind]][["limits"]] <- this_limits
      }
        
    }
    stats_limits[[dim]] <- x$scales$scales[[ind]][["limits"]]
    #update breaks and labels
    thisBreaks <- breaks[[this_aes]]
    if(!is.null(thisBreaks)){
      x$scales$scales[[ind]]$breaks <- thisBreaks[["at"]]
      x$scales$scales[[ind]]$labels <- thisBreaks[["label"]]  
    }
    
  }
  stats_limits <- as.data.frame(stats_limits, check.names = FALSE)
  stats_limits[["density"]] <- c(0,1e-4)
  fs <- x[["fs"]]
  #lazy parsing stats layer since the stats_limits is set at the end
  for(e2 in x[["GeomStats"]])
  {
    gate <- e2[["gate"]]
    #parse the gate from the each gate layer if it is not present in the current geom_stats layer
    if(is.null(gate))
    {
      
      pd <- .pd2dt(pData(fs))
      gates_parsed <- lapply(x$layers, function(layer){
        
        if(is.geom_gate_filterList(layer))#restore filter from fortified data.frame
          .filterList2dataframe(layer$data, colnames(pd))
        # else if(isTRUE(layer[["is_1d_gate"]]))
        # {
        #   .gate2dataframe(layer$data)
        # }
        else
          NULL
      })
      #remove NULL elements
      gates_parsed <- flowWorkspace:::compact(gates_parsed)
    }else{
      gates_parsed <- list(gate)
    }             
    
    
    if(length(gates_parsed) == 0)
      stop("geom_gate layer must be added before geom_stats!")
    
    
    # compute pop stats for each gate layer and 
    value <- e2[["value"]]
    stat_type <- e2[["type"]]
    

    #add default density range
    #In order to ensure the stats visiblity
    #try to put it closer to zero because we don't know the actual density range
    data_range <- as.data.frame(data_range)
    
    
    negated <- e2[["negated"]]
    adjust <- e2[["adjust"]]
    digits <- e2[["digits"]]
    if(length(trans)>0)
    {
      translist <- lapply(trans, function(t)t[["transform"]])
      translist <- transformList(names(translist), translist)
      inverselist <- lapply(trans, function(t)t[["inverse"]])
    }
    if(length(trans)>0&&is.null(value))#means fs will be used to compute stats and thus needs to be scaled properly
    {
      suppressMessages(fs <- transform(fs, translist))
    }
    for(gate in gates_parsed){
      if(length(trans)>0)
        gate <- transform(gate, translist)
      stats <- compute_stats(fs, gate, type = stat_type, value = value, data_range = data_range, limits = stats_limits, negated = negated, adjust = adjust, digits = digits)
      
      #restore the stats dimensions to raw scale
      if(length(trans)>0)
      {
        for(param in names(inverselist))
        {
          thisTrans <- inverselist[[param]]
          v <- thisTrans(stats[[param]])
          stats[, (param) := v]
        }  
      }
      
        
      # instantiate the new stats layer
      thisCall <- quote(geom_label(data = stats))
      # copy all the other parameters
      thisCall <-  as.call(c(as.list(thisCall), e2[["geom_label_params"]]))
      
      e2.new <- eval(thisCall)
      attr(e2.new, "is.recorded") <- TRUE
      # update aes
      stats_mapping <- aes_string(label = stat_type)
      #add y aes for 1d density plot
      dims <- sapply(x$mapping,as.character)
      dims <- dims[grepl("[x|y]", names(dims))]
      if(length(dims) == 1)
        stats_mapping <- defaults(stats_mapping, aes(y = density))
      e2.new$mapping <- defaults(e2.new$mapping, stats_mapping)  
      
      x <- ggplot2:::`+.gg`(x, e2.new)      
    }
  }
  #clear the raw data format
  x[["fs"]] <- NULL
  x[["gs"]] <- NULL 
  #strip the ggcyto class attributes
  asS3(x)
}
