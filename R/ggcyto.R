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
  ggcyto.flowSet(fortify.fs(data, ...), mapping)
}


#' Create a new ggcyto plot from a flowSet
#'
#' @param data default flowSet for plot
#' @param mapping default list of aesthetic mappings (these can be colour,
#'   size, shape, line type -- see individual geom functions for more details)
#' @param ... ignored
#' @param environment in which evaluation of aesthetics should occur
#' @method ggcyto flowSet
#' @export
#' @importFrom RColorBrewer brewer.pal
ggcyto.flowSet <- function(data, mapping, ...){
  #instead of using ggplot.default method to contruct the ggplot object
  # we call the underlining s3 method directly to avoid foritying data at this stage
#   p <- ggplot2:::ggplot.data.frame(data, mapping)

  if(!missing(mapping)){
    dims <- sapply(mapping,as.character)
    dims <- dims[grepl("[x|y]", names(dims))]
    nDims <- length(dims)
  }else
    stop("mapping must be supplied to ggplot!")
#   browser()  
  p <- ggplot(data = data, mapping, ...)
  # add default facetting
  p <- p + facet_wrap(~name) 
  #     browser()
  if(nDims == 2){
    # add default fill gradien
    myColor <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
    p <- p + scale_fill_gradientn(colours = myColor)  
  }
  # prepend the ggcyto class attribute
  class(p) <- c("ggcyto", class(p))  
  p
}

#' overloaded '+' method for ggcyto
#' 
#' It tries to copy pData from ggcyto object to the gate layers
#' so that the gate layer does not need to have `pd` to be supplied explicitly by users.
#' 
#' @param e1 An object of class \code{ggcyto}
#' @param e2 A component to add to \code{e1}
#' 
#' @method + ggcyto
#' @rdname ggcyto-add
#' @importFrom plyr defaults
#' @export
`+.ggcyto` <- function(e1, e2){
  
  # modifying e2 layer by adding pd attribute to layered data 
  # it is used solely for geom_gate.filterList layer
  if(is.proto(e2)){
    layer_data <- e2$data
    
    if(is(layer_data, "geom_gate_filterList")){
        if(!isTRUE(attr(layer_data, "annotated"))){
          pd <- attr(e1$data, "pd")
        
          layer_data <- merge(layer_data, pd, by = ".rownames")  
          attr(layer_data, "annotated") <- TRUE
          e2$data <- layer_data
        }
        
    }else if(e2$geom$objname == "popStats"){
#       browser()    
      #parse the flow data
      plot_data <- e1$data
      fs <- .df2fs(plot_data)
      #parse the gate (currently we only look at the first gate layer )
      found <- FALSE
      for(layer in e1$layers){
        layer_data <- layer$data
        if(isTRUE(attr(layer_data, "annotated"))){
#           browser()
          gate <- .df2gate(layer_data, flowCore::colnames(fs))
          found <- TRUE
          break
        }  
      }
      if(!found)
        stop("geom_gate layer must be added before geom_stats!")
      
     # do the gating to get pop stats
     stat_type <- e2$stat_params[["type"]]
     stats <- compute_stats(fs, gate, type = stat_type)
     # update the data for geo_stats
     e2$data <- stats
     # update aes
     stats_mapping <- aes_string(label = stat_type)

     #add y aes for 1d density plot
     dims <- sapply(e1$mapping,as.character)
     dims <- dims[grepl("[x|y]", names(dims))]
     if(length(dims) == 1)
       stats_mapping <- defaults(stats_mapping, aes(y = density))
     e2$mapping <- defaults(e2$mapping, stats_mapping)

    }
    
  }
  
# browser()
  
  ggplot2:::`+.gg`(e1, e2)
}

#' Convert data.frame back to original flowSet format
#' 
#' It is used for gating purporse for geom_stats layer
#' @importFrom plyr dlply
.df2fs <- function(df){
  
  pd <- attr(df, "pd")

  frlist <- dlply(df, .variables = ".rownames", function(sub_df){
    markers <- setdiff(colnames(sub_df), colnames(pd))
    fr <- flowFrame(exprs = as.matrix(sub_df[, markers]))
    fr
  })
  fs <- as(frlist, "flowSet")
  pData(fs) <- name_rows(pd)
  fs
}

#' Convert data.frame back to original gate format
#' 
#' It is used for gating purporse for geom_stats layer
#' @param chnls the valid channels
.df2gate <- function(df, chnls){
  
  
  glist <- dlply(df, .variables = ".rownames", function(sub_df){
    
    markers <- intersect(colnames(sub_df), chnls)
    sub_df <- sub_df[, markers, drop = FALSE]
    nDim <- length(markers)
    if(nDim == 2){
      g <- polygonGate(sub_df)  
    }else if (nDim == 1){
#       browser()
      g <- rectangleGate(sub_df)
    }else
      stop("invalid dimension number!")
    g
  })
  
  filterList(glist)
}

# #' Draw ggcyto on current graphics device.
# #'
# #' A wrapper for contructing the actual ggplot objects
# #' @param x ggcyto object to display
# #' @param ... other arguments not used by this method
# #' @export
# #' @method print ggplot
# print.ggcyto <- function(x, ...) {
# 
#     #fortify plot data here instead
#     x$data <- fortify(x$data)
#     mapping <- x$mapping
#     if(!missing(mapping)){
#       dims <- sapply(mapping,as.character)
#       dims <- dims[grepl("[x|y]", names(dims))]
#       nDims <- length(dims)
#     }else
#       stop("mapping must be supplied to ggplot!")
#   
#     
#     # add default facetting
#     x <- x + facet_wrap(~name) 
# #     browser()
#     if(nDims == 2){
#       # add default fill gradien
#       myColor <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
#       x <- x + scale_fill_gradientn(colours = myColor)  
#     }
#     #strip the ggcyto attribute and let ggplot does its job
#     class(x) <- class(x)[-1]
#     printx
# }
# #' @rdname print.ggcyto
# #' @method plot ggcyto
# #' @export
# plot.ggcyto <- print.ggcyto
