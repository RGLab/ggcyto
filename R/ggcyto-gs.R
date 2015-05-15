
#' Create a new ggcyto plot from a flowSet
#'
#' @param data GatingSet to plot
#' @inheritParams ggcyto.flowSet
#' @method ggcyto GatingSet
#' @export
ggcyto.GatingSet <- function(data, parent, mapping, ...){
#   
#   
#   if(!missing(mapping)){
#     dims <- sapply(mapping,as.character)
#     dims <- dims[grepl("[x|y]", names(dims))]
#     nDims <- length(dims)
#   }else
#     stop("mapping must be supplied to ggplot!")
  
  p <- ggcyto.default(data = data, parent = parent, mapping = mapping, ...)
  p <- p + labs(title  = parent)
  #somehow p + theme won't work here
  p <- `+.ggcyto`(p, theme(plot.title = element_text(face="bold")))
  
  # prepend the ggcyto class attribute
  class(p) <- c("ggcyto.gs", class(p))  
  p
}

#' overloaded '+' method for ggcyto.gs
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
`+.ggcyto.gs` <- function(e1, e2){
  
  if(is.proto(e2)){
    if(e2$geom$objname=="gs.node"){
      #instanatiate e2 layer as a specific gate layer
#       browser()      
      gs <- attr(e1$data, "gs")
      node <- e2$stat_params[["node"]]  
      gate <- getGate(gs, node)
      e2.new <- geom_gate(gate)
      # copy all the other parameters
      e2.new$geom_params <- defaults(e2.new$geom_params, e2$geom_params)
      e2.new$stat_params <- defaults(e2.new$stat_params, e2$stat_params)
      e2 <- e2.new
    }else if(e2$geom$objname == "popStats"){
      browser()
      # insert the pre-calculated stats
      stat_type <- e2$stat_params[["type"]]
      gate <- e2$stat_params[["gate"]]
      value <- e2$stat_params[["value"]]
      
      if(is.null(value)){
        # look for node from previous gate layer if it is not given
        node <- e2$stat_params[["node"]]
        if(is.null(node)){
          found <- FALSE
          for(layer in e1$layers){
            layer_data <- layer$data
            if(is(layer_data, "geom_gate_filterList")){
              browser()
              found <- TRUE
              break
            }
            
          }
          if(!found)
            stop("geom_gate layer must be added before geom_stats!")
        }
      }
    }
    
  }
  
    
  `+.ggcyto`(e1, e2)
}
