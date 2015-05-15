
#' Create a new ggcyto plot from a flowSet
#'
#' @param data GatingSet to plot
#' @param subset character that specifies the node path or node name in the GatingSet. 
#'                Default is "_parent_", which will be substitute with the actual node name 
#'                based on the geom_gate layer to be added later.
#' @inheritParams ggcyto.flowSet
#' @method ggcyto GatingSet
#' @export
ggcyto.GatingSet <- function(data, mapping, subset = "_parent_", ...){
  attr(data, "subset") <- subset
#   browser()
  p <- ggcyto.flowSet(data = data, mapping = mapping, ...)
  p <- p + labs(title  = subset)
  #somehow p + theme won't work here
  p <- `+.ggcyto_flowSet`(p, theme(plot.title = element_text(face="bold")))
  
  # prepend the ggcyto class attribute
  class(p) <- c("ggcyto_GatingSet", class(p))  
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
#' @method + ggcyto_GatingSet
#' @rdname ggcyto-add
#' @importFrom plyr defaults
#' @export
`+.ggcyto_GatingSet` <- function(e1, e2){
#   browser()
  if(is.proto(e2)){
    if(e2$geom$objname=="gs.node"){
      #instanatiate e2 layer as a specific gate layer
#       browser()      
      gs <- e1$data
      nodes <- e2$stat_params[["node"]]  
      #instantiate the parent by the first node if it is not yet been done
      parent <- attr(gs, "subset")
      if(nodes == "_child_"){
        if(parent == "_parent_")
          stop("either 'subset' in ggcyto object or 'data' in geom_gate layer needs to be specified!")
        nodes <- getChildren(gs[[1]], parent, showHidden = FALSE)
      }
      if(parent == "_parent_"){
        parent <- getParent(gs[[1]], nodes[[1]])
        attr(e1$data, "subset") <- parent
      }
#       browser()
      if(e1$labels[["title"]] == "_parent_")
        e1$labels[["title"]] <- parent
      
      for(node in nodes)
      {
        gate <- getGate(gs, node)
        e2.new <- geom_gate(gate)
        # copy all the other parameters
        e2.new$geom_params <- defaults(e2.new$geom_params, e2$geom_params)
        e2.new$stat_params <- defaults(e2.new$stat_params, e2$stat_params)
        e1 <- `+.ggcyto_flowSet`(e1, e2.new)
      }
     return (e1) 
      
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
  
    
  `+.ggcyto_flowSet`(e1, e2)
}
