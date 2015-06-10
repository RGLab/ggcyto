#' add a flowCore gate layer
#' 
#' When 'data' is a list of gates or a filterList object, to be used directly with 'ggplot', pdata of the flow data must be supplied through 'pd' argument explicitly in order for 
#' the gates to be dispatched to each panel. 
#' However It is not necessary when it is used with 'ggcyto' wrapper
#' 
#' When 'data' is a character, it construct an abstract geom layer for a character that represents nodes in a Gating tree
#' and will be instanatiated later as a specific geom_gate layer or layers based on the gates extracted from the given GatingSet object.
#'
#' @param data a filter (Currently only rectangleGate (1d or 2d), polygonGate, ellipsoidGate are supported.)
#'              or a list of these gates 
#'              or filterList
#'              or character specifying a gated cell population in the GatingSet
#'              
#' @param ... other arguments
#'        mapping, The mapping aesthetic mapping
#'        data a polygonGate
#'        fill polygonGate is not filled by default
#'        colour default is red
#'        pd pData (data.frame) that has rownames represents the sample names used as key to be merged with filterList
#' @export
geom_gate <- function(data, ...)UseMethod("geom_gate")

#' @export
#' @rdname geom_gate
geom_gate.default <- function(data, ...){
  
  if(missing(data)){
    data <- "_child_"
      geom_gate(data, ...)
  }else
    stop("ggcyto doesn't know how to deal with gate of class ", class(data), call. = FALSE)
}

#' @rdname geom_gate
#' @export
geom_gate.list <- function(data, ...){
  data <- filterList(data)
  geom_gate(data, ...)  
}


#' @rdname geom_gate
#' @export
geom_gate.filterList <- function(data, ...){
  .geom_gate_filterList(data, ...)
}
.geom_gate_filterList <- function(data, pd, ...){  
  #construct gate-type specific layer
  geom_gate_layer <- geom_gate(data[[1]], ...)
#   browser()
  
  
  # assuming it is already attached to attribute
  # when pd is missing
  if(!missing(pd)){
    attr(data, "pd") <- pd #this step is done automatically when `+.ggcyto_flowSet` is invoked
  }
    
  
  #must explicitly fortify it since ggplot only does it during the layer$new method 
  data <- fortify(data) 

  #tag this data.table so that ggcyo wrapper can recongnize it
  class(data) <- c("geom_gate_filterList", class(data))
  # update data with pdata
  geom_gate_layer[["data"]] <- data
  geom_gate_layer
}

#' construct geom layer for polygonGate
#' 
#' @export
#' @rdname geom_gate
geom_gate.polygonGate <- function(data, ...){
  .geom_gate_polygonGate(data, ...)  
}
.geom_gate_polygonGate <- function(data, mapping = NULL, fill = "transparent", colour = "red", ...){
  
  geom_path(mapping = mapping, data = data , fill = fill, colour = colour, ...)  
}

#' construct geom layer for rectangleGate
#' 
#' @rdname geom_gate
#' @export
geom_gate.rectangleGate <- function(data, ...){
  .geom_gate_rectangleGate(data, ...)
}
.geom_gate_rectangleGate <- function(data, mapping = NULL, fill = "transparent", colour = "red", ...){
  
  param <- parameters(data)
  nDim <- length(param)
  if (nDim ==  2){
        geom_path(data = data, mapping = mapping, fill = fill, colour = colour, ...)
  }else if(nDim ==  1){
#     browser()
      geom_hvline(data = data, fill = fill, colour = colour, ...)
         
  }else
    stop("rectangelGate with dimension ", nDim, "is not supported!")
  
}

#' @rdname geom_gate
#' @export
geom_gate.character <- function(data, ...){
  
  GeomGsNode$new(data = NULL, node = data, ...)
}

GeomGsNode <- proto(ggplot2:::Geom, {
  objname <- "gs.node"
  default_stat <- function(.) StatIdentity
})
