#' add a flowCore gate layer
#' 
#' Currently only rectangleGate (1d or 2d), polygonGate, ellipsoidGate are supported.
#' 
#' @param data a rectangleGate, polygonGate or ellipsoidGate
#' @export
geom_gate <- function(data, ...)UseMethod("geom_gate")

#' @export
#' @rdname geom_gate
geom_gate.default <- function(data, ...){
  stop("ggcyto doesn't know how to deal with gate of class ", class(data), call. = FALSE)
}

#' @rdname geom_gate.filterList
#' @param data a named list
#' @export
geom_gate.list <- function(data, ...){
  data <- filterList(data)
  geom_gate(data, ...)  
}

#' Add layer for flowCore::filterList
#' 
#' pdata of the flow data must be supplied here explicitly in order for 
#' the gates to be dispatched to each panel.
#' 
#' @param data filterList
#' @param pd pData (data.frame) that has rownames represents the sample names used as key to be merged with filterList
#' @export
geom_gate.filterList <- function(data, pd, ...){
  
  #construct gate-type specific layer
  geom_gate_layer <- geom_gate(data[[1]], ...)
  # update data
  df <- fortify(data, pd)
  #   browser()
  geom_gate_layer[["data"]] <- df
  geom_gate_layer
}

#' construct geom layer for polygonGate
#' 
#' @export
#' 
#' @param The mapping aesthetic mapping
#' @param data a polygonGate
#' @param fill polygonGate is not filled by default
#' @param colour default is red
geom_gate.polygonGate <- function(data, mapping = NULL, fill = "transparent", colour = "red", ...){
  
  geom_polygon(mapping = mapping, data = data , fill = fill, colour = colour, ...)  
}

#' construct geom layer for rectangleGate
#' 
#' 
#' @export
#' @param The mapping aesthetic mapping
#' @param gate a rectangleGate
#' @param fill rectangleGate is not filled by default
#' @param colour default is red
geom_gate.rectangleGate <- function(data, mapping = NULL, fill = "transparent", colour = "red", ...){
  
  param <- parameters(data)
  nDim <- length(param)
  if (nDim ==  2){
        geom_polygon(data = data, mapping = mapping, fill = fill, colour = colour, ...)
  }else if(nDim ==  1){
    if(is.null(mapping))
      stop("mapping must be provided for 1d rectangelGate!")
      coord <- c(data@min, data@max)
      toRm <- is.infinite(coord)
      if(any(toRm))
        coord <- coord[!toRm] 
      #try to figure out on which dimension to plot the line
      axis <- as.character(mapping)
      axis <- gsub("`", "", axis)
      axis <- names(axis)[match(param, axis)]
      if(axis == "x")
        geom_vline(data = data, aes_q(xintercept = as.symbol(param)), fill = fill, colour = colour, ...)
      else if(axis == "y")
        geom_hline(data = data, aes_q(yintercept = as.symbol(param)), fill = fill, colour = colour, ...)
      else
        stop("not avalid axis!")
         
  }else
    stop("rectangelGate with dimension ", dDim, "is not supported!")
  
}

# 
# setOldClass("protofilterList", "proto")
# #' @import proto 
# GeomFilterList <- proto(ggplot2:::Geom, {
#   objname <- "gate"
#   default_stat <- function(.) ggplot2:::StatIdentity
#   #   
#   #   new <- function(., mapping=NULL, data=NULL, stat=NULL, position=NULL, ...){
#   #     do.call("layer", list(mapping=mapping, data=data, stat=stat, geom=., position=position, ...))
#   #   }
#   
#   #   draw_groups <- function(., ...) .$draw(...)
#   #   draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
#   #     data <- remove_missing(data, na.rm,
#   #                            c("x", "y", "size", "shape"), name = "geom_point")
#   #     if (empty(data)) return(zeroGrob())
#   #     
#   #     with(coord_transform(coordinates, data, scales),
#   #          ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape,
#   #                                         gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), fontsize = size * .pt)))
#   #     )
#   #   }
#   #   
#   #   draw_legend <- function(., data, ...) {
#   #     data <- aesdefaults(data, .$default_aes(), list(...))
#   #     
#   #     with(data,
#   #          pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape,
#   #                     gp=gpar(
#   #                       col=alpha(colour, alpha),
#   #                       fill=alpha(fill, alpha),
#   #                       fontsize = size * .pt)
#   #          )
#   #     )
#   #   }
#   #   
#   #   default_stat <- function(.) StatIdentity
#   #   required_aes <- c("x", "y")
#   #   default_aes <- function(.) aes(shape=16, colour="black", size=2, fill = NA, alpha = NA)
#   
# })
# 
# #' dummy fority method
# #' Just to get around the error from ggplot2::layer constructor
# #' @export
# # fortify.filterList <- function(model, data, ...){
# #   return(model)
# # }
# 
# # setMethod("+", signature = c("list", "protofilterList"), definition = function(e1, e2){
# # #   browser()
# # })
# 
