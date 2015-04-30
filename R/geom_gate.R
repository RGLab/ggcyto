#' plot a flowCore gate 
#' 
#' Currently only rectangleGate (1d or 2d), polygonGate, ellipsoidGate are supported.
#' 
#' @param data a rectangleGate, polygonGate or ellipsoidGate
#' @export
geom_gate <- function(data, ...)UseMethod("geom_gate")

#' plot a polygonGate
#' 
#' @param The mapping aesthetic mapping
#' @param data a polygonGate
#' @param fill polygonGate is not filled by default
#' @param colour default is red
#' @export
geom_gate.polygonGate <- function(data, mapping = NULL, fill = "transparent", colour = "red", ...){
  
  geom_polygon(mapping = mapping, data = data , fill = fill, colour = colour, ...)  
}

#' plot a rectangleGate
#' 
#' It is coerced to polygonGate.
#' 
#' @param The mapping aesthetic mapping
#' @param gate a rectangleGate
#' @param fill rectangleGate is not filled by default
#' @param colour default is red
#' @export
geom_gate.rectangleGate <- function(data, mapping = NULL, fill = "transparent", colour = "red", ...){
  
  param <- parameters(data)
  nDim <- length(param)
  l.b <- data@min
  r.t <- data@max
  if (nDim ==  2){
    
    l.t <- c(l.b[1], r.t[2])
    r.b <- c(r.t[1], l.b[2])
    
    boundaries <- list(l.b, l.t, r.t, r.b)
    boundaries <- do.call(rbind, boundaries)
    g <- polygonGate(boundaries)
    geom_gate(data = g, mapping = mapping, fill = fill, colour = colour, ...)
  }else if(nDim ==  1){
    if(is.null(mapping))
      stop("mapping must be provided for 1d rectangelGate!")
      coord <- c(l.b, r.t)
    
      toRm <- is.infinite(coord)
      if(any(toRm))
        coord <- coord[!toRm] 
      #try to figure out which dimension to plot the line
      axis <- as.character(mapping)
      axis <- gsub("`", "", axis)
      axis <- names(axis)[match(param, axis)]
      if(axis == "x")
        geom_vline(xintercept = coord, fill = fill, colour = colour, ...)
      else if(axis == "y")
        geom_hline(yintercept = coord, fill = fill, colour = colour, ...)
      else
        stop("not avalid axis!")
         
  }else
    stop("rectangelGate with dimension ", dDim, "is not supported!")
  
}
