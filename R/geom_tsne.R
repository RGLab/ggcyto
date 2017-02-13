#' Some geom_layers to be added to  "ggcyto_tsne" object
#' 
#' They are the abstract layers that will be instantiated as the "geom_point" 
#' by overloaded "+" operator when they are added to \link{ggcyto_tsne} object.
#' with some customized settings to visualize the "Degree of Functionality", "polyfunctional" or "marker signal".
#' 
#' See examples in \link{stat_tsne}.
#' 
#' @param size,alpha,... arguments passed to geom_point
#' @rdname geom_tsne
#' @export
geom_tsne_degree <- function(size = 1.5, alpha = 0.5, ...){
  
  structure(
    list(size = size
         , alpha = alpha
         , geom_point_args = list(...)
    )
    , class = c("geom.tsne.degree", "ggcyto_virtual_layer")
  )
}

#' @param marker  one of the specific marker name used in "stat_tsne". It will be used as "colour" aesthetic for geom_tsne_marker layer. 
#' @rdname geom_tsne
#' @export
geom_tsne_marker <- function(marker, size = 1.5, alpha = 0.5, ...){
  if(missing(marker))
    stop("marker is missing!")
  structure(
    list(marker = marker
         , size = size
         , alpha = alpha
         , geom_point_args = list(...)
    )
    , class = c("geom.tsne.marker", "ggcyto_virtual_layer")
  )
}

#' @param degree a threshold that sets the minimum degrees of functionality to be displayed in geom_tsne_poly layer
#' @param count a threshold that sets the minimum cell count of polyfunctionality to be displayed in geom_tsne_poly layer
#' @rdname geom_tsne
#' @export
geom_tsne_poly <- function(degree = 3, count = 20, size = 1.5, alpha = 0.5, ...){
  
  structure(
    list(degree = degree
         , count = count
         , size = size
         , alpha = alpha
         , geom_point_args = list(...)
    )
    , class = c("geom.tsne.poly", "ggcyto_virtual_layer")
  )
}
