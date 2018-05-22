#' Overlay a population on an existing ggcyto plot analogous to backgating.
#' 
#' It is useful for "backgating" plots.
#' 
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
#' @return a geom_overlay layer
#' @examples 
#' library(ggcyto)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' p <- autoplot(gs, "CD3+")
#' 
#' # add a flowSet as the overlay
#' fs <- getData(gs, "DPT")
#' p + geom_overlay(data = fs, size = 0.3, alpha = 0.7)
#' 
#' # add overlay layer by gate name
#' p + geom_overlay(data = "DNT", size = 0.3, alpha = 0.7)
#' 
#' #add overlay for 1d densityplot
#' p <- ggcyto(gs, aes(x = CD4), subset = "CD3+") + geom_density(aes(y = ..count..))
#' p + geom_overlay("DNT", aes(y = ..count..), fill = "red")
geom_overlay <- function(data, ...)UseMethod("geom_overlay")

#' @export
#' @rdname geom_overlay
geom_overlay.default <- function(data, ...){
  
    stop("unsupported overlay data type: ", class(data), call. = FALSE)
}

#' @rdname geom_overlay
#' @export
geom_overlay.character <- function(data, ...){
  if(length(data) > 1)
    stop("More than one population names provided for geom_overlay. Please add one population per layer!")
  structure(
    list(node = data
         , overlay_params = list(...)
    )
    , class = c("overlay.node", "ggcyto_virtual_layer")
  )
}

#' @rdname geom_overlay
#' @export
geom_overlay.ncdfFlowList <- function(data, ...){
  geom_overlay.flowSet(data, ...)
  
}

#' @rdname geom_overlay
#' @export
geom_overlay.flowSet <- function(data, ...){
  
  structure(
    list(fs = data
         , overlay_params = list(...)
    )
    , class = c("overlay.fs", "ggcyto_virtual_layer")
  )
  
}

#' @rdname geom_overlay
#' @export
geom_overlay.flowFrame <- function(data, ...){
  geom_overlay.flowSet(data, ...)
}
