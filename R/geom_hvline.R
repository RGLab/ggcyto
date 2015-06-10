#' Vertical or horizontal line.
#'
#' This geom is based on the source code of ' \code{\link{geom_hline}} and \code{\link{geom_vline}}.
#'
#' The goal is to determine the line to be either vertial or horizontal based on the 1-d data provided in 
#' this layer. 
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "vline")}
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param position The position adjustment to use for overlapping points
#'    on this layer
#' @param ... other arguments passed on to \code{\link{layer}}. This can
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#' @param show_guide should a legend be drawn? (defaults to \code{FALSE})
#' @export
#' @examples
#' 
#' 
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' # vline
#' p + geom_hvline(data = data.frame(wt= 3))
#' # hline
#' p + geom_hvline(data = data.frame(mpg= 20))
geom_hvline <- function (mapping = NULL, data = NULL, position = "identity", show_guide = FALSE, ...) {
  obj <- GeomHVline$new(mapping = mapping, data = data, position = position, show_guide = show_guide, ...)
  
  
  # somehow applying this GeomHVline directly doesn't work 
  # it seems that the object created by GeomHVline$new will lose the overriden function
  # (no wonder ggplot is planning to deprecate proto with S3)
  obj$compute_aesthetics <- .my_compute_aesthetics
  obj
}

#' @import proto 
GeomHVline <- proto(ggplot2:::Geom, {
  objname <- "hvline"
  
  new <- function(., data = NULL, mapping = NULL, ...) {
    #inherit.aes must be TRUE to get original aes
    .super$new(., data = data, mapping = mapping, inherit.aes = TRUE, ...)
  }
  
  
  draw <- function(., data, scales, coordinates, ...) {
    
    ranges <- coord_range(coordinates, scales)
    
    # determien wether x or y 
    if("x"%in% colnames(data)){
      axis.used <- "x"
      axis.missing <- "y"
    }else{
      axis.used <- "y"
      axis.missing <- "x"
    }
    #fill values for the missing axis
    thisRange <- ranges[[axis.missing]]  
    data[[axis.missing]]    <- thisRange[1]
    data[[paste0(axis.missing, "end")]] <- thisRange[2]
    #fill end point for the used axis
    data[[paste0(axis.used, "end")]] <- data[[axis.used]]

    #remove Inf lines
    data <- data[!is.infinite(data[,axis.used]), ]
    GeomSegment$draw(unique(data), scales, coordinates)
  }
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"
  
})

#' override the original Layer$compute_aesthetics method so that it could make it for the layer
#' that contains 1d data but uses the 2d aes
#' 
#' This is the hook invoked by ggplot_build at early stage of plotting.
#' We need to match the 1d gate definition with the mapping
#' stored in the original aes (from 'plot$mapping') so that the line can be determined as either horizontal or vetical
#' 
#' And so far this is the best place where we can look up the original mapping and store the axis information.
#' Idealy it could have been be done at `map_statistic` step. Unfortunetly, the data would have already been generalized at that point and 
#' thus it won't be able to do the matching otherwise.  
#' 
#' It would be nice if we could also hack to associate pData from plot$data here. Unforutntely the data
#' is already facetted before this step and there is no way to access the facetted plot$data in order to
#' reassign the 'panel' info for layer data.
#' @importFrom plyr eval.quoted compact empty
.my_compute_aesthetics <- function(., data, plot) {
  
  aesthetics <- .$layer_mapping(plot$mapping)
  
  if (!is.null(.$subset)) {
    include <- data.frame(eval.quoted(.$subset, data, plot$env))
    data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
  }
  
  # Override grouping if set in layer.
  if (!is.null(.$geom_params$group)) {
    aesthetics["group"] <- .$geom_params$group
  }
  
  ggplot2:::scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
  
   
  #the hack:  
  # rm the missing axis from mapping aes so that 
  # it won't fail the following aesthetics evaluation 
  aes_matched <-  sapply(aesthetics, as.character) %in% colnames(data)
  aesthetics <- aesthetics[aes_matched]
  
  
  # Evaluate aesthetics in the context of their data frame
  evaled <- compact(
    eval.quoted(aesthetics, data, plot$plot_env))
  
  lengths <- vapply(evaled, length, integer(1))
  n <- if (length(lengths) > 0) max(lengths) else 0
  
  wrong <- lengths != 1 & lengths != n
  if (any(wrong)) {
    stop("Aesthetics must either be length one, or the same length as the data",
         "Problems:", paste(aesthetics[wrong], collapse = ", "), call. = FALSE)
  }
  
  if (empty(data) && n > 0) {
    # No data, and vectors suppled to aesthetics
    evaled$PANEL <- 1
  } else {
    evaled$PANEL <- data$PANEL
  }
  data.frame(evaled)
}
