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
#' @param show.legend should a legend be drawn? (defaults to \code{FALSE})
#' @export
#' @return a geom_hvline layer
#' @examples
#' 
#' 
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' # vline
#' p + geom_hvline(data = data.frame(wt= 3))
#' # hline
#' p + geom_hvline(data = data.frame(mpg= 20))
geom_hvline <- function (mapping = NULL, data = NULL, position = "identity", show.legend = FALSE, ...) {
  obj <- layer(geom = GeomHVline, mapping = mapping, data = data
               , position = position, show.legend = show.legend
               , stat = StatIdentity
               , params  = list(na.rm = FALSE
                                ,...)
  )
  obj$compute_aesthetics <- .my_compute_aesthetics
  obj
}

GeomHVline <- ggproto("hvline", Geom,
                    draw_panel = function(data, panel_scales, coord) {
                      ranges <- coord$range(panel_scales)
                      
                      
                      # determien whether x or y 
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
                      #fill values for the used axis
                      data[[paste0(axis.used, "end")]] <- data[[axis.used]] #<- data[[paste0(axis.used, "intercept")]]
                      
                      #remove Inf lines
                      data <- data[!is.infinite(data[,axis.used]), ] 
                      
                      GeomSegment$draw_panel(unique(data), panel_scales, coord)
                    },
                    
                    default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA)
                    # ,required_aes = "yintercept"
                    
                    ,draw_key = draw_key_path
)

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
.my_compute_aesthetics = function(self, data, plot) {
  # For annotation geoms, it is useful to be able to ignore the default aes
  if (self$inherit.aes) {
    aesthetics <- defaults(self$mapping, plot$mapping)
  } else {
    aesthetics <- self$mapping
  }
  
  # Drop aesthetics that are set or calculated
  set <- names(aesthetics) %in% names(self$aes_params)
  calculated <- ggplot2:::is_calculated_aes(aesthetics)
  aesthetics <- aesthetics[!set & !calculated]
  
  # Override grouping if set in layer
  if (!is.null(self$geom_params$group)) {
    aesthetics[["group"]] <- self$aes_params$group
  }
  
  ggplot2:::scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
  #the two lines of hack for hvline:  
  # rm the missing axis from mapping aes so that 
  # it won't fail the following aesthetics evaluation 
  aes_matched <-  sapply(aesthetics, quo_name) %in% colnames(data)
  aesthetics <- aesthetics[aes_matched]
  
  # Evaluate and check aesthetics
  aesthetics <- compact(aesthetics)
  evaled <- lapply(aesthetics, rlang::eval_tidy, data = data)
  
  n <- nrow(data)
  if (n == 0) {
    # No data, so look at longest evaluated aesthetic
    if (length(evaled) == 0) {
      n <- 0
    } else {
      n <- max(vapply(evaled, length, integer(1)))
    }
  }
  ggplot2:::check_aesthetics(evaled, n)
  
  # Set special group and panel vars
  if (empty(data) && n > 0) {
    evaled$PANEL <- 1
  } else {
    evaled$PANEL <- data$PANEL
  }
  evaled <- lapply(evaled, unname)
  evaled <- ggplot2:::as_gg_data_frame(evaled)
  evaled <- ggplot2:::add_group(evaled)
  evaled
}
