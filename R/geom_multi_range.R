#' Draw multi-ranges as multiple rectangles on 1D or 2D plot
#'
#' This geom is based on the source code of ' \code{\link{geom_rect}}
#'
#' The goal is to determine the line to be either vertial or horizontal based on the data provided in 
#' this layer. Also convert input 1D intervals to geom_rect acceptable shapes
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
#' @importFrom rlang list2
#' @return a geom_rect layer
geom_multi_range <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      linejoin = "mitre",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
 obj = layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMultiRange,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )

  obj$compute_aesthetics <- .my_compute_aesthetics
  obj
}


                      


GeomMultiRange<- ggproto("GeomMultiRange", Geom,
  default_aes = aes(colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1,
    alpha = NA),


  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
    data <- ggplot2:::check_linewidth(data, ggplot2:::snake_class(self))
    
    # determien whether x or y 
    if("x"%in% colnames(data)){
      axis.used <- "x"
      axis.missing <- "y"
    }else{
      axis.used <- "y"
      axis.missing <- "x"
    }                        
    # convert range gate to rect format, that is "xmin", "xmax", "ymin", "ymax"
    multi_intervals=data[[axis.used]]
    num_breaks=length(multi_intervals)
    start=multi_intervals[seq(1, num_breaks, 2)]
    end=multi_intervals[seq(2, num_breaks, 2)]
    data[[axis.used]] <- NULL
    panel_data = data[1,]
    data = data.frame(start=start,end=end)
    names(data) <-c(paste0(axis.used,"min"),paste0(axis.used,"max"))
    data[[paste0(axis.missing,"min")]] <--Inf
    data[[paste0(axis.missing,"max")]] <-Inf
    data=cbind(data, panel_data)

    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )
      index <- rep(seq_len(nrow(data)), each = 4)

      new <- data[index, aesthetics, drop = FALSE]
      new$x <- ggplot2:::vec_interleave(data$xmin, data$xmax, data$xmax, data$xmin)
      new$y <- ggplot2:::vec_interleave(data$ymax, data$ymax, data$ymin, data$ymin)
      new$group <- index

      ggname("geom_rect", GeomPolygon$draw_panel(
        new, panel_params, coord, lineend = lineend, linejoin = linejoin
      ))
    } else {
      coords <- coord$transform(data, panel_params)
      ggplot2:::ggname("geom_rect", grid::rectGrob(
        coords$xmin, coords$ymax,
        width = coords$xmax - coords$xmin,
        height = coords$ymax - coords$ymin,
        default.units = "native",
        just = c("left", "top"),
        gp = grid::gpar(
          col = coords$colour,
          fill = alpha(coords$fill, coords$alpha),
          lwd = coords$linewidth * .pt,
          lty = coords$linetype,
          linejoin = linejoin,
          lineend = lineend
        )
      ))
    }
  },

  draw_key = draw_key_polygon,

  rename_size = TRUE
)