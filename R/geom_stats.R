#' Add a popluation statistics layer to a ggcyto plot.
#' 
#' This is a virtual layer and will be instanatiated as geom_label layer within ggycto.+ operator.
#' 
#' So it is dedicated for ggcyto context and thus cannot be added to ggplot object directly. 
#' 
#' @param gate a 'filterList` or character (represent as a population node in GatingSet)
#'             if not supplied, ggcyto then tries to parse the gate from the first geom_gate layer.
#' @param negated whether the gate needs to be negated
#' @param adjust see details for \code{\link{stat_position}}
#' @param location see details for \code{\link{stat_position}}
#' @param label.padding,label.size arguments passed to geom_label layer
#' @param digits control the stats format
#' @param ... other arguments passed to geom_label layer
#'          
#' @inheritParams compute_stats
#' @export
#' @return  a geom_popStats layer 
#' @examples
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") + geom_hex(bins = 64)
#' p
#' # add gate and stats layer
#' p + geom_gate("CD4") + geom_stats()
#' 
#' # display gate name
#' p + geom_gate(c("CD4", "CD8")) + geom_stats(type = "gate_name")
#' # display gate name and percent
#' p + geom_gate(c("CD4", "CD8")) + geom_stats(type = c("gate_name", "percent"))
geom_stats <- function(gate = NULL, ..., value = NULL, type = "percent", negated = FALSE, adjust = 0.5
                       , location = "gate", label.padding = unit(0.05, "lines"), label.size = 0, digits = 3){
  type <- unlist(lapply(type, function(stat_type)match.arg(stat_type, c("percent", "count", "gate_name"))))
  location <- unlist(lapply(location, function(location_type)match.arg(location_type, c("gate", "data", "plot", "fixed"))))
  
  structure(
    list(gate = gate, value = value, type = type, negated = negated, adjust = adjust, location = location, digits = digits
         , geom_label_params = list(label.padding = label.padding
                                    , label.size = label.size
                                    , ...
                                    )
      )
    , class = c("GeomStats", "ggcyto_virtual_layer")
  )  

  
  
  
}

#' clear all the geom_stats() layer previously added
#' 
#' @export
#' @examples 
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' #autoplot display pop stats by default
#' p <- autoplot(gs, "CD4")
#' #it is easy to remove the default stats
#' p <- p + stats_null()
#' #and add a new one
#' p <- p + geom_stats(type = "count")
stats_null <- function(){
  structure(
    list()
    , class = c("statsNull", "ggcyto_virtual_layer")
  )  
}
