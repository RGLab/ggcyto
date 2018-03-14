#' Popluation statistics layer
#' 
#' It is a virtual layer and will be instanatiated as geom_label layer within ggycto.+ operator.
#' 
#' So it is dedicated for ggcyto context and thus can't not be added to ggplot object directly. 
#' 
#' @param gate a 'filterList` or character (represent as a population node in GatingSet)
#'             if not supplied, ggcyto then tries to parse the gate from the first geom_gate layer.
#' @param negated whether the gate needs to be negated
#' @param adjust adjust the position of the centroid. from 0 to 1.             
#' @param label.padding,label.size arguments passed to geom_label layer
#' @param digits control the stats format
#' @param ... other arguments passed to geom_label layer            
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
#' #sometime the stats are not shown (or positoned not properly) due to the inaccurate default instrument measurement range
#' #stored in flow data. then it will be necessary to correct that by supplying the actual 'data_range'
#' fr <- getData(gs)[[1]]
#' rng <- range(fr, type = "data")
#' p + geom_gate("CD4") + geom_stats(data_range = rng)
geom_stats <- function(gate = NULL, ..., value = NULL, type = "percent", data_range = NULL, negated = FALSE, adjust = 0.5
                       , label.padding = unit(0.05, "lines"), label.size = 0, digits = 3){
  type <- match.arg(type, c("percent", "count"))
  # data_range can be passed in to prevent the data(gs or fs) to be evaluated by compute_stats
  structure(
    list(gate = gate, value = value, type = type, data_range = data_range, negated = negated, adjust = adjust, digits = digits
         , geom_label_params = list(label.padding = label.padding
                                    , label.size = label.size
                                    , ...
                                    )
      )
    , class = c("GeomStats", "ggcyto_virtual_layer")
  )  

  
  
  
}



