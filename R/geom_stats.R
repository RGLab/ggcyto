#' Popluation statistics layer
#' 
#' It is a virtual layer and will be instanatiated as geom_label layer within ggycto.+ operator.
#' 
#' So it is dedicated for ggcyto context and thus can't not be added to ggplot object directly. 
#' 
#' @param gate a 'filterList` or character (represent as a population node in GatingSet)
#'             if not supplied, ggcyto then tries to parse the gate from the first geom_gate layer.
#' @param adjust adjust the position of the centroid. from 0 to 1.             
#' @param label.padding,label.size arguments passed to geom_label layer            
#' @param ... other arguments passed to geom_label layer            
#' @inheritParams compute_stats
#' @export
#' @return  a geom_popStats layer 
#' @examples
#' \dontrun{
#' 
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
#' p <- p + geom_hex(bins = 128)
#' p + geom_gate(rect.gates) + geom_stats()
#' }
geom_stats <- function(gate = NULL, ..., value = NULL, type = "percent", data_range = NULL, adjust = 0.5
                       , label.padding = unit(0.05, "lines"), label.size = 0){
  type <- match.arg(type, c("percent", "count"))
  # data_range can be passed in to prevent the data(gs or fs) to be evaluated by compute_stats
  structure(
    list(gate = gate, value = value, type = type, data_range = data_range, adjust = adjust
         , geom_label_params = list(label.padding = label.padding
                                    , label.size = label.size
                                    , ...
                                    )
      )
    , class = c("GeomStats", "ggcyto_virtual_layer")
  )  

  
  
  
}



