#' Popluation statistics layer
#' 
#' It is a dummy layer that simply inherits GeomBText.
#' The actual data for geom_text layer will be instanatiated within ggycto.+ operatoer
#' by looking up the geom_gate layer.
#' So it is dedicated for ggcyto context and thus can't not be added to ggplot object directly. 
#' 
#' @param gate a 'filterList` or character (represent as a population node in GatingSet)
#'             if not supplied, ggcyto then tries to parse the gate from the first geom_gate layer
#' @param adjust adjust the position of the centroid. from 0 to 1.             
#' @inheritParams compute_stats
#' @export
#' @examples
#' \dontrun{
#' 
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
#' p <- p + geom_hex(bins = 128)
#' p + geom_gate(rect.gates) + geom_stats()
#' }
geom_stats <- function(gate = NULL, ..., value = NULL, type = "percent", data_range = NULL, adjust = 0.5){
  type <- match.arg(type, c("percent", "count"))
  # data_range can be passed in to prevent the data(gs or fs) to be evaluated by compute_stats
  GeomStats$new(gate = gate, ...,  value = value, type = type, data_range = data_range, adjust = adjust)
}

GeomStats <- proto(ggcyto:::GeomBText, {
  objname <- "popStats"
  
})
