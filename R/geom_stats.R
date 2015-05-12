#' Popluation statistics layer
#' 
#' It is a dummy layer that simply inherits GeomBText.
#' The actual data for geom_text layer will be instanatiated within ggycto.+ operatoer
#' by looking up the geom_gate layer.
#' So it is dedicated for ggcyto context and thus can't not be added to ggplot object directly. 
#' 
#' @inheritParams compute_stats
#' @export
#' @examples
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
#' p <- p + geom_hex(bins = 128)
#' p + geom_gate(rect.gates) + geom_stats()
geom_stats <- function(..., type = "percent"){
  GeomStats$new(..., type = type)
}

GeomStats <- proto(ggcyto:::GeomBText, {
  objname <- "popStats"
  
})
