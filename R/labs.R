#' Change axis labels and legend titles
#'
#' The actual labels text will be instantiated when it is added to ggcyto plot.
#'
#' @param labels default labels for x, y axis. Can be "channel" , "marker", or "both" (default)
#' @export
#' @examples
#' \dontrun{
#' 
#' # default is "both"
#' p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "3+") + geom_hex(bins = 64)
#' p
#' 
#' #use marker name as x,y labs
#' p + labs_cyto("marker")
#' 
#' use channel name as x,y labs
#' p + labs_cyto("channel")
#' }
labs_cyto <- function(labels = "both") {
  
  structure(list(labels = labels), class = "labs_cyto")
}
