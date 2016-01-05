#' Change axis labels and legend titles
#'
#' The actual labels text will be instantiated when it is added to ggcyto plot.
#'
#' @param labels default labels for x, y axis. Can be "channel" , "marker", or "both" (default)
#' @export
#' @return a list 
#' @examples
#' 
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' 
#' # default is "both"
#' p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") + geom_hex(bins = 64)
#' p
#' 
#' #use marker name as x,y labs
#' p + labs_cyto("marker")
#' 
#' use channel name as x,y labs
#' p + labs_cyto("channel")
labs_cyto <- function(labels = "both") {
  
  structure(list(labels = labels), class = "labs_cyto")
}
