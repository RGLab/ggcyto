#' print method for ggcyto_gate_layout class
#' It calls arrangeGrob to arrange a list of ggplot objects stored as ggcyto_gate_layout object
#' @param x ggcyto_gate_layout, which is essentially a list of ggplot objects
#' @param ... not used
#' @export
#' @return nothing
print.ggcyto_GatingLayout <- function(x, ...){
  arrange.main <- x@arrange.main
  plot(do.call(arrangeGrob, c(grobs = x, top = arrange.main)))  
}

#' @param object ggcyto_GatingLayout
#' @rdname print.ggcyto_GatingLayout
#' @method show ggcyto_GatingLayout
#' @export
show.ggcyto_GatingLayout <- function(object){print(object)}

#' @rdname print.ggcyto_GatingLayout
#' @method show ggcyto
#' @export
setMethod("show", "ggcyto_GatingLayout", show.ggcyto_GatingLayout)


#' overloaded '+' method for ggcyto_gate_layout
#' 
#' It adds the layer specified by 'e2' to each individual ggplot object stored in ggcyto_gate_layout
#' @param e1 ggcyto_gate_layout
#' @param e2 any ggplot layer
#' @return a modified ggcyto_gate_layout object
#' @export
#' @return a GatingLayout object
#' @examples 
#' \dontrun{
#' #autplot for GatingSet
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' gh <- gs[[1]]
#' p <- autoplot(gh)
#' class(p)
#' # customize the font size of strip text for each ggcyo plots contained in GatingLayout object
#' p + theme(strip.text = element_text(size = 14))
#' }
`+.ggcyto_GatingLayout` <- function(e1, e2){
  # browser()
  for(i in seq_along(e1)){
    e1[[i]] <- e1[[i]] + e2
  }
  e1
}
#' @rdname plus-.ggcyto_GatingLayout
setMethod("+", "ggcyto_GatingLayout", `+.ggcyto_GatingLayout`)

