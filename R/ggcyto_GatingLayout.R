#' print method for ggcyto_gate_layout class
#' @inheritParams ggcyto_arrange
#' @export
#' @return nothing
#' @method print ggcyto_GatingLayout
print.ggcyto_GatingLayout <- function(x, ...){
  gt <- ggcyto_arrange(x, ...)
  plot(gt)
}


#' Arrange a list of ggplot objects into gtable
#' 
#' It is usually implicitly invoked by print and show method and can be called by user when the further manipulation is needed, 
#' 
#' @param x ggcyto_gate_layout, which is essentially a list of ggplot objects that were previously stored as ggcyto_gate_layout object by autoplot function.
#' @param ... other arguments passed to arrangeGrob
#' @export
#' @return gtable
#' @examples 
#' \dontrun{
#' # get ggcyto_GatingLayout object from first sample
#' res <- autoplot(gs[[1]], nodes, bins = 64)
#' class(res)
#' # arrange it as one-row gtable object 
#' gt <- ggcyto_arrange(res, nrow = 1)
#' gt
#' # do the same to the second sample
#' gt2 <- ggcyto_arrange(autoplot(gs[[2]], nodes, bins = 64), nrow = 1)
#' # combine the two and print it on the sampe page
#' gt3 <- gridExtra::gtable_rbind(gt, gt2)
#' plot(gt3)
#' }
ggcyto_arrange <- function(x, ...){
  #must do the conversion to ggplot here since + require ggcyto object
  for(j in seq_along(x)){
    # browser()
    p <- x[[j]]
    popName <- attr(p$data, "strip.text")
    p <- as.ggplot(p)

    p$data[, name:= popName]

    for(i in seq_along(p$layers)){
      if(!ggplot2:::is.waive(p$layers[[i]][["data"]])){

        p$layers[[i]][["data"]][, name:= popName]
      }

    }

    x[[j]] <- p
  }

  arrange.main <- x@arrange.main
  arrangeGrob(grobs = x, top = arrange.main, ...)
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
#' #autplot for GatingSet
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' gh <- gs[[1]]
#' p <- autoplot(gh)
#' class(p)
#' # customize the font size of strip text for each ggcyo plots contained in GatingLayout object
#' p + theme(strip.text = element_text(size = 14))
`+.ggcyto_GatingLayout` <- function(e1, e2){

  for(i in seq_along(e1)){
    e1[[i]] <- e1[[i]] + e2
  }
  e1
}
#' @rdname plus-.ggcyto_GatingLayout
setMethod("+", "ggcyto_GatingLayout", `+.ggcyto_GatingLayout`)

