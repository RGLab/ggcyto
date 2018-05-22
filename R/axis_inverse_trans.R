#' Display ggcyto axis labels using their raw values (as stored in the data structure)
#' 
#' It is essentially a dummy continous scale and will be instantiated 
#' by '+.ggcyto_GatingSet' with 'breaks` and 'lables' customized.
#' 
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @export
#' @return a raw_scale object that inherits scale class.
#' @examples 
#'  dataDir <- system.file("extdata",package="flowWorkspaceData")
#'  gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#'  p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") + geom_hex(bins = 64)
#'  p <- p + geom_gate("CD4") + geom_stats() #plot CD4 gate and it is stats
#'  p
#'  p + axis_x_inverse_trans() #inverse transform the x axis into raw scale
axis_x_inverse_trans <- function(...){
    obj <- scale_x_continuous(...)
    class(obj) <- c(class(obj), "raw_scale")
    obj
}

#' @rdname axis_x_inverse_trans
#' @export
axis_y_inverse_trans <- function(...){
  obj <- scale_y_continuous(...)
  class(obj) <- c(class(obj), "raw_scale")
  obj
}

