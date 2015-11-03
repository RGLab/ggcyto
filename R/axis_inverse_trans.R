#' Display axis in raw scales
#' 
#' It is essentially a dummy continous scale and will be instantiated 
#' by '+.ggcyto_GatingSet' with 'breaks` and 'lables' customized.
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @export
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
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

