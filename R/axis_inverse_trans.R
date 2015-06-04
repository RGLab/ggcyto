#' Display axis in raw scales
#' 
#' It is essentially a dummy continous scale and will be instantiated 
#' by '+.ggcyto_GatingSet' with 'breaks` and 'lables' customized.
#' @param gs GatingSet
#' @param channel the channel or marker name
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @export
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

