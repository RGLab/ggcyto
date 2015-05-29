#' Display axis in raw scales
#' 
#' @param gs GatingSet
#' @param channel the channel or marker name
#' @param ... common continuous scale parameters passed to 'continuous_scale' (not used currently)
#' @export
scale_x_raw <- function(...){
    obj <- scale_x_continuous(...)
    class(obj) <- c(class(obj), "raw_scale")
    obj
}

#' @rdname scale_x_raw
#' @export
scale_y_raw <- function(...){
  obj <- scale_y_continuous(...)
  class(obj) <- c(class(obj), "raw_scale")
  obj
}

