#' logicle transformation.
#' 
#' @export
#' @importFrom scales trans_new
logicle_trans <- function(){
  trans_new("logicle", )
  
}

#' logicle transformation.
#' 
#' @export
#' @importFrom scales trans_new
fasinh_trans <- function(){
  trans_new("fasinh", transform = flowWorkspace:::.fasinh, inverse = flowWorkspace:::.fsinh)
  
}