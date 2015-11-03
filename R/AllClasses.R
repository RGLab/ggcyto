setOldClass(c("gg", "ggplot"))
#' @export
#' @rdname ggcyto
setClass("ggcyto", contains = c("gg", "ggplot"))
#' @export
#' @rdname ggcyto
setClass("ggcyto_flowSet", contains = "ggcyto")
#' @export
#' @rdname ggcyto
setClass("ggcyto_GatingSet", contains = "ggcyto_flowSet")
#' @export
#' @rdname ggcyto
setClass("ggcyto_gate_layout", contains = "list", slots = c(arrange.main = "character"))

