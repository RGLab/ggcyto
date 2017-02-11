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
setClass("ggcyto_GatingLayout", contains = "list", slots = c(arrange.main = "character"))
#' @export
#' @rdname ggcyto
setClass("ggcyto_tsne", contains = "ggcyto_GatingSet", slots = c(state = "environment"))

setClass("GatingSet_tsne", contains = "GatingSet", slots = c(tsne_params = "list"))

