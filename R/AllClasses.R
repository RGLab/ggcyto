if (exists("class_ggplot", envir = asNamespace("ggplot2"))) {
  # Should equal `S7:::class_dispatch(class_ggplot)`
  ggplot_class <- c("ggplot2::ggplot", "ggplot2::gg", "S7_object")
} else {
  ggplot_class <- c("gg", "ggplot")
}
setOldClass(ggplot_class)

#' @export
#' @rdname ggcyto
setClass("ggcyto", contains = ggplot_class)
#' @export
#' @rdname ggcyto
setClass("ggcyto_flowSet", contains = "ggcyto")
#' @export
#' @rdname ggcyto
setClass("ggcyto_GatingSet", contains = "ggcyto_flowSet")
#' @export
#' @rdname ggcyto
setClass("ggcyto_GatingLayout", contains = "list", slots = c(arrange.main = "character"))

