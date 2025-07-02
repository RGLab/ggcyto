ggplot_is_S7 <- exists("class_ggplot", envir = asNamespace("ggplot2"))

if (ggplot_is_S7) {
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

if (ggplot_is_S7) {
  setAs(
    from = "ggplot2::ggplot",
    to = "ggcyto_flowSet",
    function(from) {
      class(from) <- union(c("ggcyto_flowSet", "ggcyto"), class(from))
      from
    }
  )

  setAs(
    from = "ggcyto_flowSet",
    to = "ggcyto_GatingSet",
    function(from) {
      class(from) <- union("ggcyto_GatingSet", class(from))
      from
    }
  )
}

