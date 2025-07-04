# copied from ggplot2 2.2.1 since it was removed from 2.3
ggplot.data.frame <- function (data, mapping = aes(), ..., environment = parent.frame())
{

  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes() or `aes_()`.",
         call. = FALSE)
  }

  class_ggplot <- get0("class_ggplot", asNamespace("ggplot2"))
  if (is.function(class_ggplot)) {
    p <- class_ggplot(
      data = data,
      mapping = mapping,
      plot_env = environment
    )
    class(p) <- union(union(c("ggplot2::ggplot", "ggplot"), class(p)), "gg")
  } else {
    p <- structure(
      list(
        data = data,
        layers = list(),
        scales = ggplot2:::scales_list(),
        mapping = mapping,
        theme = list(),
        coordinates = coord_cartesian(),
        facet = facet_null(),
        plot_env = environment,
        guides = ggplot2:::guides_list()
      ),
      class = c("gg", "ggplot")
    )
    p$labels <- ggplot2:::make_labels(mapping)
  }
  set_last_plot(p)
  p
}
