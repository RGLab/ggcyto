# copied from ggplot2 2.2.1 since it was removed from 2.3
ggplot.data.frame <- function (data, mapping = aes(), ..., environment = parent.frame()) 
{
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes() or `aes_()`.", 
         call. = FALSE)
  }
  p <- structure(list(data = data, layers = list(), scales = ggplot2:::scales_list(), 
                      mapping = mapping, theme = list(), coordinates = coord_cartesian(), 
                      facet = facet_null(), plot_env = environment), class = c("gg", 
                                                                               "ggplot"))
  p$labels <- ggplot2:::make_labels(mapping)
  set_last_plot(p)
  p
}
