.element_tree <- list(
                      limits = "data"
                      , facet = facet_wrap(~name, scales = "free") 
                      , hex_fill = scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral")), trans = "sqrt")  
                      )

#' Set ggcyto theme elements
#'
#'
#' Use this function to modify theme settings.
#'
#'
#' @section Theme elements:
#' The individual theme elements are:
#'
#' \tabular{ll}{
#'   limits             \tab can be "data"(default) or "instrument" or a list of actual numeric limits for x and y
#'                    (\code{list(x = c(0, 4000))}) \cr
#'   facet             \tab the regular facet object
#'   hex_fill             \tab default scale_fill_gradientn for geom_hex layer
#' }
#'
#' @param ... a list of element name, element pairings that modify the
#'   existing theme.
#'
#' @export
#' @examples
#' \dontrun{
#' p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "3+") 
#' # 2d plot 
#' p <- p + geom_hex(bins = 64)
#' p
#' 
#' #use instrument range by overwritting limits setting in the default theme
#' p + theme_ggcyto(limits = "instrument")
#'
#' #manually set limits
#' myTheme <- theme_ggcyto(limits = list(x = c(0,3.2e3), y = c(-10, 3.5e3)))
#'  p  + myTheme# or xlim(0,3.2e3) + ylim(-10, 3.5e3) 
#' }
theme_ggcyto <- function(...) {
  elements <- list(...)
  # Check that all elements have the correct class (element_text, unit, etc)
  mapply(validate_element, elements, names(elements))
  structure(elements, class = c("ggcyto_theme"))
    
}

#' Return The default ggcyto theme
#' @export
theme_ggcyto_default <- function(){
  do.call(theme_ggcyto, .element_tree)
}

validate_element <- function(el, elname) {
  eldef <- .element_tree[[elname]]
  
  if (is.null(eldef)) {
    stop('"', elname, '" is not a valid theme element name.')
  }
  
  invisible()
}

#' Reports whether x is a ggcyto_theme object
#' @param x An object to test
#' @export
is.ggcyto_theme <- function(x) inherits(x, "ggcyto_theme")

add_theme <- function(t1, t2, t2name) {
  if (!is.ggcyto_theme(t2)) {
    stop("Don't know how to add ", t2name, " to a ggcyto_theme object",
         call. = FALSE)
  }
  
  t1 <- modifyList(t1, t2)
  class(t1) <- c("ggcyto_theme")
  t1
}
