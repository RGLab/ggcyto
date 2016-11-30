#' @include labs.R
#' @importFrom RColorBrewer brewer.pal
.element_tree <- function(){#don't want to make it a global variable 
                            #since it will be evaluated and stored in package during the installation
                            #and some stored ggplot2 layer objects may become obsolete overtime as ggplot2 upgrades
                 list(
                      limits = "data" #or "instrument"
                      , facet = facet_wrap(~name, scales = "free") 
                      , hex_fill = scale_fill_gradientn(colours = rev(brewer.pal(11, "Spectral")), trans = "sqrt")  
                      , lab = labs_cyto("both") 
                      )
}
.lazy_element <- c("limits")
#' Set some default parameters for ggcyto
#'
#'
#' Use this function to modify ggcyto parameters 
#' These are the regular (or to be instantiated as) scales, labs, facet objects.
#' They can be added as a single layer to the plot for the convenience.
#'
#' @section elements:
#' The individual elements are:
#'
#' \tabular{ll}{
#'   limits             \tab can be "data"(default) or "instrument" or a list of numeric limits for x and y
#'                    (e.g. \code{list(x = c(0, 4000))}) \cr
#'   facet             \tab the regular facet object \cr
#'   hex_fill             \tab default scale_fill_gradientn for geom_hex layer \cr
#'   lab              \tab labs_cyto object
#'   
#' }
#'
#' @param ... a list of element name, element pairings that modify the
#'   existing parameter settings
#' @return a list of new settings for ggycto
#' @export
#' @examples 
#' library(ggcyto)
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' 
#' p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") 
#' # 2d plot 
#' p <- p + geom_hex(bins = 64)
#' p
#' 
#' #use instrument range by overwritting the default limits settings
#' p + ggcyto_par_set(limits = "instrument")
#'
#' #manually set limits
#' myPars <- ggcyto_par_set(limits = list(x = c(0,3.2e3), y = c(-10, 3.5e3)))
#'  p  + myPars# or xlim(0,3.2e3) + ylim(-10, 3.5e3) 
ggcyto_par_set <- function(...) {
  elements <- list(...)
  # Check that all elements have the correct class (element_text, unit, etc)
  mapply(validate_element, elements, names(elements))
  structure(elements, class = c("ggcyto_par"))
    
}

#' Return The default ggcyto settings
#' @return a list of default settings for ggycto
#' @export
#' @examples
#' ggcyto_par_default()
ggcyto_par_default <- function(){
  do.call(ggcyto_par_set, .element_tree())
}

validate_element <- function(el, elname) {
  eldef <- .element_tree()[[elname]]
  
  if (is.null(eldef)) {
    stop('"', elname, '" is not a valid ggcyo parameter element! Print the default parameters by "ggcyto_par_default()"')
  }
  
  invisible()
}

#' Reports whether x is a ggcyto_par object
#' @return TRUE or FALSE
#' @param x An object to test
#' @examples 
#' myPar <- ggcyto_par_set(limits = "instrument")
#' is.ggcyto_par(myPar)
#' @export
is.ggcyto_par <- function(x) {
  inherits(x, "ggcyto_par")
}

add_par <- function(t1, t2, t2name) {
  if (!is.ggcyto_par(t2)) {
    stop("Don't know how to add ", t2name, " to a ggcyto_par object",
         call. = FALSE)
  }
  
  t1 <- modifyList(t1, t2)
  class(t1) <- c("ggcyto_par")
  t1
}
