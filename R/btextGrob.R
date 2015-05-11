##' Grid Text with a Background (modified based on Kmisc::grid.text2)
##' 
##' 
##' @importFrom grid unit gpar is.unit stringWidth stringHeight grid.rect grid.text gList
##' @param label A character or \code{\link{expression}} vector. 
##' Other objects are coerced by \code{as.graphicsAnnot}.
##' @param x A numeric vector or unit object specifying x-values.
##' @param y A numeric vector or unit object specifying y-values.
##' @param just The justification of the text relative to its (x, y) location. 
##' If there are two values, the first value specifies horizontal justification 
##' and the second value specifies vertical justification. Possible string values 
##' are: \code{"left"}, \code{"right"}, \code{"centre"}, \code{"center"}, 
##' \code{"bottom"}, and \code{"top"}. 
##' For numeric values, 0 means left alignment and 1 means right alignment.
##' @param hjust A numeric vector specifying horizontal justification. 
##' If specified, overrides the \code{just} setting.
##' @param vjust A numeric vector specifying vertical justification. 
##' If specified, overrides the \code{just} setting.
##' @param check.overlap A logical value to indicate whether to check for 
##' and omit overlapping text.
##' @param default.units A string indicating the default units to use 
##' if \code{x} or \code{y} are only given as numeric vectors.
##' @param name A character identifier.
##' @param gp An object of class \code{gpar}, typically the output from a call to the function
##' \code{gpar}. This is basically a list of graphical parameter settings.
##' @param draw A logical value indiciating whether graphics output should be produced.
##' @param vp A Grid viewport object (or \code{NULL}).
##' @param widthAdj A width adjustment parameter, to help control how much horizontal padding
##' there should be between the text and the background rectangle.
##' @param heightAdj A height adjustment parameter, to help control how much
##' vertical padding there should be between the text and the background rectangle.
##' @seealso \code{\link{grid.text}} and \code{\link{grid.rect}}
##' @export
btextGrob <- function(label, 
                      x=unit(0.5, "npc"), 
                      y=unit(0.5, "npc"),
                      just="centre",
                      hjust=NULL,
                      vjust=NULL,
                      check.overlap=FALSE,
                      default.units="npc",
                      name=NULL,
                      gp=gpar(col="black", 
                              fill="grey92", 
                              lineend="butt", 
                              linejoin="round"
                      ),
                      gp.rect=gpar(col="white", 
                              fill="white", 
                              lineend="butt", 
                              linejoin="round"
                      ),
                      vp=NULL,
                      widthAdj=unit(0.05, "npc"),
                      heightAdj=unit(0.05, "npc")
) {
  
  if( !is.unit(x) ) {
    x <- unit(x, "npc")
  }
  
  if( !is.unit(y) ) {
    y <- unit(y, "npc")
  }
  
  x_rect <- x
  y_rect <- y
#   browser()
  fontsize <- gp$fontsize
  size <- fontsize/ggplot2:::.pt
  ratio <- size/5
  strWidth <- ratio * stringWidth(label)
  strHeight <- ratio * stringHeight(label)
  
  if( isTRUE( just[1] == "left" ) | isTRUE( hjust == 0 ) ) {
    x_rect <- x - widthAdj * 0.5 
  }
  
  if( isTRUE( just[1] == "right" ) | isTRUE( hjust == 1 ) ) {
    x_rect <- x + widthAdj * 0.5
  }
  
  rg <- grid.rect(x=x_rect, 
            y=y_rect, 
            width=strWidth + widthAdj, 
            height=strHeight + heightAdj,
            just=just,
            hjust=hjust,
            vjust=vjust,
            default.units=default.units,
            name=name,
            gp=gp.rect,
            draw=FALSE
  )
  
  tg <- grid.text(label=label,
            x=x, 
            y=y, 
            just=just,
            hjust=hjust,
            vjust=vjust,
            rot=0,
            check.overlap=check.overlap,
            default.units=default.units,
            name=name,
            gp=gp,
            draw=FALSE,
            vp=vp
  )
#   browser()
  gList(rg,tg)
}
