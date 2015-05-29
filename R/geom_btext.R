#' modified geom_text that draws the rectangle background.
#'
#' Limitation: It does not support rotation.
#' 
#' @inheritParams geom_hvline
#' @param stat The statistical transformation to use on the data for this
#'    layer.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @export
#' @examples
#' \donttest{
#' ggplot(mtcars, aes(x=wt, y=mpg, label=rownames(mtcars))) + geom_btext(size = 5, fill = "gray")
#' }
geom_btext <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
parse = FALSE, ...) {
  GeomBText$new(mapping = mapping, data = data, stat = stat, position = position,
  parse = parse, ...)
}

GeomBText <- proto(ggplot2:::Geom, {
  objname <- "btext"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, ..., parse = FALSE, na.rm = FALSE) {
    data <- remove_missing(data, na.rm,
      c("x", "y", "label"), name = "geom_btext")

    lab <- data$label
    if (parse) {
      lab <- parse(text = lab)
    }
  
    with(coord_transform(coordinates, data, scales),
        btextGrob(lab, x, y, default.units="native",
        hjust=hjust, vjust=vjust, 
        gp = gpar(col = alpha(colour, alpha), fontsize = size * .pt
                  ,fontfamily = family, fontface = fontface, lineheight = lineheight)
        ,gp.rect = gpar(col = fill, fill = fill, alpha = bgalpha)
         , widthAdj = unit(0, "npc")
        , heightAdj = unit(0, "npc")
        )
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    with(data,
      textGrob("a", 0.5, 0.5, rot = 0,
      gp=gpar(col=alpha(colour, alpha), fontsize = size * .pt))
    )
  }


  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y", "label")
  default_aes <- function(.) aes(colour="black", fill = "white", bgalpha = 1, size = 4 , angle=0, hjust=0.5,
    vjust=0.5, alpha = NA, family="", fontface=1, lineheight=1.2)
  guide_geom <- function(x) "text"

})

