#' overloaded '+' method for ggcyto_tsne
#' 
#' It tries to parse geom_tsne_* layers added ggcyto_tsne object.
#' 
#' @param e1 An object of class \code{ggcyto_tsne}
#' @param e2 A component to add to \code{e1}
#' @return ggcyto_tsne object
#' @rdname ggcyto_tsne_add
#' @importFrom plyr defaults
#' @export
`+.ggcyto_tsne` <- function(e1, e2){
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))
  
  if      (is.ggcyto_par(e1))  add_par(e1, e2, e2name)
  else if (is.ggcyto_tsne(e1)) add_ggcyto_tsne(e1, e2, e2name)
  
}

#' Reports whether x is a ggcyto_tsne object
#' @param x An object to test
#' @return TRUE or FALSE
#' @export
is.ggcyto_tsne <- function(x){
  inherits(x, "ggcyto_tsne")
}
#' @rdname ggcyto_tsne_add
#' @export
setMethod("+", c("ggcyto_tsne"), `+.ggcyto_tsne`)


add_ggcyto_tsne <- function(e1, e2, e2name){
  gs <- e1[["data"]]
  frm <- getFlowFrame(gs)
  mycolor <- rev(brewer.pal(11, "Spectral"))
  
  
  if(is(e2, "geom.tsne.degree")){
    thisCall <- quote(geom_point(aes(colour = degree)))
    thisCall <- as.call(c(as.list(thisCall)
                          , list(size = e2[["size"]], alpha = e2[["alpha"]])
                          , e2[["geom_point_args"]]))
    e2.new <- eval(thisCall)
    e1 <- ggplot2:::`+.gg`(e1, e2.new)  
    
    
    e1 <- ggplot2:::`+.gg`(e1, scale_color_gradientn(colours = mycolor))  
    e1 <- ggplot2:::`+.gg`(e1, labs(title = "Degree of functionality"))
    return (e1)
  }else if(is(e2, "geom.tsne.marker")){
    #parse marker name
    marker <- getChannelMarker(frm, e2[["marker"]])[["desc"]]
    
    thisCall <- quote(geom_point())
    thisCall <- as.call(c(as.list(thisCall)
                          , list(mapping = aes_q(colour = as.symbol(marker))
                                  ,size = e2[["size"]]
                                 , alpha = e2[["alpha"]])
                          , e2[["geom_point_args"]]))
    e2.new <- eval(thisCall)
    e1 <- ggplot2:::`+.gg`(e1, e2.new)  
    
    
    e1 <- ggplot2:::`+.gg`(e1, scale_color_gradientn(colours = mycolor, trans = "log10"))  
    e1 <- ggplot2:::`+.gg`(e1, labs(title = marker))
    return (e1)
  }else if(is(e2, "geom.tsne.poly")){
    e1[["data"]]@tsne_params[["degree.filter"]] = e2[["degree"]]
    e1[["data"]]@tsne_params[["polycount.filter"]] = e2[["count"]]
    thisCall <- quote(geom_point(aes(colour = poly)))
    thisCall <- as.call(c(as.list(thisCall)
                          , list(size = e2[["size"]]
                                 , alpha = e2[["alpha"]])
                          , e2[["geom_point_args"]]))
    e2.new <- eval(thisCall)
    e1 <- ggplot2:::`+.gg`(e1, e2.new)  
    
    e1 <- ggplot2:::`+.gg`(e1, labs(title = "Polyfunctionality"))
    return (e1)
  }
}