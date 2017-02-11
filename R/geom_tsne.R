geom_tsne_poly <- function(mapping = aes(colour = poly), degreeFilter = 3, ...){
  layer <- geom_point(mapping, ...)
  class(layer) <- c("geom.tsne.poly", class(layer))
  
}