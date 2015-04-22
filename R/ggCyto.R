fortify.flowFrame <- function(model, data, ...){
  as.data.frame(exprs(model))
}

#' @importFrom plyr ldply
fortify.flowSet <- function(model, data, ...){
  
  df.list <- fsApply(model, fortify, simplify = FALSE)
  pd <- pData(model)
  df <- ldply(df.list)
  df <- merge(pd, df, by.x = "name", by.y = ".id")
  df
}

#' @export
autoplot.flowFrame <- function(object, ...){
  sn <- identifier(object)
  object <- as(object, "flowSet")
  sampleNames(object) <- sn
  pData(object)[["name"]] <- sn
  autoplot(object, ...)
}

#' @export
autoplot.flowSet <- function(object, ...){
  df <- fortify(object)
  
  p <- ggplot(df, ...)
  p <- p + aes(fill = 1)
  p <- p + guides(colour = F, fill = F)

  p <- p + facet_wrap(~name)
  
  p
}
