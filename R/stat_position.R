#' compute the positions of the population statistics based on the geometic gate centroid
#' @param gate a flowCore filter
#' @export
stat_position <- function(gate, ...)UseMethod("stat_position")


.range_intersect <- function (r1, r2) 
{
  c( max(c(min(r1), min(r2))) 
   , min(c(max(r1), max(r2)))
    )
}

#' replace infinite values 
.fixInf <- function(x)
{
  #  browser()
  for(i in seq_along(x)){
    y <- x[i]
    if(is.infinite(y) && y<0)
      x[i] <- -.Machine$integer.max
    else if(is.infinite(y) && y>0)
      x[i] <- .Machine$integer.max
  }
  return(x)
}

#' @param adjust adjust the position of the centroid
#' @param abs logical
#' @param data_range the actual data range
#' @return the gate centroid coordinates
#' @export
#' @rdname stat_position
stat_position.filter <- function(gate, adjust = 0.5, abs = FALSE, data_range = NULL){
  
  params <- parameters(gate)
  if(abs)#plot label whithin the boundary by default 
  {
    gate_range <- data_range
  }else #specify location by absolute position of the current window
  {
    df <- fortify(gate)
    gate_range <- apply(df, 2, range)
#     browser()
    #fix the gate range with data range
    if(!is.null(data_range)){
      gate_range <- sapply(colnames(data_range), function(dim){
        
        dr <- data_range[[dim]]
        
        if(!dim %in% params){
          #the current data dimension is not present gate range
          #i.e. 1-d gate
          gr <- dr
        }else{
          gr <- gate_range[,dim]
          #fix inf value
          gr <- .fixInf(gr)
          gr <- .range_intersect(gr, dr)  
        }
        gr
      })    
    }
    
  }
#       browser() 
  #calculate centroid
  centroids <- colMeans(gate_range)
  
  # adjust the position
  #   adjust <- rep(adjust, length=2)[1:2]
  diffs <- apply(gate_range,2, diff)
  centroids <- centroids + diffs * (adjust - 0.5)
  
  
  as.data.table(t(centroids))
  
}

# stat_position.rectangleGate <- function(gate){
# 
#   param <- parameters(gate)
#   nDim <- length(param)
#   if (nDim ==  2){
#     stat_position.polygonGate(gate)
#   }else if(nDim ==  1){
#       browser()
#     
#  }
#   
#   
# }

#' @export
#' @rdname stat_position
stat_position.filterList<- function(gates, ...){
  
  .ldply(gates, stat_position, ..., .id = ".rownames")
  
}

#' @export
#' @rdname stat_position
stat_position.list<- function(gates, ...){
  stat_position(filterList(gates), ...)
}
