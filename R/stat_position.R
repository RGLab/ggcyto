#' compute the positions of the population statistics based on the geometic gate centroid
#' 
#' It is usually not called directly by user but mainly used by compute_stats function (which is called by ggcyto add method when geom_states layer is added).
#' 
#' @param gate a flowCore filter
#' @return a data.table
#' @export
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:4]
#' rect.g <- rectangleGate(list("FSC-H" =  c(300,500), "SSC-H" = c(50,200)))
#' rect.gates <- sapply(sampleNames(fs), function(sn)rect.g)
#' stat_position(rect.gates)
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

  for(i in seq_along(x)){
    y <- x[i]
    if(is.infinite(y) && y<0)
      x[i] <- -.Machine$integer.max
    else if(is.infinite(y) && y>0)
      x[i] <- .Machine$integer.max
  }
  return(x)
}


#' @param ... other arguments
#'        adjust adjust the position of the centroid
#'        
#'        abs logical
#'        
#'        data_range the actual data range
#'        
#' @return the gate centroid coordinates
#' @export
#' @rdname stat_position
 stat_position.filter <- function(gate, ...){
   .stat_position_filter(gate, ...)
 }

#' @param data_range a two-row data.frame. Each column is a a range for a specific channel. First row is min, Second row is max.
.stat_position_filter <- function(gate, negated = FALSE, adjust = 0.5, abs = FALSE, data_range = NULL, ...){
  
  params <- parameters(gate)
  if(abs)#plot label whithin the boundary by default 
  {
    gate_range <- data_range
  }else #specify location by absolute position of the current window
  {
    df <- fortify(gate)
    gate_range <- apply(df, 2, range)

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
          if(negated)
          {
            margins <- abs(dr - gr)
            if(which.max(margins) == 1)#use lower margin
            {
              gr <- c(dr[1], gr[1])
            }else
              gr <- c(gr[2], dr[2]) # use higher margin
          }
        }
        gr
      })    
    }
    
  }

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
#     
#  }
#   
#   
# }

#' @export
#' @rdname stat_position
stat_position.filterList<- function(gate, ...){
  
  .ldply(gate, stat_position, ..., .id = ".rownames")
  
}

#' @export
#' @rdname stat_position
stat_position.list<- function(gate, ...){
  stat_position(filterList(gate), ...)
}
