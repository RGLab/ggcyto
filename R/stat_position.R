#' Compute the positions of the population statistics based on the geometic gate centroid for a ggcyto plot.
#' 
#' It is usually not called directly by user but mainly used by compute_stats function (which is called by ggcyto add method when geom_states layer is added).
#' 
#' @name stat_position
#' @aliases stat_position.filter stat_position.filterList stat_position.list
#' @param gate a flowCore filter
#' @param negated logical indicating whether position needs to be moved to negative side of gate
#' @param limits used to fix the gate range
#' @param ... other arguments
#' @param adjust see details
#' @param location see details
#' @param data_range a two-row data.frame representing the actual data range. Each column is a a range for a specific channel. First row is min, Second row is max.
#' @details
#' ## Specifying location for statistical annotation
#' 
#' The \code{adjust} and \code{location} arguments allow for a few different ways to adjust the location of the statistical
#' annotation for a gate on a \code{ggcyto} plot. The valid values for \code{location} are "gate" (default), "data", "plot", and "fixed".
#' 
#' ### Relative location
#' 
#' If \code{location} is not "fixed", the starting position of the annotation will be determined with respect to a rectangular window whose
#' bounds are determined in the following way:
#' * For \code{location = "gate"}, the window will be set by the range of the data in the gate
#' * For \code{location = "data"}, the window will be set by the range of values in all of the data on the plot (provided by \code{data_range})
#' * For \code{location = "plot"}, the window will be set by the axis limits of the plot (adjusted by \code{\link{ggcyto_par_set}})
#' 
#' This starting position can then be adjusted by passing values in a vector to the \code{adjust} parameter, where they will be
#' interpreted as relative proportions of the window dimension, where 0.0 represents the lower bound of the dimension and 1.0 represents
#' the upper bound. So, for a 2-D plot, \code{adjust=c(0,0)} places the annotation at the lower left corner of this window and \code{adjust=c(1,1)} places
#' it at the upper right corner.
#' 
#' As another example, for a 2-D gate, if \code{location = "gate"} and \code{adjust=c(0.25, 0.75)}, the statistical annotation will be
#' placed 1/4 of the way across the x-range of the gate and 3/4 of the way across the y-range of the gate.
#' 
#' ### Fixed location
#' 
#' If \code{location = "fixed"}, the numeric vector passed to \code{adjust} will be interpreted as values on the data scales of the plot to provide
#' an explicit location for the annotation.
#' 
#' For example, if the annotation should be at the location 3000, 5000 on the plot, that could be done with \code{location="fixed"} and
#' \code{adjust = c(3000,5000)}.
#' 
#' ### Default
#' 
#' The default behavior if no values are provided to \code{location} or \code{adjust} will be to place the annotation at
#' the center of the range of the data in the gate.
#' 
#' @return a data.table of gate centroid coordinates
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
#' @noRd 
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

#' @rdname stat_position
#' @export
 stat_position.filter  <- function(gate, negated = FALSE, adjust = 0.5, location = "gate", data_range = NULL, limits = NULL, ...){
  
  params <- parameters(gate)
  location <- match.arg(location, c("gate", "data", "plot", "fixed"))
  
  if(location == "gate"){
    df <- fortify(gate, data = data_range)
    gate_range <- apply(df, 2, range)

    #fix the gate range with limits
    if(!is.null(limits)){
      gate_range <- sapply(colnames(limits), function(dim){
        
        dr <- limits[[dim]]
        
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
    
  }else if(location == "data"){
    gate_range <- data_range
  }else if(location == "plot"){
    gate_range <- limits[,params]
  }

  if(location == "fixed"){
    centroids <- setNames(adjust, params)
  }else{
    #calculate centroid
    centroids <- colMeans(gate_range)
    
    # adjust the location
    #   adjust <- rep(adjust, length=2)[1:2]
    diffs <- apply(gate_range,2, diff)
    not_density <- names(centroids) != "density"
    centroids[not_density] <- centroids[not_density] + diffs[not_density] * (adjust - 0.5)
  }
  
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
stat_position.filterList<- function(gate, ...){
  
  .ldply(gate, stat_position, ..., .id = ".rownames")
  
}

#' @export
stat_position.list<- function(gate, ...){
  stat_position(filterList(gate), ...)
}
