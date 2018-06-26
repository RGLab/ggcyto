#' compute the statistics of the cell population defined by gates
#' 
#' It calls the underlining stats routine and merge it with the label position calculated by stat_position
#' as well as the pData of flowSet.
#' 
#' This function is usually not called directly by user but used by ggcyto when geom_stat layer is added.
#' 
#' @param fs flowSet. can be NULL when precaculated 'value' is provided
#' @param gates a list of filters
#' @param type a vector of strings to specify the stats types. can be any or multiple values of "percent", "count", "gate_name", or "MFI" (MFI is currently not supported yet). 
#' @param value the pre-calculated stats value. when supplied, the stats computing is skipped.
#' @param ... other arguments passed to stat_position function
#' @return
#' a data.table that contains percent and centroid locations as well as pData
#' that used as data for geom_btext layer.
#' @export
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:4]
#' rect.g <- rectangleGate(list("FSC-H" =  c(300,500), "SSC-H" = c(50,200)), filterId = "P1")
#' rect.gates <- sapply(sampleNames(fs), function(sn)rect.g)
#' compute_stats(fs, rect.gates)
#' compute_stats(fs, rect.gates, type = c("gate_name", "percent"))
compute_stats <- function(fs = NULL, gates, type = "percent", value = NULL, ...){
  
  if(is.null(fs)&&(is.null(value)))
    stop("fs must be provided when 'value' is not supplied!")
  if(is.list(value))
  {
    if(length(value) != length(type))
    {
      if(length(type)==1)
        value <- list(value)
      else
        stop("length of value is not consistent with the length of stats type vector!")
    }
  }
  if(!is.list(value))
    value <- list(value)
  stats.list <- mapply(type, value, FUN = function(stat_type, val){
    stat_func <- eval(as.symbol(paste(".stat", stat_type, sep = "_")))
    stats <- stat_func(fs, gates, value = val, ...)  
    stats
  }, SIMPLIFY = FALSE)
  #cat the multiple stats
  stats <- Reduce(function(x,y){
    val <- paste(x[, value], y[, value], sep = "\n")
    x[, value := val]
    }, x = stats.list)
  
  centroids <- stat_position(gates, ...)
  
  stats <- merge(centroids, stats, by = ".rownames") # merge stats with centroid
  merge(stats, .pd2dt(pData(fs)), by = ".rownames") # merge with pdata
}

.stat_gate_name <- function(fs, gates, value = NULL, ...){
  if(is.null(value))
    value <- sapply(gates, function(gate)gate@filterId)
  
  value <- unlist(value)
  
  data.table(value = value, .rownames = names(value))
}
#' compute the proportion/percent of the cell population over the parent 
#' 
#' @inheritParams compute_stats
#' @param digits control the percent format
.stat_percent <- function(fs, gates, digits = 3, value = NULL, negated = FALSE, ...){
  if(is.null(value)){
    # compute the stats
    fres <- filter(fs, gates)
    value <- sapply(fres, function(res){
      p <- sum(res@subSet)/length(res@subSet)
      if(negated)
        p = 1 - p
      p
      }, simplify = FALSE)
  }
  sn <- names(value)
  value <- unlist(value)
  #format the calculated stats values
  value <- paste(format(value *100,digits=digits),"%",sep="")
  stats <- data.table(value = value, .rownames = sn) 
    
  stats
}

#' compute the event count of the cell population
#' 
#' @inheritParams compute_stats
.stat_count <- function(fs, gates, value = NULL, negated = FALSE, ...){
  if(is.null(value)){
    fres <- filter(fs, gates)
    value <- sapply(fres, function(res){
      ind <- res@subSet
      if(negated)
        ind <- !ind
      sum(ind)
      }, simplify = FALSE)
  }
  sn <- names(value)
  value <- unlist(value)
  stats <- data.table(value = value, .rownames = sn) 
    
  stats
}

#' compute the MFI of the cell population
#' 
#' @inheritParams compute_stats
.stat_MFI <- function(fs, gates, digits = 3, negated = FALSE, ...){
  stop("MFI not supported yet!")
  fs_sub <- Subset(fs, gates)
     
}