#' compute the statistics of the cell population defined by gates
#' 
#' It calls the underlining stats routine and merge it with the label position calculated by stat_position
#' as well as the pData of flowSet.
#' 
#' This function is usually not called directly by user but used by ggcyto when geom_stat layer is added.
#' 
#' @param fs flowSet. can be NULL when precaculated 'value' is provided
#' @param gates a list of filters
#' @param type can be "percent", "count" or "MFI" (MFI is currently not supported yet).
#' @param value the pre-calculated stats value. when supplied, the stats computing is skipped.
#' @param data_range a data.frame that specifies the data range for each channels (see examples for its format.)
#'                  Default is the instrument range extracted from fs object.
#' @param ... other arguments passed to stat_position function
#' @return
#' a data.table that contains percent and centroid locations as well as pData
#' that used as data for geom_btext layer.
#' @export
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:4]
#' rect.g <- rectangleGate(list("FSC-H" =  c(300,500), "SSC-H" = c(50,200)))
#' rect.gates <- sapply(sampleNames(fs), function(sn)rect.g)
#' compute_stats(fs, rect.gates)
#' #overwrite the default data_range (that is instrument range by default, which could be inaccurate sometime)
#' compute_stats(fs, rect.gates, data_range = range(fs[[1]], type = "data"))
compute_stats <- function(fs = NULL, gates, type = "percent", value = NULL, data_range = NULL, ...){
  
  if(is.null(fs)&&(is.null(value)||is.null(data_range)))
    stop("fs must be provided when 'value' or 'data_range' is not supplied!")
  
  stat_func <- eval(as.symbol(paste(".stat", type, sep = "_")))
  stats <- stat_func(fs, gates, value = value, ...)  
  
  if(is.null(data_range))
    data_range <- range(fs[[1, use.exprs = FALSE]])

  #add default density range
  #In order to ensure the stats visiblity
  #try to put it closer to zero because we don't know the actual density range
  data_range[["density"]] <- c(0,1e-4)
  centroids <- stat_position(gates, data_range = data_range, ...)
  
  stats <- merge(centroids, stats, by = ".rownames") # merge stats with centroid
  merge(stats, .pd2dt(pData(fs)), by = ".rownames") # merge with pdata
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
  stats <- data.table(percent = value, .rownames = sn) 
    
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
  stats <- data.table(count = value, .rownames = sn) 
    
  stats
}

#' compute the MFI of the cell population
#' 
#' @inheritParams compute_stats
.stat_MFI <- function(fs, gates, digits = 3, negated = FALSE, ...){
  stop("MFI not supported yet!")
  fs_sub <- Subset(fs, gates)
     
}