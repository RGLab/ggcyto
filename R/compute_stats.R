#' compute the statistics of the cell population defined by gates
#' 
#' It calls the underlining stats routine and merge it with the lable position calculated by stat_position
#' as well as the pData of flowSet.
#' 
#' @param fs flowSet. can be NULL when precaculated 'value' is provided
#' @param gates a list of filters
#' @param type can be "percent", "count" or "MFI".
#' @param value the pre-calculated stats value. when supplied, the stats computing is skipped.
#' @param data_range the data range for each channels
#' @param ... other arguments passed to stat_position function
#' @return
#' a data.table that contains percent and centroid locations as well as pData
#' that used as data for geom_btext layer.
#' @export
compute_stats <- function(fs = NULL, gates, type = "percent", value = NULL, data_range = NULL, ...){
  
  if(is.null(fs)&&(is.null(value)||is.null(data_range)))
    stop("fs must be provided when 'value' or 'data_range' is not supplied!")
  
  stat_func <- eval(as.symbol(paste(".stat", type, sep = "_")))
  stats <- stat_func(fs, gates, value = value, ...)  
  
  if(is.null(data_range))
    data_range <- range(fs[[1, use.exprs = F]])

  #add default density range

  data_range[["density"]] <- c(0,1)
  centroids <- stat_position(gates, data_range = data_range, ...)
  
  stats <- merge(centroids, stats, by = ".rownames") # merge stats with centroid
  merge(stats, .pd2dt(pData(fs)), by = ".rownames") # merge with pdata
}

#' compute the proportion/percent of the cell population over the parent 
#' 
#' @inheritParams compute_stats
#' @param digits control the percent format
.stat_percent <- function(fs, gates, digits = 3, value = NULL, ...){
  if(is.null(value)){
    # compute the stats
    fres <- filter(fs, gates)
    value <- sapply(fres, function(res)sum(res@subSet)/length(res@subSet), simplify = FALSE)
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
.stat_count <- function(fs, gates, value = NULL, ...){
  if(is.null(value)){
    fres <- filter(fs, gates)
    value <- sapply(fres, function(res)sum(res@subSet), simplify = FALSE)
  }
  sn <- names(value)
  value <- unlist(value)
  stats <- data.table(count = value, .rownames = sn) 
    
  stats
}

#' compute the MFI of the cell population
#' 
#' @inheritParams compute_stats
.stat_MFI <- function(fs, gates, digits = 3, ...){
  stop("MFI not supported yet!")
  fs_sub <- Subset(fs, gates)
     
}