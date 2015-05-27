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
#' a data.frame that contains percent and centroid locations as well as pData
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
# browser()
  data_range[["density"]] <- c(0,1)
  centroids <- stat_position(gates, data_range = data_range, ...)
  stats <- merge(centroids, stats) # merge stats with centroid
  merge(stats, name_rows(pData(fs))) # merge with pdata
}

#' compute the proportion/percent of the cell population over the parent 
#' 
#' @inheritParams compute_stats
#' @importFrom plyr rename
#' @param digits control the percent format
.stat_percent <- function(fs, gates, digits = 3, value = NULL){
  if(is.null(value)){
    # compute the stats
    fres <- filter(fs, gates)
    stats <- ldply(fres, function(res)sum(res@subSet)/length(res@subSet), .id = ".rownames")   
  }else{
    #simply fomart the pre-calculated stats values
    stats <- ldply(value, function(x)x, .id = ".rownames") 
  }
  stats <- rename(stats, replace = c("V1" = "percent"))
  stats[["percent"]] <- paste(format(stats[["percent"]]*100,digits=digits),"%",sep="")  
  stats
}

#' compute the event count of the cell population
#' 
#' @inheritParams compute_stats
.stat_count <- function(fs, gates, value = NULL){
  if(is.null(value)){
    fres <- filter(fs, gates)
    stats <- ldply(fres, function(res)sum(res@subSet), .id = ".rownames")
  }else{
    #simply fomart the pre-calculated stats values
    stats <- ldply(value, function(x)x, .id = ".rownames") 
  }
  
  stats <- rename(stats, replace = c("V1" = "count"))
  
  stats
}

#' compute the MFI of the cell population
#' 
#' @inheritParams compute_stats
.stat_MFI <- function(fs, gates, digits = 3){
  stop("MFI not supported yet!")
  fs_sub <- Subset(fs, gates)
  stats <- ldply(sampleNames(fs_sub), function(sn){
    
    
  }, .id = ".rownames")   
}