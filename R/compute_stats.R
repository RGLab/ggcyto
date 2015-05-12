#' compute the statistics of the cell population defined by gates
#' 
#' It calls the underlining stats routine and merge it with the lable position calculated by stat_position
#' as well as the pData of flowSet.
#' 
#' @param fs flowSet
#' @param gates a list of filters
#' @param type can be "percent", "count" or "MFI".
#' @param ... other arguments passed to stat_position function
#' @return
#' a data.frame that contains percent and centroid locations as well as pData
#' that used as data for geom_btext layer.
#' @export
compute_stats <- function(fs, gates, type = "percent", ...){
  
  type <- match.arg(type, c("percent", "count", "MFI"))
#   browser()
  stat_func <- eval(as.symbol(paste(".stat", type, sep = "_")))
  stats <- stat_func(fs, gates, ...)

  data_range <- range(fs[[1, use.exprs = F]])
#   params <- parameters(gates[[1]])
#   data_range <- data_range[, params, drop = FALSE]
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
#' @param digits control the percent format
.stat_percent <- function(fs, gates, digits = 3){
  fres <- filter(fs, gates)
  stats <- ldply(fres, function(res){
    thisStat = sum(res@subSet)/length(res@subSet)
    thisStat <- paste(format(thisStat*100,digits=digits),"%",sep="")  
    names(thisStat) <- "percent"
    thisStat
  }, .id = ".rownames")   
}

#' compute the event count of the cell population
#' 
#' @inheritParams compute_stats
.stat_count <- function(fs, gates){
  fres <- filter(fs, gates)
  stats <- ldply(fres, function(res){
    thisStat = sum(res@subSet)
    names(thisStat) <- "count"
    thisStat
  }, .id = ".rownames")   
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