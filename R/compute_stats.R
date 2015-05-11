#' compute the statistics of the gates and its position in the respective projections
#' 
#' It is used to produce the data for geom_btext layer.
#' @param fs flowSet
#' @param gates a list of filters
#' @param digits control the percent format
#' @param ... other arguments passed to stat_position function
#' @return
#' a data.frame that contains percent and centroid locations as well as pData
#' @export
compute_stats.percent <- function(fs, gates, digits = 3, ...){
  
  fres <- filter(fs, gates)
  stats <- ldply(fres, function(res){
    thisStat = sum(res@subSet)/length(res@subSet)
    thisStat <- paste(format(thisStat*100,digits=digits),"%",sep="")  
    names(thisStat) <- "percent"
    thisStat
  }, .id = ".rownames")   
#   browser()
  
  

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

