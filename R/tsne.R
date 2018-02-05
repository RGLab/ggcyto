#' this function runs the dimension-reduction algorithm tSNE (t-Distributed Stochastic Neighbor Embedding, Van der Maaten's Barnes-Hut implementation, R pkg 'Rtsne') on a gatingSet
#' Will sample the minimal number of cells available in all samples to generate balanced cell counts
#' 
#' IMPORTANT: Requires a valid gatingSet with cytokine gates downstream of a parent gate
#' Also expects that pData(gs) contains at least columns: 'name', 'ptid' so we can identify cells later
#' 
#' @param gs a GatingSet object, properly gated data with annotation in its pData
#' @param parent a \code{string} describing the gate upstream of the cytokine gates (eg. "CD4", "cd8+", etc...)
#' @param cytokine a \code{vector} of \code{strings} describing the cytokine gates immediately downstream of parent, eg: "IL2", "IFNg"
#' @param otherMarkers the remaining markers of the data
#' @param markerMap named list of marker names to gate names, eg.  list("CD4/IL2" = "IL2","CD4/IFNg" = "IFNg")
#' @param swap boolean for whether marker and gate names (from markerMap above) should be swapped. Passed onto getSingleCellExpression()
#' @param groupBy columns of the \code{gatingSet}'s phenoData, same number of cells will be sampled from each group
#' @param degreeFilter keep cells of this degree and higher, useful when tSNE takes too long to run
#' @param seed since tSNE is random, need a random seed so we can reproduce results
#' @param theta parameter to be passed to the \code{Rtsne} function
#' @param ... other parameters to be passed to the \code{Rtsne} function
#' @return a \code{matrix} of X and Y coordinates
#' @import flowWorkspace
#' @import data.table
#' @import plyr
#' @import Rtsne
#' @importFrom flowWorkspace updateIndices
.subsamlple.parent <- function (gs, parent, groupBy, nEvents = NULL) {
  
 
  pd <- as.data.table(pData(gs))
  message("getting total cell counts from parent gate ", parent)
  parent_count <- unlist(lapply(gs, function(gh) getTotal(gh, parent)))
  parent_count = ldply(parent_count)
  setnames(parent_count, c("name", parent))
  pd <- merge(pd, parent_count, by = "name")
  actual.events <- min(pd[, sum(get(parent)), by = groupBy][, V1])
  if(!is.null(nEvents))
    nEvents <- min(nEvents, actual.events)
  
  message("after grouping by '", groupBy, "', all groups will at least ", nEvents, "cells.")
  pd[, {
    totalEvents <- sum(get(parent))
    gInd <- 1:totalEvents
    gInd <- sample.int(totalEvents, size = nEvents)
    gInd.logical <- rep(F, totalEvents)
    gInd.logical[gInd] <- T
    sn.factor <- unlist(sapply(name, function(sn) rep(sn, 
                                                      .SD[name == sn, get(parent)])))
    ind.vec <- split(gInd.logical, sn.factor)
    for (sn in name) {
      thisInd <- ind.vec[[sn]]
      gh <- gs[[sn]]
      updateIndices(gh, parent, thisInd)
    }
  }, by = groupBy] 
  message("subsampling complete ! recomputing... ")
  nodes <- getChildren(gs[[1]], parent, path = 2)
  for (node in nodes) 
    suppressMessages(recompute(gs, node))
}

.getSingleCellExpression <- function (gs, nodes, degreeFilter = 0, ...) {
  message("generating event masks ")
  
  res <- getSingleCellExpression(gs, nodes, threshold = FALSE, ...)
  
  res_mask <- getSingleCellExpression(gs, nodes, ...)
  
  res_collapse <- rbindlist(
                      lapply(names(res), function(sn) {
                      mat <- as.data.table(res[[sn]])
                      if (nrow(mat) > 0) {
                        mat_mask <- res_mask[[sn]]
                        mat_mask[mat_mask > 0] <- 1
                        mat[, degree := rowSums(mat_mask)]
                        mat[, poly := Reduce(paste0, as.list(as.data.frame(mat_mask)))]
                        #convert poly code to the marker names
                        markers <- colnames(mat_mask)
                        polyExpr <- sapply(mat[, poly], function(poly){
                          boolExpr <- as.logical(as.integer(strsplit(poly, split = "")[[1]]))
                          paste0(markers[boolExpr], collapse = " ")
                        })
                        mat[, poly := polyExpr]
                        pd <- data.table(pData(gs[[sn]]))
                        
                        cbind(mat, pd)
                      } 
                      else NULL
                    })
                    , use.names = TRUE, fill = FALSE, idcol = FALSE)
                    
  
  
  # message(" input matrix has", nrow(res_collapse), "rows...")
  # res_collapse <- res_collapse[degree > degreeFilter,]
  # message("input matrix has", nrow(res_collapse), "rows after filtering for cells of degree >", degreeFilter)
  res_collapse
}
#' @import Rtsne Rtsne
.tsne <- function (input_mat, ...) {
    
  message("starting tSNE run at ", date())
  tsne_out <- Rtsne(input_mat, ...)
  dat <- tsne_out$Y
  colnames(dat) <- c("x", "y")
  message("completed tSNE run at", date(), "!")
  return(dat)
  
}