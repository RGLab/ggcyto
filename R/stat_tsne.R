#' tsne layer.
#' 
#' This is a virtual layer which only passes on the parameters to ggcyto object without doing any actual computing on itself. 
#' Once it is added, the ggcyto object is transformed to the 'ggcyto_tsne' class.
#' The save the time, the tsne calculation is only invoked at the inital plotting action. 
#' The subsequent plots will use the cached data stored in ggcyto_tsne object.
#' @examples 
#' \dontrun{
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' p <- ggcyto(gs, subset = "CD8")
#' p <- p + stat_tsne(nodes = c("CD8/38+ DR-", "CD8/CCR7- 45RA+"), marginal = FALSE, nEvents = 1e3) 
#' #plotting first time could be slow due to the inital computing of tsne
#' p + geom_tsne_poly(degree = 2, count = 40)
#' 
#' # subsequent plotting does not need to recompute tsne
#' p + geom_tsne_degree() #or directly use geom_point(aes(color = degree)) 
#' p + geom_tsne_marker("CD38") # or use geom_point(aes(color = `CD38 APC`), size = 1.5, alpha = 0.5)  
#' }
#' @export
#' @rdname stat_tsne
stat_tsne <- function(data, ...)UseMethod("stat_tsne")

#' @export
#' @rdname stat_tsne
stat_tsne.default <- function(data, ...){
  
  
    stop("Unsupported nodes type in 'stat_tsne' layer: ", class(data), call. = FALSE)
}

#' @param data the parent population node to be operated on. If not supplied, it must be specified through 'subset' argument in 'ggcyto' constructor.
#' @param nodes,... the arguments passed to see \link{getSingleCellExpression}
#' @param groupBy the phenotype data used to group the samples for subsampling cell events
#' @param nEvents the maximum number of cell events to be used for tsne 
#' @param tsne_args a list of arguments passed to \link{Rtsne}
#' @export
#' @rdname stat_tsne
stat_tsne.character <- function(data = "_parent_"
                                , nodes
                                , groupBy = "name"
                                , nEvents = NULL
                                , tsne_args = list(check_duplicates = FALSE, theta = 0.9)
                                , ...){
  structure(
    list(parent = data
         , nodes = nodes
         , groupBy = groupBy
         , nEvents = nEvents
         , tsne_args = tsne_args
         , getSingleCellExpression_args = list(...)
    )
    , class = c("stat.tsne", "ggcyto_virtual_layer")
  )
}


#' @export
#' @return data.table
#' @rdname fortify.flowSet
fortify.GatingSet_tsne <- function(model, ...){
  gs <- model
  tsne_params <- attr(gs, "tsne_params")
  
  suppressMessages(gs <- clone(gs, isNew = FALSE))
  .subsamlple.parent(gs, tsne_params[["parent"]], tsne_params[["groupBy"]], nEvents = tsne_params[["nEvents"]]) 
  
  thisCall <- quote(.getSingleCellExpression(gs, nodes = tsne_params[["nodes"]]))
  thisCall <- as.call(c(as.list(thisCall),tsne_params[["getSingleCellExpression_args"]]))
  res_collapse <- eval(thisCall)
  
  meta_cols <- colnames(pData(gs))
  meta_cols <- c(meta_cols, "degree", "poly","name")
  mat <- as.matrix(res_collapse[, !names(res_collapse) %in% meta_cols, with = FALSE])
  
  thisCall <- quote(.tsne(mat))
  thisCall <- as.call(c(as.list(thisCall), tsne_params[["tsne_args"]]))
  dat <- eval(thisCall)
  
  dat <- cbind(dat, res_collapse)
  data.table(dat)
  
}

#' @export
#' @return data.table
#' @rdname fortify.flowSet
fortify.GatingSetList_tsne <- function(model, ...){
  getS3method("fortify", "GatingSet_tsne")(model, ...)
}

#' @export
#' @rdname as.ggplot
as.ggplot.ggcyto_tsne <- function(x){
  #get any possible filters before overwrite data
  degree.filter <- x[["data"]]@tsne_params[["degree.filter"]]
  polycount.filter <- x[["data"]]@tsne_params[["polycount.filter"]]
  
  #data needs to be fortified here if geom_gate was not added
  if(is.null(x@state[["data"]])){
    x@state[["data"]] <- fortify(x[["data"]])
  }
  x[["data"]] <- x@state[["data"]]
  #apply filter if there is any
  if(!is.null(degree.filter))
  {
    x[["data"]] <- x[["data"]][degree >= degree.filter, ]     
  }
  
  if(!is.null(polycount.filter))
  {
    filtered.poly <- x[["data"]][, nrow(.SD) , by = poly][V1 > polycount.filter, poly]
    
    x[["data"]] <- x[["data"]][poly %in% filtered.poly, ]     
  }
  #clear the raw data format
  x[["fs"]] <- NULL
  x[["gs"]] <- NULL 
  #restore the mapping 
  x[["mapping"]][["x"]] <- quote(x)
  x[["mapping"]][["y"]] <- quote(y)
  x[["labels"]] <- list()#clear axis labels
  #strip the ggcyto class attributes
  asS3(x)
}
