#' replace current cytometry data
#' 
#' It essentially reconstruct the entire ggcyto plot object based on the new data and the original mapping and layers
#' recorded in the plot object
#' 
#' @rdname replace_data
#' @param e1 the ggcyto object
#' @param e2 the new cytometry data . It can be 'GatingSet' or 'flowSet'.
#' @return the new ggcyto object
#' @export
#' @importFrom ggplot2 %+%
#' @examples 
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_bcell_auto",full = TRUE))
#' gs1 <- gs[1]
#' gs2 <- gs[2]
#' 
#' #construct the ggcyto object for gs1
#' p <- ggcyto(gs1, aes(cd24, cd38)) + geom_hex(bins = 128)
#' p <- p + geom_gate("Transitional") #add gate
#' #customize the stats layer
#' p <- p + geom_stats(type = "count", size = 6,  color = "white", fill = "black", adjust = 0.3)
#' #customize the layer
#' p <- p + labs_cyto("channel")
#' #customize the axis limits
#' p <- p + ggcyto_par_set(limits = "instrument")
#' #add another population as the overlay dots
#' p <- p + geom_overlay("IgD-CD27-", col = "black", size = 1.2, alpha = 0.4)
#' #hide the legend
#' p <- p + guides(fill=FALSE)
#' p
#' 
#' #replace the data with gs2 and see the same visual effect
#' p %+% gs2
#' 
setMethod("%+%", c("ggcyto"),function(e1,e2)replace.ggcyto.data(e1,e2))

replace.ggcyto.data <- function(e1, e2){
    history <- e1[["layer.history"]]
    thisCall <- quote(ggcyto(e2))
    mapping <- history[["mapping"]]
    
    if(!is.null(mapping)){
      thisCall[["mapping"]] <- mapping
      history[["mapping"]] <- NULL  
    }
    
    subset <- history[["subset"]]
    if(!is.null(subset)){
      thisCall[["subset"]] <- subset
      history[["subset"]] <- NULL
    }
    
    p <- eval(thisCall)
    
      
    #re-adding the original layers
    for(e in history)
    {
      attr(e, "is.recorded") <- FALSE
      p <- p + e
    }
      
    p
}

#' @rdname replace_data
#' @export
setMethod("%+%", c("ggcyto_GatingLayout"),function(e1,e2)replace.ggcyto_GatingLayout.data(e1,e2))
replace.ggcyto_GatingLayout.data <- function(e1, e2){
  stop("Sorry, %+% operator currently doesn't support 'ggcyto_GatingLayout' yet!")
}
