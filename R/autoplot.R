#' Plot fluorescence intensity in one or two dimension.
#' 
#' Overloaded autoplot for the cytomertry data structure: flowFrame or flowSet, Gatinghierarchy, GatingSet.
#' It plots the cytometry data with geom_histogram, geom_density or geom_hex.
#' 
#' @param object flowFrame, flowSet, GatingSet object
#' @param x,y define the dimension of the plot
#' @param bins passed to geom_hex
#' @param ... other arguments passed to ggplot
#' 
#' @rdname autoplot
#' @return a ggcyto object
#' 
#' @examples
#' library(flowCore)
#' data(GvHD)
#' fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
#'
#' #1d- density plot
#' autoplot(fs, x = "SSC-H")
#' 
#' #2d plot: default geom_hex plot
#' autoplot(fs, x = 'FSC-H', y ='SSC-H')
#' 
#' @export 
autoplot.flowSet <- function(object, x, y = NULL, bins = 30, ...){
  
  # check the dimensions
  if(missing(x))
    stop("'x' must be supplied to ggplot!")
  if(is.null(y)){
    p <- ggcyto(object, aes_q(x = as.symbol(x)), ...)  #aes_string doesn't play well with special character (e.g. '-')
    p <- p + geom_density(fill = "black")
  }else{
    p <- ggcyto(object, aes_q(x = as.symbol(x), y = as.symbol(y)), ...)  
    p <- p + geom_hex(bins = bins) 
    
  }
    
  # apply boundary filter to remove outliers
#   if(margin){
#     g <- boundaryFilter(x = dims, tol = 1e-5)
#     object <- Subset(object, g)
#   }

  p
}

#' @export 
#' @rdname autoplot
autoplot.flowFrame <- function(object, ...){
  object <- fortify_fs(object)
  autoplot(object, ...)
}

#' @param gate the gate to be plotted
#' @export 
#' @rdname autoplot
autoplot.GatingSet <- function(object, gate, x = NULL,  y = "SSC-A", ...){
  if(missing(gate))
    stop("Must specifiy 'gate'!")
  if(is.null(x)){
    #determine dimensions from gate
    g <- getGate(object[[1]], gate[1])
    params <- parameters(g)
    nDims <- length(params)
    if(nDims == 1){
      x <- params
    }else{
      x <- params[1]
      y <- params[2]
    }    
  }  
  
  mapping <- aes_q(x = as.symbol(x), y = as.symbol(y))
  
  p <- ggcyto(object, mapping) + geom_hex(...) + geom_gate(gate) + geom_stats() 
  p <- p + ggcyto_par_set(limits = "instrument")
  p <- p + axis_x_inverse_trans() + axis_y_inverse_trans()
  p
  
}

#' @param bool whether to plot boolean gates
#' @param arrange.main the main title of the arranged plots
#' @param arrange whether to use arrangeGrob to put multiple plots in the same page
#' @param merge wehther to merge multiple gates into the same panel when they share the same parent and projections
#' @param projections a list of customized projections
#' @param strip.text either "parent" (the parent population name) or "gate "(the gate name). The latter usually is used when merge is FALSE
#' @importFrom gridExtra arrangeGrob
#' @export 
#' @rdname autoplot
autoplot.GatingHierarchy <- function(object, gate, y = "SSC-A", bool=FALSE
                         , arrange.main = sampleNames(object), arrange=TRUE, merge=TRUE
                         , projections = list()
                         , strip.text = c("parent", "gate")
                         , ...){
  strip.text <- match.arg(strip.text)
  if(missing(gate)){
    gate <- getNodes(object, path = "auto")
    gate <- setdiff(gate,"root")
  }else if (is.numeric(gate)){
    gate <- getNodes(object, path = "auto")[gate]
  }
    
  #match given axis to channel names
  fr <- getData(object, use.exprs = FALSE)
  projections <- lapply(projections, function(thisPrj){
    sapply(thisPrj, function(thisAxis)getChannelMarker(fr, thisAxis)[["name"]])
  })
  
#   browser()
  plotList <- flowWorkspace:::.mergeGates(object, gate, bool, merge, projections = projections)
  Objs <- lapply(plotList,function(plotObjs){
    
    if(is.list(plotObjs)){
      gate <- plotObjs[["popIds"]]
      parent <- plotObjs[["parentId"]]
      myPrj <- projections[[as.character(gate[1])]]
      
    }else{
      gate <- plotObjs
      parent <- getParent(object, gate, path = "auto")
      myPrj <- projections[[as.character(gate)]]
    }
    
#     browser()
    if(is.null(myPrj)){
      p <- autoplot.GatingSet(object, gate, ...)
    }else{
      p <- autoplot.GatingSet(object, gate, x = myPrj[["x"]], y = myPrj[["y"]], ...)
    }
    
    p <- p + guides(fill=FALSE) + labs(title = NULL)
    p <- as.ggplot(p)
    myTheme <- theme(axis.title = element_text(color = gray(0.3), size = 8)
                     , axis.text = element_text(color = gray(0.3), size = 6)
                     , strip.text = element_text(size = 10)
                     , plot.margin = unit(c(0,0,0,0), "cm")
                     , panel.margin = unit(0, "cm")
                    )
    p <- p + myTheme
    
    #rename sample name with parent or current pop name in order to display it in strip
    
    if(strip.text == "parent"){
      popName <- parent
    }else{
      popName <- paste(gate, collapse = "|")
    }
    p$data[, name:= popName]
    
    for(i in 1:length(p$layers)){
      if(!ggplot2:::is.waive(p$layers[[i]][["data"]])){
        
        p$layers[[i]][["data"]][, name:= popName]
      }
        
    }
      
    
    p

  })
  			
  if(arrange){
    #convert it to a special class to dispatch the dedicated print method
    Objs <- as(Objs, "ggcyto_gate_layout")
    Objs@arrange.main <- arrange.main
  }
  
  Objs
  
}

#' print method for ggcyto_gate_layout class
#' It calls arrangeGrob to arrange a list of ggplot objects stored as ggcyto_gate_layout object
#' @param x ggcyto_gate_layout, which is essentially a list of ggplot objects
#' @param ... not used
#' @export
print.ggcyto_gate_layout <- function(x, ...){
  arrange.main <- x@arrange.main
  plot(do.call(arrangeGrob, c(grobs = x, top = arrange.main)))  
}

#' overloaded '+' method for ggcyto_gate_layout
#' 
#' It adds the layer specified by 'e2' to each individual ggplot object stored in ggcyto_gate_layout
#' @param e1 ggcyto_gate_layout
#' @param e2 any ggplot layer
#' @return a modified ggcyto_gate_layout object
#' @export
`+.ggcyto_gate_layout` <- function(e1, e2){
  # browser()
  for(i in seq_along(e1)){
    e1[[i]] <- e1[[i]] + e2
  }
  e1
}

setMethod("+", "ggcyto_gate_layout", `+.ggcyto_gate_layout`)

#' @rdname print.ggcyto
#' @method show ggcyto_gate_layout
#' @export
show.ggcyto_gate_layout <- function(object){print(object)}

#' @rdname print.ggcyto
#' @method show ggcyto
#' @export
setMethod("show", "ggcyto_gate_layout", show.ggcyto_gate_layout)
