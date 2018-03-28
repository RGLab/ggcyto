#' Plot fluorescence intensity in one or two dimension.
#'
#' Overloaded autoplot for the cytomertry data structure: flowFrame or flowSet, Gatinghierarchy, GatingSet.
#' It plots the cytometry data with geom_histogram, geom_density or geom_hex.
#'
#' @param object flowFrame, flowSet, GatingSet object
#' @param x define the x dimension of the plot. When object is a flowFrame, it can be missing, which plots 1d density plot on all the channels. 
#' @param y define the y dimension of the plot. Default is NULL, which means 1d densityplot.
#' @param bins passed to geom_hex
#' @param axis_inverse_trans logical flag indicating whether to add \link{axis_x_inverse_trans} and axis_x_inverse_trans layers.
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
#' #1d- density plot on all channels
#' autoplot(fs[[1]])
#' 
#' #2d plot: default geom_hex plot
#' autoplot(fs, x = 'FSC-H', y ='SSC-H')
#'
#' #autplot for GatingSet
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' autoplot(gs, "CD3+")
#' #display axis values in transformed scale
#' autoplot(gs, "CD3+", axis_inverse_trans = FALSE)
#'
#' #autplot for GatingHierarchy
#' gh <- gs[[1]]
#' autoplot(gh) # by default the strip.text shows the parent population
#'
#' #To display the gate name
#' #autoplot(gh , strip.text = "gate")
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
autoplot.ncdfFlowList <- function(object, ...){
  getS3method("autoplot", "flowSet")(object, ...)
}

#' @export
#' @rdname autoplot
autoplot.flowFrame <- function(object, x, ...){
  
  if(missing(x)){
    density_fr_all(object) 
  }else{
    object <- fortify_fs(object)
    autoplot(object, x = x, ...)
  }
    
}

density_fr_all <- function(fr, strip.text = c("both", "channel", "marker"), ...){
  
  #plot each individual channel
  Objs <- sapply(colnames(fr), function(chnl){
      p <- autoplot(fr, chnl, ...)
      p <- p + guides(fill=FALSE) + labs(title = NULL)
      myTheme <- theme(axis.title = element_text(color = gray(0.3), size = 8)
                       , axis.text = element_text(color = gray(0.3), size = 6)
                       , axis.title.y = element_blank()
                       , strip.text = element_blank()
                       , plot.margin = unit(c(0,0,0,0), "cm")
                       , panel.spacing = unit(0, "cm")
      )
      p <- p + myTheme
      attr(p$data, "strip.text") <- chnl
      p
    }, simplify = FALSE)
  
  
  #convert it to a special class to dispatch the dedicated print method
  Objs <- as(Objs, "ggcyto_GatingLayout")
  Objs@arrange.main <- identifier(fr)


  Objs
  
  
}

#' @export
#' @rdname autoplot
autoplot.GatingSetList <- function(object, ...){
  getS3method("autoplot", "GatingSet")(object, ...)
}

#' @param gate the gate to be plotted
#' @export
#' @rdname autoplot
autoplot.GatingSet <- function(object, gate, x = NULL,  y = "SSC-A", bins = 30, axis_inverse_trans = TRUE, ...){
  if(missing(gate))
    stop("Must specifiy 'gate'!")
  if(is.null(x)){
    #determine dimensions from gate
    g <- getGate(object[[1]], gate[1])
    params <- parameters(g)
    nDims <- length(params)
    if(nDims == 1){
      x <- params
      y <- flowWorkspace:::fix_y_axis(gs = object, x = x, y = y)
    }else{
      x <- params[1]
      y <- params[2]
    }
  }

  mapping <- aes_q(x = as.symbol(x), y = as.symbol(y))

  p <- ggcyto(object, mapping, ...) + geom_hex(bins = bins) + geom_gate(gate) + geom_stats()
  p <- p + ggcyto_par_set(limits = "instrument")
  if(axis_inverse_trans)
    p <- p + axis_x_inverse_trans() + axis_y_inverse_trans()
  p

}

#' @param bool whether to plot boolean gates
#' @param arrange.main the main title of the arranged plots
#' @param arrange whether to use arrangeGrob to put multiple plots in the same page
#' @param merge wehther to merge multiple gates into the same panel when they share the same parent and projections
#' @param projections a list of customized projections
#' @param strip.text either "parent" (the parent population name) or "gate "(the gate name). The latter usually is used when merge is FALSE
#' @param path the gating path format (passed to \link{getNodes})
#' @importFrom gridExtra arrangeGrob
#' @export
#' @rdname autoplot
autoplot.GatingHierarchy <- function(object, gate, y = "SSC-A", bool=FALSE
                         , arrange.main = sampleNames(object), arrange=TRUE, merge=TRUE
                         , projections = list()
                         , strip.text = c("parent", "gate")
                         , path = "auto"
                         , ...){
  strip.text <- match.arg(strip.text)
  if(missing(gate)){
    gate <- getNodes(object, path = path)
    gate <- setdiff(gate,"root")
  }else if (is.numeric(gate)){
    gate <- getNodes(object, path = path)[gate]
  }

  #match given axis to channel names
  fr <- getData(object, use.exprs = FALSE)
  projections <- lapply(projections, function(thisPrj){
    sapply(thisPrj, function(thisAxis)getChannelMarker(fr, thisAxis)[["name"]])
  })


  plotList <- flowWorkspace:::.mergeGates(object, gate, bool, merge, projections = projections)
  Objs <- lapply(plotList,function(plotObjs){

    if(is.list(plotObjs)){
      gate <- plotObjs[["popIds"]]
      parent <- plotObjs[["parentId"]]
      myPrj <- projections[[as.character(gate[1])]]

    }else{
      gate <- plotObjs
      parent <- getParent(object, gate, path = path)
      myPrj <- projections[[as.character(gate)]]
    }


    if(is.null(myPrj)){
      p <- autoplot.GatingSet(object, gate, y = y, ...)
    }else{
      p <- autoplot.GatingSet(object, gate, x = myPrj[["x"]], y = myPrj[["y"]], ...)
    }

    p <- p + guides(fill=FALSE) + labs(title = NULL)
    myTheme <- theme(axis.title = element_text(color = gray(0.3), size = 8)
                     , axis.text = element_text(color = gray(0.3), size = 6)
                     , strip.text = element_text(size = 10)
                     , plot.margin = unit(c(0,0,0,0), "cm")
                     , panel.spacing = unit(0, "cm")
    )
    p <- p + myTheme

    #rename sample name with parent or current pop name in order to display it in strip

    if(strip.text == "parent"){
      popName <- parent
    }else{
      popName <- paste(gate, collapse = "|")
    }
    attr(p$data, "strip.text") <- popName

    p

  })

  if(arrange){
    #convert it to a special class to dispatch the dedicated print method
    Objs <- as(Objs, "ggcyto_GatingLayout")
    Objs@arrange.main <- arrange.main
  }

  Objs

}
