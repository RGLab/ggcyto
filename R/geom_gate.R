#' Add a gate layer to a ggcyto plot.
#' 
#' When 'data' is a gate (or flowCore filter) or a list of gates or a filterList object. 
#' When it is used directly with 'ggplot', pdata of the flow data must be supplied through 'pd' argument explicitly in order for 
#' the gates to be dispatched to each panel. 
#' However It is not necessary when used with 'ggcyto' wrapper since the latter will attach pData automatically.
#' 
#' When 'data' is a character, it construct an abstract geom layer for a character that represents nodes in a Gating tree
#' and will be instanatiated later as a specific geom_gate layer or layers based on the gates extracted from the given GatingSet object.
#' 
#' @name geom_gate
#' @aliases geom_gate.default geom_gate.list geom_gate.filter geom_gate.filterList
#' geom_gate.quadGate geom_gate.character geom_gate.filters geom_gate.filtersList
#' geom_gate.logical geom_gate.logicalFilterResult
#' @usage geom_gate(data, mapping = NULL, fill = NA, colour = "red", nPoints = 100, ...)
#' @param data a filter (Currently only rectangleGate (1d or 2d), polygonGate, ellipsoidGate are supported.)
#'              or a list of these gates 
#'              or filterList
#'              or character specifying a gated cell population in the GatingSet
#' @param mapping The aesthetic mapping
#' @param fill fill color for the gate. Not filled by default.
#' @param colour default is red
#' @param nPoints used for interpolating polygonGates to prevent them from losing shape when truncated by axis limits
#' @param ... other arguments
#' @param pd pData (data.frame) that has rownames represents the sample names used as key to be merged with filterList
#' @return a geom_gate layer
#' @examples 
#' data(GvHD)
#' fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
#' p <- p + geom_hex(bins = 128)
#' rect.g <- rectangleGate(list("FSC-H" =  c(300,500), "SSC-H" = c(50,200)))
#' #constuctor for a list of filters
#' rect.gates <- sapply(sampleNames(fs), function(sn)rect.g)
#' p + geom_gate(rect.gates)
#' 
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") + geom_hex(bins = 64)
#' # add gate layer by gate name
#' p + geom_gate("CD4")
#' @export
geom_gate <- function(data, ...)UseMethod("geom_gate")

geom_gate_impl <- function(data, ...)UseMethod("geom_gate_impl")

#' @export
geom_gate.default <- function(data, ...){
  
  if(missing(data)){
    data <- "_child_"
      geom_gate(data, ...)
  }else
    stop("ggcyto doesn't know how to deal with gate of class ", class(data), call. = FALSE)
}

#' @export
geom_gate.list <- function(data, ...){
  element <- data[[1]]
  if(is(element, "logical")||is(element, "logicalFilterResult")){
    geom_gate.logical(data, ...)
  }else if(is(element, "filter")){
    data <- filterList(data)
    geom_gate(data, ...)    
  }else if(is(element, "filters")){
    data <- filtersList(data)
    geom_gate(data, ...)    
  }else
    stop("unsupported geom_gate type:", class(element))
  
}

#' @export
geom_gate.filterList <- function(data, ...){
  .geom_gate_filterList(data, ...)
}
.geom_gate_filterList <- function(data, pd, nPoints = 100, ...){  
  #construct gate-type specific layer
  geom_gate_layer <- geom_gate_impl(data[[1]], nPoints = nPoints, ...)

  
  
  # assuming it is already attached to attribute
  # when pd is missing
  if(!missing(pd)){
    attr(data, "pd") <- pd #this step is done automatically when `+.ggcyto_flowSet` is invoked
  }
  
  
  #can't fortify it yet since pd may not be attached by fs yet
  # data <- fortify(data)

  # update data with pdata
  geom_gate_layer[["data"]] <- data
  geom_gate_layer
}

#' @export
geom_gate.filter <- function(data, mapping = NULL, fill = NA, colour = "red", nPoints = 100, ...){
  structure(
    list(filter = data
         , gate_params = list(mapping = mapping
                              , fill = fill
                              , colour = colour
                              , nPoints = nPoints
                            ,...)
    )
    , class = c("filter.layer", "ggcyto_virtual_layer")
  )
}


geom_gate_impl.polygonGate <- function(data, mapping = NULL, fill = NA, colour = "red", nPoints = 100, ...){
  
  #To proper interpolate the polygon we need to pass nPoints
  # so we need to avoid the fority process triggered by geom_path$new here (by not passing the data)
  path_layer <- geom_path(mapping = mapping, data = NULL , colour = colour, ...) 
  # #now we can saftely assign the data
  path_layer[["data"]] <- data
  # 
  path_layer
  
}


geom_gate_impl.rectangleGate <- function(data, mapping = NULL, fill = NA, colour = "red", nPoints = 100, ...){
  
  param <- parameters(data)
  nDim <- length(param)
  if (nDim ==  2){
    geom_gate_impl(data = as(data, "polygonGate"), mapping = mapping, fill = fill, colour = colour, nPoints = nPoints, ...)
  }else if(nDim ==  1){

     layer <- geom_hvline(data = data, colour = colour, ...)
     
     # layer[["is_1d_gate"]] <- TRUE
     layer
         
  }else
    stop("rectangelGate with dimension ", nDim, "is not supported!")
  
}


geom_gate_impl.ellipsoidGate <- function(data, ...){
  
  geom_gate_impl.polygonGate(data, ...)
}

setAs(from = "quadGate", to = "filters", function(from){
  cutpoint <- from@boundary
  dimnames <- names(cutpoint)
  coord <- list(c(-Inf, cutpoint[[1]]) ,c(cutpoint[[2]], Inf))
  names(coord) <- dimnames
  ul <- rectangleGate(coord)
  
  coord <- list(c(cutpoint[[1]], Inf) ,c(cutpoint[[2]], Inf))
  names(coord) <- dimnames
  ur <- rectangleGate(coord)
  
  coord <- list(c(cutpoint[[1]], Inf) ,c(-Inf, cutpoint[[2]]))
  names(coord) <- dimnames
  br <- rectangleGate(coord)
  
  coord <- list(c(-Inf, cutpoint[[1]]) ,c(-Inf, cutpoint[[2]]))
  names(coord) <- dimnames
  bl <- rectangleGate(coord)
  
  filters(list(ul, ur, br, bl))
})

#' @export
geom_gate.quadGate <- function(data, ...){
 
  
  data <- as(data, "filters")
  geom_gate(data, ...)
}

#' @export
geom_gate.character <- function(data, ...){
        structure(
                 list(node = data
                      , gate_params = list(...)
                  )
                , class = c("gs.node", "ggcyto_virtual_layer")
                )
}

#' @export
geom_gate.filters <- function(data, ...){
  structure(
    list(gate = data
         , gate_params = list(...)
    )
    , class = c("geom.filters", "ggcyto_virtual_layer")
  )
}

#' @export
geom_gate.filtersList <- function(data, ...){
  structure(
    list(gate = data
         , gate_params = list(...)
    )
    , class = c("geom.filtersList", "ggcyto_virtual_layer")
  )
}

#' @export
geom_gate.logicalFilterResult <- function(data, ...){
  geom_gate.logical(data, ...)
}

#' @export
geom_gate.logical <- function(data, ...){
  structure(
    list(indices = data
         , gate_params = list(...)
    )
    , class = c("logicalGates", "ggcyto_virtual_layer")
  )
}
