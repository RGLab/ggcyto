#' Create a new ggcyto plot from a flowSet
#'
#' @param data default flowSet for plot
#' @param mapping default list of aesthetic mappings (these can be colour,
#'   size, shape, line type -- see individual geom functions for more details)
#' @param filter a flowcore gate object or a function that takes flowSet and channels as input and returns a data-dependent flowcore gate
#'                The gate is used to filter the flow data before it is plotted. 
#' @param max_nrow_to_plot the maximum number of cells to be plotted. When the actual data exceeds it, The subsampling process will be triggered to speed up plotting. Default is 2e5.To turn off the subsampling, simply set it to a large enough number or Inf
#' @param ... ignored
#' @method ggcyto flowSet
#' @return a ggcyto_GatingSet object which is a subclass of ggcyto class.
#' @importFrom rlang quo_name
#' @export
#' @examples
#' 
#' data(GvHD)
#' fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
#' # 1d histogram/densityplot
#' p <- ggcyto(fs, aes(x = `FSC-H`)) 
#' #facet_wrap(~name)` is used automatically
#' p1 <- p + geom_histogram() 
#' p1
#' #overwriting the default faceeting
#' p1 + facet_grid(Patient~Visit)
#'
#' #display density
#' p + geom_density()
#' 
#' #you can use ggjoy package to display stacked density plot
#' require(ggjoy)
#' #stack by fcs file ('name')
#' p + geom_joy(aes(y = name)) + facet_null() #facet_null is used to remove the default facet_wrap (by 'name' column)
#' #or to stack by Visit and facet by patient
#' p + geom_joy(aes(y = Visit)) + facet_grid(~Patient)
#' 
#' # 2d scatter/dot plot
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
#' p <- p + geom_hex(bins = 128)
#' p
#'
ggcyto.flowSet <- function(data, mapping, filter = NULL, max_nrow_to_plot = 5e4, ...){
  #add empty layers recording
  
  
  fs <- data
  #instead of using ggplot.default method to contruct the ggplot object
  # we call the underlining s3 method directly to avoid foritying data at this stage
  p <- ggplot.data.frame(fs, mapping, ...)
  p[["layer.history"]] <- list()
  
  if(!missing(mapping)){
    p[["layer.history"]][["mapping"]] = mapping  
    
    dims <- mapping[grepl("[x|y]", names(mapping))]
    dims <- sapply(dims,quo_name)
    
    
    #update x , y with actual channel name
    frm <- getFlowFrame(fs)
    dims.tbl <- .ldply(dims, function(dim)getChannelMarker(frm, dim), .id = "axis")
    chnl <- dims.tbl[, name]
    
    for(axis_name in names(dims))
      mapping[[axis_name]] <- as.symbol(dims.tbl[axis == axis_name, name])
    #update dims
    p$mapping <- mapping
    
    nDims <- length(dims)
    
    #attach dims to data for more efficient fortify
    attr(fs, "dims") <- dims.tbl
    if(is.null(filter)&&is.finite(max_nrow_to_plot))
      filter <- sampleFilter(size = max_nrow_to_plot)
    attr(fs, "filter") <- filter
    p[["fs"]] <- fs  
    p[["data"]] <- fs #update data as well
    p[["instrument_range"]] <- range(frm)[, chnl, drop = FALSE]
    
    
  }else
    stop("mapping must be supplied to ggplot!")
  
    
  #init axis inversed labels and breaks
  p[["axis_inverse_trans"]] <- list()
  # prepend the ggcyto class attribute
  
  p <- as(p, "ggcyto_flowSet")  
  #add default the2me
  p[["ggcyto_pars"]] <- list()
  
  p[["GeomStats"]] <- list()
  
  p <- p + ggcyto_par_default()
  p <-  p + guides(fill=FALSE) #the counts at legend could be reflecting the subsampled data and we want to hide this from user to avoid confusion
  
  p
}

#' @rdname ggcyto.flowSet
#' @export
ggcyto.ncdfFlowList <- function(data, ...){
  getS3method("ggcyto", "flowSet")(data, ...)
}

#' Reports whether x is a ggcyto_flowSet object
#' @param x An object to test
#' @return TRUE or FALSE
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:2]
#' p <- ggcyto(fs, aes(x = `FSC-H`))
#' is.ggcyto_flowSet(p)
#' @export
is.ggcyto_flowSet <- function(x){
  inherits(x, "ggcyto_flowSet")
}


#' overloaded '+' method for ggcyto
#' 
#' It tries to copy pData from ggcyto object to the gate layers
#' so that the gate layer does not need to have `pd` to be supplied explicitly by users.
#' It also calculates population statistics when geom_stats layer is added.
#' It supports addition ggcyto layers such as 'ggcyto_par' and 'labs_cyto'.
#' 
#' @param e1 An object of class \code{ggcyto_flowSet}
#' @param e2 A component to add to \code{e1}
#' @return ggcyto_flowSet object
#' @rdname ggcyto_flowSet_add
#' @importFrom plyr defaults
#' @export
#' @examples
#' 
#' data(GvHD)
#' fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + geom_hex(bins = 128)
#' #add rectangleGate layer (2d)
#' rect.g <- rectangleGate(list("FSC-H" =  c(300,500), "SSC-H" = c(50,200)))
#' rect.gates <- sapply(sampleNames(fs), function(sn)rect.g)
#' p + geom_gate(rect.gates) + geom_stats()
`+.ggcyto_flowSet` <- function(e1, e2){
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
      
    if      (is.ggcyto_par(e1))  add_par(e1, e2, e2name)
    else if (is.ggcyto_flowSet(e1)) add_ggcyto(e1, e2, e2name)
    
}

#' @rdname ggcyto_flowSet_add
#' @export
setMethod("+", c("ggcyto_flowSet"), `+.ggcyto_flowSet`)


add_ggcyto <- function(e1, e2, e2name){

  dims <- attr(e1[["fs"]], "dims")
  filter <- attr(e1[["fs"]], "filter")
  chnl <- dims[, name]
  flowData <- e1$data
  gs <- e1[["gs"]]
  fs <- e1[["fs"]]
  is.recorded <- attr(e2, "is.recorded")
  if(is.null(is.recorded))
    is.recorded <- FALSE
  if(!is.recorded)
    e1[["layer.history"]][[length(e1[["layer.history"]]) + 1]] <- e2
  # modifying e2 layer by adding pd attribute to layered data 
  # it is used solely for geom_gate.filterList layer
  if(is.ggproto(e2)){
    layer_data <- e2$data  
    if(!is.null(layer_data)){
        pd <- .pd2dt(pData(fs))
    }
    
    if(is(layer_data, "filterList")){
      
        if(!isTRUE(attr(layer_data, "pd")))
            attr(layer_data, "pd") <- pd
                
        #try to fortify the flow data(if it has not been fortified yet) here in order to get actual data range for the reasonable gate interpolation
        #can't do it ealier than this because 'subset` attribute of gs won't be necessarily set until gate layer is added
        if(!is(flowData, "data.table")){ #check if already fortified
          if(!is.null(gs)){#check if it is currently gs
            fs <- fortify_fs(gs)  
            attr(fs, "dims") <- dims
            attr(fs, "filter") <- filter
            e1[["fs"]] <- fs
          }
          dt <- fortify(fs)
          data_range <- apply(dt[, chnl, with = FALSE], 2, range)
          rownames(data_range) <- c("min", "max")  
          e1[["data_range"]] <- data_range
          e1$data <- dt
        }
          
        
        
          
          #do the lazy-fortify here since we need the range info from main flow data

          layer_data <- fortify(layer_data
                                         , data = e1[["data_range"]]
                                         , nPoints = attr(layer_data, "nPoints")
                                         ) 
                          
        attr(layer_data, "annotated") <- TRUE
        e2$data <- layer_data
                
        
    }
    
  }else if(is(e2, "filter.layer")){#coerce filter to filterList to ensure the consistent behavior later for other layers
    e2$data <- filterList(sapply(sampleNames(fs), function(x)e2$filter))
    thisCall <- quote(geom_gate(data = e2$data))
    # copy all the other parameters
    thisCall <-  as.call(c(as.list(thisCall), e2[["gate_params"]]))
    e2.new <- eval(thisCall)
    
    attr(e2.new, "is.recorded") <- TRUE
    e1 <- e1 + e2.new
    return (e1)
  }else if(is(e2, "geom.filters")){
    gates <- e2[["gate"]]
    for(gate in gates){
      thisCall <- quote(geom_gate(data = gate))
      # copy all the other parameters
      thisCall <-  as.call(c(as.list(thisCall), e2[["gate_params"]]))
      e2.new <- eval(thisCall)
      attr(e2.new, "is.recorded") <- TRUE
      e1 <- e1 + e2.new
    }
    return(e1)
  }else if(is(e2, "geom.filtersList")){
    gates <- e2[["gate"]]
    #disassemble filtersList into filterLists
    myFilterLists <- vector(mode = "list", length = length(gates[[1]]))
    for(filts in gates){
      for(i in seq_along(filts)){
        myFilterLists[[i]] <- c(myFilterLists[[i]], filts[[i]])
      }
    }
    for(myFilterList in myFilterLists){
      names(myFilterList) <- names(gates)
      myFilterList <- filterList(myFilterList)
      thisCall <- quote(geom_gate(data = myFilterList))
      # copy all the other parameters
      thisCall <-  as.call(c(as.list(thisCall), e2[["gate_params"]]))
      e2.new <- eval(thisCall)
      attr(e2.new, "is.recorded") <- TRUE
      e1 <- e1 + e2.new
    }
    return(e1)
  }else if(is(e2, "overlay.fs")){
    fs.overlay <- e2[["fs"]]  
    nDim <- nrow(dims)
    if(nDim == 1){
      thisCall <- quote(geom_density(data = fs.overlay))
    }else{
      thisCall <- quote(geom_point(data = fs.overlay))  
    }

    thisCall <- as.call(c(as.list(thisCall), e2[["overlay_params"]]))
    e2.new <- eval(thisCall)
    attr(e2.new, "is.recorded") <- TRUE
    e1 <- `+.ggcyto_flowSet`(e1, e2.new)
    return (e1)
  }else if(is(e2, "GeomStats")){
    e1[["GeomStats"]] <- c(e1[["GeomStats"]], list(e2)) #stats needs to be compputed after limits is set at as.ggplot function
    return(e1)
  }else if (is.ggcyto_par(e2)) {
    # store the ggcyto pars for the lazy-eval elements
    e1$ggcyto_pars <- add_par(e1$ggcyto_pars, e2, deparse(substitute(e2)))
    # apply the non-lazy-eval elements right away
    to_apply <- e2[!names(e2) %in% .lazy_element] 
    
    for(element in names(to_apply)){
      #skip hex_fill for 1d plot
      
      if(element == "hex_fill" && nrow(dims) == 1)
        next
      e2.new <- to_apply[[element]]
      attr(e2.new, "is.recorded") <- TRUE
      e1 <- e1 + e2.new
    }
      
    
    
    return(e1)
  }else if(inherits(e2, "labs_cyto")){
    # instantiated it to a concrete labs object
    
    lab_txt <- list()    
    for(axis_name in dims[, axis]){
      thisDim  <- dims[axis == axis_name, ]
      marker <- thisDim[, desc]
      chnl <- thisDim[, name]
      lab_txt[[axis_name]] <- switch(e2[["labels"]]
                                  , "marker" = ifelse(is.na(marker), chnl, marker)
                                  , "channel" = chnl
                                  , "both" = sub("NA","",paste(chnl, marker))
                                  )
                        
    }
    e2 <- labs(lab_txt)
    
  }else if(is.theme(e2)){
    #have to take care of theme object since it inherits gg class and will
    #cause the dispatch conflicts due to the special rule of groupGeneric
    e1$theme <- ggplot2:::update_theme(e1$theme, e2)
    return(e1)
  }else if(is(e2, "logicalGates")){
    
    if(is(fs, "GatingSet")){
      thisfs <- getData(gs)  
      #make sure pass on subset attr here so that lazy-fortify will succeed in as.ggplot call. 
      #otherwise fortifying usually takes place early at the regular geom_gate layer thought gs directly'
      attr(e1[["data"]], "subset") <- attr(gs, "subset")
    }else{
      thisfs <- fs
    }
    
    subfs <- Subset(thisfs, e2[["indices"]])
    # instantiate the new gate layer
    thisCall <- quote(geom_point(data = subfs))
    # copy all the other parameters
    thisCall <-  as.call(c(as.list(thisCall), e2[["gate_params"]]))
    e2 <- eval(thisCall)
    
  }
  
  ggplot2:::`+.gg`(e1, e2)
  
  
}

# Checking if a layer is geom_gate layer for a filterList
# by checking If the layer data is annotated.
# 
# TODO: It will be more robust to define a new type of proto object to for
# this type of idenitiy checking.
# 
is.geom_gate_filterList <- function(layer){
  isTRUE(attr(layer$data, "annotated"))
}
# Convert data.frame back to original flowSet format
# 
# It is used for gating purporse for geom_stats layer
#.df2fs <- function(df){
#  
#  pd <- attr(df, "pd")
#
#  frlist <- dlply(df, .variables = ".rownames", function(sub_df){
#    markers <- setdiff(colnames(sub_df), colnames(pd))
#    fr <- flowFrame(exprs = as.matrix(sub_df[, markers]))
#    fr
#  })
#  fs <- as(frlist, "flowSet")
#  pData(fs) <- name_rows(pd)
#  fs
#}
#


#' Convert data.frame back to filterList
#' 
#' It is used for gating purporse for geom_stats layer
#' (no longer needed since the data is now not foritfied until print.ggcyo)
#' @importFrom plyr dlply
#' @param pcols the pData columns
.filterList2dataframe <- function(df, pcols = ".rownames"){
  
  markers <- setdiff(colnames(df), pcols)
  df <- df[, c(markers, ".rownames"), with = FALSE]
  # unfortunately data.table j expression won't return a list
  # instead it will always try to coerce the list back to dt, which is not desirable here
  glist <- dlply(df, .variables = ".rownames", function(sub_df){
    
    sub_df[[".rownames"]] <- NULL
    
    .gate2dataframe(sub_df)    
  })
  
  filterList(glist)
}

# Convert data.frame back to original gate format
.gate2dataframe <- function(df){
  markers <- colnames(df)
  nDim <- length(markers)
  if(nDim == 2){
    
    #check if can be coerced to rectangleGate first
    #because open-end polygonGate would be problematic
    #in further operation such as 'filter' or 'fortify' with the interpolation
    verts <- sapply(markers, function(marker)unique(df[[marker]]), simplify = FALSE)
    if(all(sapply(verts, length) == 2))
    {
     
     rectangleGate(verts)
    }else
      polygonGate(df)  
    
  }else if (nDim == 1){
    rectangleGate(df)
  }else
    stop("invalid dimension number!")
  
}

