#' @rdname ggcyto
#' @export
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
  # the counts at legend could be reflecting the subsampled data and we want to hide this from user to avoid confusion
  p <- p + theme(legend.position = 'none')
  
  p
}

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
#' @name ggcyto_add
#' @usage e1 + e2
#' @aliases + +.ggcyto_flowSet +,ggcyto_flowSet-method
#' +.ggcyto_GatingSet +,ggcyto_GatingSet-method
#' +.ggcyto_GatingLayout +,ggcyto_GatingLayout-method
#' +.ggcyto_ncdfFlowList +,ggcyto_GatingLayout,ANY-method
#' +,ggcyto_GatingSet,ANY-method +,ggcyto_flowSet,ANY-method
#' @param e1 An object of class \code{ggcyto} or a class inheriting from \code{ggcyto}, such
#' as \code{ggcyto_flowSet}, \code{ggcyto_GatingSet}, or \code{ggcyto_GatingLayout}. In the case
#' of \code{ggcyto_GatingLayout}, the component of \code{e2} will be added to each subsidiary plot.
#' @param e2 A component to add to \code{e1}
#' @return ggcyto object
#' @importFrom plyr defaults
#' @examples
#' 
#' ## flowSet
#' data(GvHD)
#' fs <- GvHD[subset(pData(GvHD), Patient %in%5:7 & Visit %in% c(5:6))[["name"]]]
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`)) + geom_hex(bins = 128)
#' #add rectangleGate layer (2d)
#' rect.g <- rectangleGate(list("FSC-H" =  c(300,500), "SSC-H" = c(50,200)))
#' rect.gates <- sapply(sampleNames(fs), function(sn)rect.g)
#' p + geom_gate(rect.gates) + geom_stats()
#' 
#' ## GatingSet
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' p <- ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") + geom_hex(bins = 64)
#' p <- p + geom_gate("CD4") + geom_stats() #plot CD4 gate and it is stats
#' p
#' p + axis_x_inverse_trans() #inverse transform the x axis into raw scale
#' 
#' ## GatingLayout
#' #autplot for GatingSet
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' gh <- gs[[1]]
#' p <- autoplot(gh)
#' class(p)
#' # customize the font size of strip text for each ggcyo plots contained in GatingLayout object
#' p + theme(strip.text = element_text(size = 14))
#' @export
`+.ggcyto_flowSet` <- function(e1, e2){
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
      
    if      (is.ggcyto_par(e1))  add_par(e1, e2, e2name)
    else if (is.ggcyto_flowSet(e1)) add_ggcyto(e1, e2, e2name)
    
}

#' @export
setMethod("+", c("ggcyto_flowSet"), `+.ggcyto_flowSet`)

#'@importFrom rlang !!!
#' @importFrom ggplot2 ggplot_add
add_ggcyto <- function(e1, e2, e2name){
  fs <- e1[["data"]]
  dims <- attr(fs, "dims")
  chnl <- dims[, name]

  is.recorded <- attr(e2, "is.recorded")
  if(is.null(is.recorded))
    is.recorded <- FALSE
  if(!is.recorded)
    e1[["layer.history"]][[length(e1[["layer.history"]]) + 1]] <- e2
  # modifying e2 layer by adding pd attribute to layered data 
  # it is used solely for geom_gate.filterList layer
  if(is.Coord(e2))
  {
    #clear the lazy element (i.e. limits = "data") for non-lazy limits setting
    #so that it won't be applied later on
    e1$ggcyto_pars <- modifyList(e1$ggcyto_pars, list(limits = NULL))
  }else if(is.ggproto(e2)){
    layer_data <- e2$data  
    if(!is.null(layer_data)){
      pd <- .pd2dt(pData(fs))
    }
    
    if(is(layer_data, "filterList")){
      
      if(!isTRUE(attr(layer_data, "pd")))
        attr(layer_data, "pd") <- pd
      #do the lazy-fortify here since we  may need the pd info from main flow data
      
      layer_data <- fortify(layer_data)
      
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
  }else if(is(e2, "gateNull")){
    torm <- integer()
    for(i in seq_along(e1[["layers"]]))
    {
      if(is(e1[["layers"]][[i]], "geomGate"))
        torm <- c(torm, i)
    }
    e1[["layers"]][torm] <- NULL
    #rm stats as well
    e1 <- e1 + stats_null()
    return(e1)
  }else if(is(e2, "statsNull")){
    e1[["GeomStats"]] <- NULL
    return(e1)
  }else if (is.ggcyto_par(e2)) {
      
    for(element in names(e2)){
      #skip hex_fill for 1d plot
      
      if(element == "hex_fill" && nrow(dims) == 1)
        next
      e2.new <- e2[[element]]
      #apply instrument range to limits
      if(element == "limits" ){
            instrument_range <- e1[["instrument_range"]]
            if(is.list(e2.new))
            {
              this_limits <- e2.new
            }else if(is.character(e2.new))
            {
              this_limits <- list()
              if(e2.new == "instrument")
              {
                for(aes_name in dims[, axis])
                  this_limits[[aes_name]] <- instrument_range[, dims[axis == aes_name, name]]    
              }else if(e2.new == "data")
              {
                # store the ggcyto pars for the lazy-eval elements for we may not have the final version of data yet at this stage
                e1$ggcyto_pars <- add_par(e1$ggcyto_pars, e2, deparse(substitute(e2)))
                next
              }else
                stop("invalid 'limits' setting!")
            
            }
            #clear the lazy element (i.e. limits = "data") for non-lazy limits setting
            #so that it won't be applied later on
            e1$ggcyto_pars <- modifyList(e1$ggcyto_pars, list(limits = NULL))
            e2.new <- coord_cartesian(xlim = this_limits[["x"]], ylim = this_limits[["y"]])
      }
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
                                  , "both" = paste0(chnl, ifelse(is.na(marker), "", paste0(" ", marker)))
                                  )
                        
    }
    e2 <- labs(!!!lab_txt)
    
  }else if(is.theme(e2)){
    #have to take care of theme object since it inherits gg class and will
    #cause the dispatch conflicts due to the special rule of groupGeneric
   ggplot_add(e2, e1)
  }else if(is(e2, "logicalGates")){
    
    if(is(fs, "GatingSet")){
      thisfs <- gs_pop_get_data(fs)  
      #make sure pass on subset attr here so that lazy-fortify will succeed in as.ggplot call. 
      #otherwise fortifying usually takes place early at the regular geom_gate layer thought gs directly'
      attr(e1[["data"]], "subset") <- attr(fs, "subset")
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
#' @noRd 
.dataframe2filterList <- function(df, pcols = ".rownames"){
  
  markers <- setdiff(colnames(df), pcols)
  df <- df[, c(markers, ".rownames"), with = FALSE]
  # unfortunately data.table j expression won't return a list
  # instead it will always try to coerce the list back to dt, which is not desirable here
  glist <- dlply(df, .variables = ".rownames", function(sub_df){
    
    sub_df[[".rownames"]] <- NULL
    
    .dataframe2gate(sub_df)    
  })
  
  filterList(glist)
}

# Convert data.frame back to original gate format
.dataframe2gate <- function(df){
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

