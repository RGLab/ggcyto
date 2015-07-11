#' Create a new ggcyto plot from a flowSet
#'
#' @param data default flowSet/GatingSet for plot
#' @param mapping default list of aesthetic mappings (these can be colour,
#'   size, shape, line type -- see individual geom functions for more details)
#'  @param filter a flowcore gate object or a function that takes flowSet and channels as input and returns a data-dependent flowcore gate
#'                The gate is used to filter the flow data before it is plotted. 
#' @param ... ignored
#' @method ggcyto flowSet
#' @export
ggcyto.flowSet <- function(data, mapping, filter = NULL, ...){
  #instead of using ggplot.default method to contruct the ggplot object
  # we call the underlining s3 method directly to avoid foritying data at this stage
  p <- ggplot2:::ggplot.data.frame(data, mapping, ...)
  
  if(!missing(mapping)){
    dims <- sapply(mapping,as.character)
    dims <- dims[grepl("[x|y]", names(dims))]
    
    #update x , y with actual channel name
    frm <- getFlowFrame(data)
    dims.tbl <- .ldply(dims, function(dim)getChannelMarker(frm, dim), .id = "axis")
    
    for(axis_name in names(dims))
      mapping[[axis_name]] <- as.symbol(dims.tbl[axis == axis_name, name])
    #update dims
    p$mapping <- mapping
    
    nDims <- length(dims)
    #attach dims to data for more efficient fortify
    attr(p$data, "dims") <- dims.tbl
    attr(p$data, "filter") <- filter
    
  }else
    stop("mapping must be supplied to ggplot!")
  
    
  #init axis inversed labels and breaks
  p[["axis_inverse_trans"]] <- list()
  # prepend the ggcyto class attribute
  class(p) <- c("ggcyto", class(p))  
  class(p) <- c("ggcyto_flowSet", class(p))  
  #add default the2me
  p[["ggcyto_pars"]] <- list()
  p <- p + ggcyto_par_default()
  
  p
}
#' Reports whether x is a ggplot object
#' @param x An object to test
#' @export
is.ggcyto_flowSet <- function(x) inherits(x, "ggcyto_flowSet")

#' overloaded '+' method for ggcyto
#' 
#' It tries to copy pData from ggcyto object to the gate layers
#' so that the gate layer does not need to have `pd` to be supplied explicitly by users.
#' 
#' @param e1 An object of class \code{ggcyto_flowSet}
#' @param e2 A component to add to \code{e1}
#' 
#' @method + ggcyto_flowSet
#' @rdname ggcyto_flowSet_add
#' @importFrom plyr defaults
#' @export
`+.ggcyto_flowSet` <- function(e1, e2) {
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))
  
  if      (is.ggcyto_par(e1))  add_par(e1, e2, e2name)
  else if (is.ggcyto_flowSet(e1)) add_ggcyto(e1, e2, e2name)
}

add_ggcyto <- function(e1, e2, e2name){
#   browser()  
  
  dims <- attr(e1$data, "dims")
  # modifying e2 layer by adding pd attribute to layered data 
  # it is used solely for geom_gate.filterList layer
  if(is.proto(e2)){
    layer_data <- e2$data
    pd <- .pd2dt(pData(e1$data))
    if(is(layer_data, "geom_gate_filterList")){
        if(!isTRUE(attr(layer_data, "annotated"))){
          
          layer_data <- merge(layer_data, pd, by = ".rownames")  
          attr(layer_data, "annotated") <- TRUE
          e2$data <- layer_data
        }
      
    }else if(e2$geom$objname == "popStats"){
      gate <- e2$stat_params[["gate"]]
      #parse the gate from the each gate layer if it is not present in the current geom_stats layer
      if(is.null(gate))
      {
        
        gates_parsed <- lapply(e1$layers, function(layer){
          
                              if(is.geom_gate_filterList(layer))
                                .df2gate(layer$data, colnames(pd))
                              else
                                NULL
                              })
        #remove NULL elements
        gates_parsed <- Filter(function(x)!is.null(x), gates_parsed)
      }else{
        gates_parsed <- list(gate)
      }             
      
      
      if(length(gates_parsed) == 0)
        stop("geom_gate layer must be added before geom_stats!")
      
    
      # compute pop stats for each gate layer and 
      value <- e2$stat_params[["value"]]
      stat_type <- e2$stat_params[["type"]]
      data_range <- e2$stat_params[["data_range"]]
      adjust <- e2$stat_params[["adjust"]]
      fs <- e1$data
      for(gate in gates_parsed){
        stats <- compute_stats(fs, gate, type = stat_type, value = value, data_range = data_range, adjust = adjust)
        
        # instantiate the new stats layer(somehow direct clone by proto doesn't work here)
        e2.new <- geom_stats(data = stats)
        # copy all the other parameters
        e2.new$geom_params <- defaults(e2.new$geom_params, e2$geom_params)
        e2.new$stat_params <- defaults(e2.new$stat_params, e2$stat_params)
        
        # update aes
        stats_mapping <- aes_string(label = stat_type)
        #add y aes for 1d density plot
        dims <- sapply(e1$mapping,as.character)
        dims <- dims[grepl("[x|y]", names(dims))]
        if(length(dims) == 1)
          stats_mapping <- defaults(stats_mapping, aes(y = density))
        e2.new$mapping <- defaults(e2.new$mapping, stats_mapping)  
      
        e1 <- ggplot2:::`+.gg`(e1, e2.new)      
      }
      
      return(e1)
    }
    
  }else if (is.ggcyto_par(e2)) {
    # store the ggcyto pars for the lazy-eval elements
    e1$ggcyto_pars <- add_par(e1$ggcyto_pars, e2, deparse(substitute(e2)))
    # apply the non-lazy-eval elements right away
    to_apply <- e2[!names(e2) %in% .lazy_element] 
    
    for(element in names(to_apply)){
      #skip hex_fill for 1d plot
      
      if(element == "hex_fill" && nrow(dims) == 1)
        next
      e1 <- e1 + to_apply[[element]]
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
#' Convert data.frame back to original gate format
#' 
#' It is used for gating purporse for geom_stats layer
#' (no longer needed since the data is now not foritfied until print.ggcyo)
#' @importFrom plyr dlply
#' @param pcols the pData columns
.df2gate <- function(df, pcols){
  
  markers <- setdiff(colnames(df), pcols)
  nDim <- length(markers)
  df <- df[, c(markers, ".rownames"), with = FALSE]
  # unfortunately data.table j expression won't return a list
  # instead it will always try to coerce the list back to dt, which is not desirable here
  glist <- dlply(df, .variables = ".rownames", function(sub_df){
    
    sub_df[[".rownames"]] <- NULL
    if(nDim == 2){
      g <- polygonGate(sub_df)  
    }else if (nDim == 1){
#       browser()
      g <- rectangleGate(sub_df)
    }else
      stop("invalid dimension number!")
    g
  })
  
  filterList(glist)
}
