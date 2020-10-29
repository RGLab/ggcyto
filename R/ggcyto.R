#' Plot cytometry data using the ggcyto API
#' 
#' \code{ggcyto()} initializes a ggcyto object that inherits ggplot class.
#' Similarly the + operator can be used to add layers to the
#' existing ggcyto object. 
#'
#' To invoke \code{ggcyto}:
#' \itemize{
#'    \item \code{ggcyto(fs, aes(x, y, <other aesthetics>))}
#'   }
#'
#' @name ggcyto
#' @aliases ggcyto.default ggcyto.flowSet ggcyto.GatingHierarchy ggcyto.GatingSet
#' ggcyto.GatingSetList
#' @import methods ggplot2 flowCore ncdfFlow flowWorkspace
#' @importFrom rlang quo_name
#' @param data The data source. A core cytometry data structure. (flowSet, flowFrame, ncdfFlowSet, GatingSet or GatingHierarchy)
#' @param mapping default list of aesthetic mappings (these can be colour,
#'   size, shape, line type -- see individual geom functions for more details)
#' @param filter a flowcore gate object or a function that takes a flowSet and channels as input and returns a data-dependent flowcore gate.
#'                The gate is used to filter the flow data before it is plotted.
#' @param max_nrow_to_plot the maximum number of cells to be plotted. When the actual data exceeds it, The subsampling process will be triggered to speed up plotting. Default is 5e4. To turn off the subsampling, simply set it to a large enough number or Inf.
#' @param subset character that specifies the node path or node name in the case of GatingSet. 
#'               Default is "_parent_", which will be substituted with the actual node name 
#'               based on the geom_gate layer to be added later.
#' @param ... other arguments passed to specific methods
#' @return ggcyto object 
#' @examples
#' 
#' data(GvHD)
#' fs <- GvHD[1:3]
#' #construct the `ggcyto` object (inherits from `ggplot` class)
#' p <- ggcyto(fs, aes(x = `FSC-H`)) 
#' p + geom_histogram() 
#'
#' # display density/area
#' p + geom_density()
#' p + geom_area(stat = "density") 
#' 
#' # 2d scatter plot
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
#' p + geom_hex(bins = 128)
#' # do it programatically through aes_string and variables
#' col1 <- "`FSC-H`" #note that the dimension names with special characters needs to be quoted by backticks
#' col2 <- "`SSC-H`"
#' ggcyto(fs, aes_string(col1,col2)) + geom_hex()
#' 
#' ## More flowSet examples
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
#' #you can use ggridges package to display stacked density plot
#' require(ggridges)
#' #stack by fcs file ('name')
#' p + geom_density_ridges(aes(y = name)) + facet_null() #facet_null is used to remove the default facet_wrap (by 'name' column)
#' #or to stack by Visit and facet by patient
#' p + geom_density_ridges(aes(y = Visit)) + facet_grid(~Patient)
#' 
#' # 2d scatter/dot plot
#' p <- ggcyto(fs, aes(x = `FSC-H`, y =  `SSC-H`))
#' p <- p + geom_hex(bins = 128)
#' p
#' 
#' ## GatingSet
#' dataDir <- system.file("extdata",package="flowWorkspaceData")
#' gs <- load_gs(list.files(dataDir, pattern = "gs_manual",full = TRUE))
#' # 2d plot 
#' ggcyto(gs, aes(x = CD4, y = CD8), subset = "CD3+") + geom_hex(bins = 64)
#' 
#' # 1d plot
#' ggcyto(gs, aes(x = CD4), subset = "CD3+")  + geom_density()
#' 
#' @export
ggcyto <- function(data = NULL, ...) UseMethod("ggcyto")


#' Reports whether x is a ggcyto object
#' @param x An object to test
#' @return TRUE/FALSE
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:2]
#' p <- ggcyto(fs, aes(x = `FSC-H`))
#' is.ggcyto(p)
#' @export
is.ggcyto <- function(x) inherits(x, "ggcyto")

#' @export
ggcyto.default <- function(data = NULL, mapping = aes(), ...) {
  ggcyto.flowSet(fortify_fs(data, ...), mapping, ...)
}

#' Draw ggcyto on current graphics device.
#'
#' A wrapper for print.ggplot. It converts the ggcyto to conventional ggplot object before printing it.
#' This is usually invoked automatically when a ggcyto object is returned to R console.
#' 
#' @name print.ggcyto
#' @aliases print,ggcyto-method plot.ggcyto show.ggcyto show,ggcyto-method
#' @return nothing
#' @param x ggcyto object to display
#' @param ... other arguments not used by this method
#' 
#' @export
#' @method print ggcyto
print.ggcyto <- function(x, ...) {
  
    
    x <- ggplot2:::plot_clone(x) #clone plot to avoid tampering original x due to ther referenceClass x$scales
    x <- as.ggplot(x) 
    ggplot2:::print.ggplot(x)
}

#' @rdname print.ggcyto
#' @method plot ggcyto
#' @export
plot.ggcyto <- print.ggcyto

#--------These S4 methods exsits for plotting ggcyto object automatically in R console---------------#
#' @export
setMethod("print", c("ggcyto"), print.ggcyto)


#' @param object ggcyto object
#' @rdname print.ggcyto
#' @method show ggcyto
#' @export
show.ggcyto <- function(object){print(object)}

#' @method show ggcyto
#' @export
setMethod("show", "ggcyto", show.ggcyto)

#' It fortifies the data, fills some default settings and returns a regular ggplot object.
#' 
#' The orginal data format is preserved during the ggcyo constructor because they still need to be used during the plot building process.
#' This function is usually called automatically in the print/plot method of ggycyto. Sometime it is useful to coerce it to ggplot explictily 
#' by user so that it can be used as a regular ggplot object.
#' 
#' @param x ggcyto object with the data that has not yet been fortified to data.frame.
#' @param pre_binning whether to pass the binned data to ggplot to avoid the overhead to scaling the original raw data for geom_hex layer
#' 
#' @return ggplot object
#' @examples 
#' data(GvHD)
#' fs <- GvHD[1:3]
#' #construct the `ggcyto` object (inherits from `ggplot` class)
#' p <- ggcyto(fs, aes(x = `FSC-H`)) + geom_histogram() 
#' class(p) # a ggcyto object
#' p$data # data has not been fortified
#' p1 <- as.ggplot(p) # convert it to a ggplot object explictily 
#' class(p1) 
#' p1$data # data is fortified
#' @importFrom hexbin hexbin hcell2xy
#' @export
as.ggplot <- function(x, pre_binning = FALSE){

  #####################
  #lazy-fortifying the plot data
  #####################
  dims <- attr(x[["data"]], "dims")
  aes_names <- dims[, axis]
  chnls <- dims[, name]
  
  instrument_range <- x[["instrument_range"]]
  dtype <- class(x[["data"]])
  gs <- fs <- NULL
  #data needs to be fortified here if geom_gate was not added
  if(dtype != "data.table"){
    if(dtype %in% c("GatingSet", "GatingSetList")){#check if it is currently gs
      gs <- x[["data"]]
      fs <- fortify_fs(gs)
      
    }else
      fs <- x[["data"]]
    x[["data"]] <- fortify(fs)
    data_range <- apply(x[["data"]][, chnls, with = FALSE], 2, range)
    rownames(data_range) <- c("min", "max")  
  }else
    data_range <- x[["data_range"]]
  
  #post process geom_hex layers 
  for(i in seq_along(x$layers))
  {
    e2 <- x$layers[[i]]
    #with the one that is based on data limits to avoid oversized bins caused by exagerated gates
    if(is(e2$geom, "GeomHex"))
    {
      bins <- e2$stat_params[["bins"]]
      if(is.null(bins))
        bins <- 32
      if(bins > 0)
      {
        if(is.null(e2$stat_params[["binwidth"]]))
        {
          transformed_range <- data_range
          for(col in c("x","y")){
            if(!is.null(x$scales$get_scales(col)$secondary.axis)){
              transformed_range[, dims[axis==col, name]] <- x$scales$get_scales(col)$transform(transformed_range[,dims[axis==col, name]]) 
            }
          }
          dummy_scales <- sapply(c("x", "y"), function(i) scale_x_continuous(limits = as.vector(transformed_range[,dims[axis==i, name]])))
          e2$stat_params[["binwidth"]] <- ggplot2:::hex_binwidth(e2$stat_params[["bins"]], dummy_scales)
          x$layers[[i]] <- e2
        }
        #optionally pass the binned data to ggplot for speed
        if(pre_binning)
        {
          pd <- pData(fs)
          df <- x[["data"]]
          cols <- c(".rownames", colnames(pd))
  
          df <- df[, {
  
            binned <- hexbin::hexbin(.SD, xbins = e2$stat_params[["bins"]])
            sd <- hexbin::hcell2xy(binned)
            names(sd) <- colnames(.SD)
            data.table(data.frame(sd,hex_cell_id = binned@cell, count=binned@count, check.names = FALSE))
          }, by = cols]
  
          x[["data"]] <- df
          e2 <- geom_hex(stat="identity",aes(fill=count))
          x$layers[[i]] <- e2
        }
      }else
      {
        df <- x[["data"]]
        cols <- densCols(df[, chnls, with = F], colramp =colorRampPalette(rev(brewer.pal(11, "Spectral"))))
        x$layers[[i]] <- geom_point(color = cols, size = 0.2)
      }
    }
  }
  #####################
  #update default scales
  #####################
  breaks <- x[["axis_inverse_trans"]]
  stats_limits <- list()
  trans <- list()
  for(this_aes in aes_names)
  {
    dim <- dims[axis == this_aes, name]
    # set limits
    if(!x$scales$has_scale(this_aes))
    {
      #add new one if not present 
      new.scale <- ggplot2:::make_scale("continuous", this_aes)
      
      x <- ggplot2:::`+.gg`(x, new.scale)      
      
    }
    ind <- which(x$scales$find(this_aes))
    #apply lazy limits setting
    par_limits <- x$ggcyto_pars[["limits"]]
    if(is.character(par_limits)&&par_limits == "data")
    {
      this_limits <- data_range[, dim]
      x$coordinates[["limits"]][[this_aes]] <- this_limits
      
    }else if(!is.null(par_limits))
      stop("How did you end up here?")
        
    stats_limits[[dim]] <- x$coordinates[["limits"]][[this_aes]]
    #update breaks and labels
    thisBreaks <- breaks[[this_aes]]
    if(!is.null(thisBreaks)){
      # set limits
      if(!x$scales$has_scale(this_aes))
      {
        #add new one if not present 
        new.scale <- ggplot2:::make_scale("continuous", this_aes)
        x <- x + new.scale
      }
      ind <- which(x$scales$find(this_aes))
      x$scales$scales[[ind]]$breaks <- thisBreaks[["at"]]
      x$scales$scales[[ind]]$labels <- thisBreaks[["label"]]  
    }
    
  }
  if(!is.null(data_range)&&length(stats_limits)!=0)
  {
    stats_limits <- as.data.frame(stats_limits, check.names = FALSE)
    stats_limits[["density"]] <- c(0,1e-4)
    
  }else
    stats_limits <- NULL
  
  #retrospect geom_hex layer to fix binwidth
  for(i in seq_along(x$layers))
  {
    e2 <- x$layers[[i]]
    #override default bindwidth that is based on the entire scale limits
    #with the one that is based on data limits to avoid oversized bins caused by exagerated gates
    if(is(e2$geom, "GeomHex"))
    {
      bw <- e2$stat_params[["binwidth"]]
      bins <- e2$stat_params[["bins"]]
      if(is.null(bins)||length(bins)==0)
      {
        bins <- formals(stat_bin_hex)[["bins"]]
      }
      
      if(is.null(bw)||length(bw)==0)
      {
        transformed_range <- data_range
        for(col in c("x","y")){
          if(!is.null(x$scales$get_scales(col)$secondary.axis)){
            transformed_range[, dims[axis==col, name]] <- x$scales$get_scales(col)$transform(transformed_range[,dims[axis==col, name]]) 
          }
        }
        dummy_scales <- sapply(c("x", "y"), function(i)scale_x_continuous(limits = as.vector(transformed_range[,dims[axis==i, name]])))
        e2$stat_params[["binwidth"]] <- ggplot2:::hex_binwidth(bins, dummy_scales)
        x$layers[[i]] <- e2
      }
      
    }
    
  }
  #lazy parsing stats layer since the stats_limits is set at the end
  for(e2 in x[["GeomStats"]])
  {
    gate <- e2[["gate"]]
    #parse the gate from the each gate layer if it is not present in the current geom_stats layer
    if(is.null(gate))
    {
      
      pd <- .pd2dt(pData(fs))
      gates_parsed <- lapply(x$layers, function(layer){
        
        if(is.geom_gate_filterList(layer))#restore filter from fortified data.frame
          .dataframe2filterList(layer$data, colnames(pd))
        else
          NULL
      })
      #remove NULL elements
      gates_parsed <- flowWorkspace:::compact(gates_parsed)
    }else{
      gates_parsed <- list(gate)
    }             
    
    
    if(length(gates_parsed) == 0)
      stop("geom_gate layer must be added before geom_stats!")
    
    
    # compute pop stats for each gate layer and 
    value <- e2[["value"]]
    stat_type <- e2[["type"]]
    

    #add default density range
    #In order to ensure the stats visiblity
    #try to put it closer to zero because we don't know the actual density range
    if(!is.null(data_range))
      data_range <- as.data.frame(data_range)
    
    
    negated <- e2[["negated"]]
    adjust <- e2[["adjust"]]
    location <- e2[["location"]]
    digits <- e2[["digits"]]
    if(length(trans)>0)
    {
      translist <- lapply(trans, function(t)t[["transform"]])
      translist <- transformList(names(translist), translist)
      inverselist <- lapply(trans, function(t)t[["inverse"]])
    }
    if(length(trans)>0&&is.null(value))#means fs will be used to compute stats and thus needs to be scaled properly
    {
      suppressMessages(fs <- transform(fs, translist))
    }
    for(gate in gates_parsed){
      if(length(trans)>0)
        gate <- transform(gate, translist)
      
      # Honor manual choice of location == "data", "plot", or "fixed"
      if(location == "gate"){
        #TODO: compute the actual data range from population data
        if(is(gate[[1]], "booleanFilter"))
          #bypass stats_postion computing to use data_range as gate_range(as a hack for now)
          location <- "data"
      }
        
      stats <- compute_stats(fs, gate
                             , type = stat_type
                             , value = value
                             , data_range = data_range
                             , limits = stats_limits
                             , negated = negated
                             , adjust = adjust
                             , digits = digits
                             , location = location)
      
      #restore the stats dimensions to raw scale
      if(length(trans)>0)
      {
        for(param in names(inverselist))
        {
          thisTrans <- inverselist[[param]]
          v <- thisTrans(stats[[param]])
          stats[, (param) := v]
        }  
      }
      
        
      # instantiate the new stats layer
      thisCall <- quote(geom_label(data = stats))
      # copy all the other parameters
      thisCall <-  as.call(c(as.list(thisCall), e2[["geom_label_params"]]))
      
      e2.new <- eval(thisCall)
      attr(e2.new, "is.recorded") <- TRUE
      # update aes
      stats_mapping <- aes_string(label = "value")
      #add y aes for 1d density plot
      dims <- x$mapping[grepl("[x|y]", names(x$mapping))]
      dims <- sapply(dims, quo_name)
      if(length(dims) == 1)
        stats_mapping <- defaults(stats_mapping, aes(y = density))
      e2.new$mapping <- defaults(e2.new$mapping, stats_mapping)  
      
      x <- ggplot2:::`+.gg`(x, e2.new)      
    }
  }
  
  #strip the ggcyto class attributes
  asS3(x)
}
