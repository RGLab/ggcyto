#' @rdname ggcyto
#' @export
ggcyto.GatingSet <- function(data, mapping, subset = "_parent_", ...){
  p <- ggcyto.flowSet(data = data, mapping = mapping, ...)
  attr(p[["data"]], "subset") <- subset#must attach parent info to attribute since foritfy method needs it to coerce it to data.frame
  p[["layer.history"]][["subset"]] = subset

  
  p <- p + labs(title  = subset)
  
  # prepend the ggcyto class attribute
  p <- as(p, "ggcyto_GatingSet")  
  
  p <- p + theme(plot.title = element_text(face="bold"))
  
  p
}

#' @rdname ggcyto
#' @export
ggcyto.GatingSetList <- function(data, ...){
  getS3method("ggcyto", "GatingSet")(data, ...)
}
#' @rdname ggcyto
#' @export
ggcyto.GatingHierarchy <- function(data, ...){
  data <- as.GatingSet(data)
  ggcyto(data, ...)  
}
as.GatingSet <- function(gh){
  sn <- sampleNames(gh)
  gs <- as(gh, "GatingSet")
  trans <- gs@transformation 
  if(!is.null(trans)&&length(trans)>0)
  {
    trans <- list(trans)
    names(trans) <- sn
    gs@transformation <- trans
  }
    
  gs[sn]
}

#' @export
`+.ggcyto_GatingSet` <- function(e1, e2){
  add_ggcyto_gs(e1,e2)
}
add_ggcyto_gs <- function(e1, e2){
  
  plot_mapping <- e1$mapping
  prj <- sapply(plot_mapping, quo_name)

  is.recorded <- attr(e2, "is.recorded")
  if(is.null(is.recorded))
    is.recorded <- FALSE
  if(!is.recorded)
    e1[["layer.history"]][[length(e1[["layer.history"]]) + 1]] <- e2
  gs <- e1[["data"]]
    
  parent <- attr(gs, "subset")
  if(is(e2, "overlay.node")){
    node <- e2[["node"]]  
    fs.overlay <- gs_pop_get_data(gs, node)
    
    thisCall <- quote(geom_overlay(fs.overlay))
    thisCall <- as.call(c(as.list(thisCall), e2[["overlay_params"]]))
    e2.new <- eval(thisCall)
    attr(e2.new, "is.recorded") <- TRUE
    e1 <- `+.ggcyto_flowSet`(e1, e2.new)
    return (e1)
  }else if(is(e2, "gs.node")){
    #instantiate e2 layer as a specific gate layer

    nodes <- e2[["node"]]  
    #instantiate the parent by the first node if it is not yet been done
    if(isTRUE(nodes == "_child_")){
      if(parent == "_parent_")
        stop("either 'subset' in ggcyto object or 'data' in geom_gate layer needs to be specified!")
      
      nodes <- .getChildren_by_projection(gs, parent, x = prj[1], y = prj[2])
    }
    #instantiate the subset/parent info
    if(parent == "_parent_"){
      parent <- gs_pop_get_parent(gs[[1]], nodes[[1]])
      attr(e1[["data"]], "subset") <- parent
    }

    if(e1$labels[["title"]] == "_parent_")
      e1$labels[["title"]] <- parent
    
    for(node in nodes)
    {
      gate <- filterList(gs_pop_get_gate(gs, node))
      #must convert bool gate to indices since
      #flowset doesn't know about booleanFilter
      if(is(gate[[1]], "booleanFilter"))
        gate <- lapply(gs, gh_pop_get_indices, y = node)
      
      thisCall <- quote(geom_gate(gate))
      thisCall <- as.call(c(as.list(thisCall), e2[["gate_params"]]))
      e2.new <- eval(thisCall)
      attr(e2.new, "is.recorded") <- TRUE
      e1 <- `+.ggcyto_flowSet`(e1, e2.new)
    }
    
    #store nodes so that geom_stats layers can use it
    e1[["nodes"]] <- c(e1[["nodes"]], nodes)
   return (e1) 
    
  }else if(is(e2, "GeomStats")){
    adjust <- e2[["adjust"]]
    
    #grab the nodes info from previous gate layers
    nodes.geom_gate <- e1[["nodes"]]
    # if(is.null(nodes.geom_gate))
    #   stop("geom_gate must be added before adding geom_stats!")
    gates <- e2[["gate"]]      
    #when gate argument is absent from the stats layer, substitute it with nodes from gates layers
    if(is.null(gates))
      gates <- nodes.geom_gate
    
    if(is.character(gates)){#if it is character then use it as node names
      #update the gate argument with the actual gates
      for(node in gates){
        gates <- filterList(gs_pop_get_gate(gs, node))
       
        stat_type <- e2[["type"]]
        value <- e2[["value"]]
        digits <- e2[["digits"]]
        
        #grab the pre-calculated stats
        if(is.null(value)){
          value <- lapply(stat_type, function(stype){
            if(stype == "count")
              lapply(gs, gh_pop_get_count, y = node)
            else if(stype == "percent")
              lapply(gs, gh_pop_get_proportion, y = node)
            else if(stype == "gate_name")
              sapply(sampleNames(gs), function(sn)basename(node), simplify = FALSE)
          })
          
        }
        
       negated <- flowWorkspace:::gh_pop_is_negated(gs[[1]], node)
       thisCall <- quote(geom_stats(gates))
       
       thisCall <- as.call(c(as.list(thisCall)
                             , list(value = value
                                    , type = stat_type
                                    , negated = negated
                                    , adjust = adjust
                                    , digits = digits
                                    )
                             , e2[["geom_label_params"]]
                             )
                           )
       e2.new <- eval(thisCall)
       attr(e2.new, "is.recorded") <- TRUE
       
       e1 <- `+.ggcyto_flowSet`(e1, e2.new) 
      }
    }else
      e1 <- `+.ggcyto_flowSet`(e1, e2) #either no nodes and gate present, or gate is supplied at geom_stats, simply add stats layer as it is for lazy-eval later 
    
    return(e1)
    
  }
    
  if(inherits(e2, "raw_scale")){
    #get channel name
    axis_name <- ifelse(any(grepl("^x$", e2[["aesthetics"]])), "x", "y")
    channel <- prj[[axis_name]]
    #compute the breaks and labels
    res <- prettyAxis(gs[[1]], channel)
    
    #instead of adding a separate scale layer
    #we store it to be used later on 
    #so that it won't be interfering with the other scale layers(g.g. xlim)
    
    e1[["axis_inverse_trans"]][[axis_name]] <- res
    return(e1)
#     #modify e2
#     e2[["breaks"]] <- res[["at"]]
#     e2[["labels"]] <- res[["label"]]
  }
  attr(e2, "is.recorded") <- TRUE
  #otherwise dispatch to ggcyto_flowSet version of +
  `+.ggcyto_flowSet`(e1,e2)
  
}
#' @export
setMethod("+", c("ggcyto_GatingSet"), `+.ggcyto_GatingSet`)
#' match the subpopulation based on the given projections and parentID
#' @noRd 
.getChildren_by_projection <- function(gs, parentID, x, y){
  cids <- gs_pop_get_children(gs[[1]], parentID, showHidden = FALSE, path = "auto")
  if(length(cids)>0)
  {
    #try to match to projections
    
    isMatched<-lapply(cids,function(cid){
      g<-gh_pop_get_gate(gs[[1]],cid)
      if(class(g)!="booleanFilter") 
      {
        prj<-parameters(g)
        if(length(prj)==1)#1d gate
        {
          return (prj%in%c(x,y))
          
        }else
        {
          #2d gate but y is absent
          if(is.null(y))
            return (FALSE)
          #try to match x,y to 2d gate
          revPrj<-rev(prj)
          if((x==prj[1]&&y==prj[2])||(x==revPrj[1]&&y==revPrj[2]))
            return (TRUE)
          else
            return (FALSE)	
        }
      }else
        return (FALSE)
    })
    
    ind<-which(unlist(isMatched))
    cids[ind]
  }
}

#TODO: more efficient cloning the existing ggproto object
ggproto.copy <- function(x){
  y <- serialize(x, NULL)
  unserialize(y)
}
