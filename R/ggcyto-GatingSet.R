
#' Create a new ggcyto plot from a flowSet
#'
#' @param data GatingSet to plot
#' @param subset character that specifies the node path or node name in the GatingSet. 
#'                Default is "_parent_", which will be substitute with the actual node name 
#'                based on the geom_gate layer to be added later.
#' @inheritParams ggcyto.flowSet
#' @method ggcyto GatingSet
#' @export
#' @importFrom ggplot2 theme element_text labs
#' @importFrom flowWorkspace GatingSet GatingHierarchy
ggcyto.GatingSet <- function(data, mapping, subset = "_parent_", ...){
  
  attr(data, "subset") <- subset#must attach parent info to attribute since foritfy method needs it to coerce it to data.frame
#   browser()
  p <- ggcyto.flowSet(data = data, mapping = mapping, ...)
  p <- p + labs(title  = subset)
  
  # prepend the ggcyto class attribute
  p <- as(p, "ggcyto_GatingSet")  
  
  p <- p + theme(plot.title = element_text(face="bold"))
  
  p
}


#' @rdname ggcyto.GatingSet
#' @export
ggcyto.GatingHierarchy <- function(data, ...){
  data <- as.GatingSet(data)
  ggcyto(data, ...)  
}
#' @importFrom flowWorkspace sampleNames
as.GatingSet <- function(gh){
  as(gh, "GatingSet")[sampleNames(gh)]
}


#' overloaded '+' method for ggcyto.gs
#' 
#' It tries to copy pData from ggcyto object to the gate layers
#' so that the gate layer does not need to have `pd` to be supplied explicitly by users.
#' 
#' @param e1 An object of class \code{ggcyto}
#' @param e2 A component to add to \code{e1}
#' 
#' @rdname ggcyto_GatingSet_add
#' @export
#' @importFrom proto is.proto proto
#' @importFrom flowWorkspace getParent getGate getData getTotal getProp prettyAxis
`+.ggcyto_GatingSet` <- function(e1, e2){
  plot_mapping <- e1$mapping
  prj <- sapply(plot_mapping, as.character)
  gs <- e1$data
  if(is.proto(e2)){
    
    parent <- attr(gs, "subset")
    if(e2$geom$objname=="gs.node"){
      #instantiate e2 layer as a specific gate layer
#       browser()      
      nodes <- e2$stat_params[["node"]]  
      #instantiate the parent by the first node if it is not yet been done
      if(isTRUE(nodes == "_child_")){
        if(parent == "_parent_")
          stop("either 'subset' in ggcyto object or 'data' in geom_gate layer needs to be specified!")
        
        nodes <- .getChildren_by_projection(gs, parent, x = prj[1], y = prj[2])
      }
      #instantiate the subset/parent info
      if(parent == "_parent_"){
        parent <- getParent(gs[[1]], nodes[[1]])
        attr(e1$data, "subset") <- parent
      }
#       browser()
      if(e1$labels[["title"]] == "_parent_")
        e1$labels[["title"]] <- parent
      
      for(node in nodes)
      {
        gate <- getGate(gs, node)
        #can't clone directly from e2 since geom_gate is a speical layer 
        #that is different from 'GeomGsNode' and needs to be constructed explicitly 
        e2.new <- geom_gate(gate) 
        # copy all the other parameters
        e2.new$geom_params <- defaults(e2.new$geom_params, e2$geom_params)
        e2.new$stat_params <- defaults(e2.new$stat_params, e2$stat_params)
        e1 <- `+.ggcyto_flowSet`(e1, e2.new)
      }
      
      #store nodes so that geom_stats layers can use it
      e1[["nodes"]] <- c(e1[["nodes"]], nodes)
     return (e1) 
      
    }else if(e2$geom$objname == "popStats"){
      # cal range here
      if(is.null(e2$stat_params[["data_range"]])){
        fs <- getData(gs, parent)
        e2$stat_params[["data_range"]] <- range(fs[[1, use.exprs = F]])  
      }
      #grab the nodes info from previous gate layers
      nodes.geom_gate <- e1[["nodes"]]
      if(is.null(nodes.geom_gate))
        stop("geom_gate must be added before adding geom_stats!")
      gates <- e2$stat_params[["gate"]]      
      #when gate argument is absent from the stats layer, substitute it with nodes from gates layers
      if(is.null(gates))
        gates <- nodes.geom_gate
      #if it is character then use it as node names
      if(is.character(gates)){
        #update the gate argument with the actual gates
        for(node in gates){
          gates <- getGate(gs, node)
          #clone e2
          
          e2.new <- proto(e2)#as.proto(as.list(e2), parent = GeomStats)
          e2.new$stat_params[["gate"]] <- gates
          stat_type <- e2.new$stat_params[["type"]]
          #grab the pre-calculated stats
          if(is.null(e2$stat_params[["value"]])){
            if(stat_type == "count")
              e2.new$stat_params[["value"]] <- lapply(gs, getTotal, y = node)
            else if(stat_type == "percent")
              e2.new$stat_params[["value"]] <- lapply(gs, getProp, y = node)
          }
#           browser()
         e1 <- `+.ggcyto_flowSet`(e1, e2.new) 
        }
        return(e1)
      }
      
    }
    
  }else if(inherits(e2, "raw_scale")){
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
  
  callNextMethod()
  
}
#' @export
#' @rdname ggcyto_GatingSet_add
setMethod("+", c("ggcyto_GatingSet"), `+.ggcyto_GatingSet`)
#' match the subpopulation based on the given projections and parentID
#' @importFrom flowWorkspace getChildren
.getChildren_by_projection <- function(gs, parentID, x, y){
  cids <- getChildren(gs[[1]], parentID, showHidden = FALSE, path = "auto")
  if(length(cids)>0)
  {
    #try to match to projections
    #  	browser()
    isMatched<-lapply(cids,function(cid){
      g<-getGate(gs[[1]],cid)
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
