#' Convert a list to a data.table
#' 
#' data.table version of ldply
#' @param .data a list
#' @return a data.table
.ldply <- function (.data, .fun = NULL, ..., .id = NA) 
{
  
  if(is.null(names(.data)))
    idcol <- NULL
  else{
    if(is.na(.id))
      idcol <- ".id"
    else
      idcol <- .id
  }

  
  res <- sapply(.data, .fun, ..., simplify = FALSE)
  
  res <- rbindlist(res, idcol = idcol)
  if(!is.null(idcol))
    setkeyv(res, idcol)
  res
}

#' convert a flowSet to a data.table
#' @param .data flowSet
.fsdply <- function (.data, .fun = NULL, ..., .id = NA) 
{
  
  if(is.na(.id))
    idcol <- ".id"
  else
    idcol <- .id

  res <- fsApply(.data, .fun, ..., simplify = FALSE)
  
  res <- rbindlist(res, idcol = idcol)
  if(!is.null(idcol))
    setkeyv(res, idcol)
  res
}

#' Generate a marginal gate.
#' 
#' It constructs an expression filter that removes the marginal events.
#' 
#' @param fs flowSet
#' @param dims the channels involved
#' @param tol the tolerance 
#' @param ... not used
#' @return  an expressionFilter
#' @export
marginalFilter <- function(fs, dims, tol = 1e-5, ...){
  r <- range(fs[[1, use.exprs = FALSE]], dims)
  
#   browser()
  exp <- NULL
  for(dim in dims){
    thisRange <- r[, dim]
    eps <- diff(thisRange) * tol
    thisExp <- substitute(dim > dim.min & dim < dim.max
               , list(dim = as.symbol(dim)
                      , dim.min = thisRange[1] + eps
                      , dim.max = thisRange[2] - eps
                      )
                )
    thisExp <- deparse(thisExp, width =  500) # it will fail if width >500
    exp <- paste(exp, thisExp, sep = ifelse(is.null(exp), "", "&"))
  }
  
  char2ExpressionFilter(exp)
}