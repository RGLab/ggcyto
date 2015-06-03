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
    setkeyv(res, .id)
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
    setkeyv(res, .id)
  res
}


