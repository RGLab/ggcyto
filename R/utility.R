#' Convert a list to a data.table
#' 
#' data.table version of ldply
#' @param .data a list
#' @return a data.table
.ldply <- function (.data,  ..., .id = NA) 
{
  index <- names(.data)
  if(is.null(index)){
    index <- 1:length(.data)
    .id <- NULL
  }
  

  res <- .do_loop(index = index, .data = .data, ..., .id = .id)
  res <- rbindlist(res)
  setkeyv(res, .id)
  res
}

#' @param index the index of the list, can be either character or numeric
#' @param  .fun the function to apply to each element of .data
#' @param ... other arguments passed to .fun
#' @param .id see help(ldply)
.do_loop <- function(index, .data, .fun = NULL, ..., .id = NA){
#   browser()
  lapply(index, function(i){
    
        dt <- .fun(.data[[i]], ...)
        dt <- as.data.table(dt)
        #append id
        if(!is.null(.id)){
          if (is.na(.id)) {
            .id <- ".id"
          }
          eval(substitute(dt[, newCol := i], list(newCol = .id)))
        }
        dt
      })
}
#' convert a flowSet to a data.table
#' @param .data flowSet
.fsdply <- function (.data, ..., .id = NA) 
{

  index <- sampleNames(.data)
  res <- .do_loop(index = index, .data = .data, ..., .id = .id)
  res <- rbindlist(res)
  setkeyv(res, .id)
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
    thisExp <- deparse(thisExp, width.cutoff = 500) # it will fail if width >500
    exp <- paste(exp, thisExp, sep = ifelse(is.null(exp), "", "&"))
  }
  
  char2ExpressionFilter(exp)
}
