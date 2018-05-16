#' transform methods for gates
#' @param _data the filter or filterList object. Currently support polygonGate, ellipsoidGate, rectangleGate and quadGate.
#' @param ...
#'      trans the transformation function or transformList object
#'      param the parameter/dimension to be transformed. When trans is transformList object, param is not needed since it is derived from transformList.
#' @return the transformed filter/filterList object
#' @export
#' @rdname transform-gate
setMethod("transform", signature = c("filter"), function(`_data`, ...){
  .transform.filter(`_data`, ...)
})

#' @export
#' @rdname transform-gate
setMethod("transform", signature = c("filterList"), function(`_data`, ...){
  res <- callNextMethod()
  filterList(res)
})

#' @export
#' @rdname transform-gate
setMethod("transform", signature = c("list"), function(`_data`, ...){
  res <- lapply(`_data`, function(g){
    transform(g, ...)
  })
  res
})

.transform.filter <- function(`_data`, trans, ...){
  if(is(trans, "transformList"))
  {
    dims <- parameters(`_data`)
    for(p in names(trans@transforms))
    {
      if(p %in% dims)
        `_data` <- transform_gate(`_data`, trans@transforms[[p]]@f, p)
    }
    `_data`
  }else if(is(trans, "function"))
    transform_gate(`_data`, trans, ...)
  else
    stop("unsupported `trans` type!")
}

#' transform methods for gates
#' @param _data the gate object. Currently support polygonGate, ellipsoidGate, rectangleGate and quadGate.
#' @param ...
#' @return the transformed gate object
#' @export
#' @rdname transform-gate
transform_gate <- function(`_data`, ...)UseMethod("transform_gate")

#' @param gate gate object
#' @param trans the transformation function
#' @param param the parameter/dimension to be transformed. 
#' @export
#' @rdname transform-gate
transform_gate.polygonGate <- function(gate, trans, param){
  gate@boundaries[, param] <- trans(gate@boundaries[, param])
  gate
}


#' @export
#' @rdname transform-gate
transform_gate.ellipsoidGate <- function(gate, ...){
  transform_gate(as(gate, "polygonGate"), ...)
}
# somehow ellips shape is not well perseved after transforming the two antipods and mean
transform_gate_old_ellipsoidGate <- function(gate, trans, param){
  #convert cov format to antipotal format since cov can not be transformed independently on each param
  #it is based on 5.3.1 of gatingML2 doc
  mu <- gate@mean
  CC <- gate@cov
  dims <- colnames(CC)
  x <- dims[1]
  y <- dims[2]
  D <- gate@distance

#   term <- sqrt((CC[x, x] - CC[y, y]) ^ 2 + 4 * CC[x, y] ^ 2)
#   lambda <- ((CC[x, x] + CC[y, y]) + c(term, -term)) / 2
#
#   if(CC[x,y] == 0){
#     X1 <- c(1, 0)
#     X2 <- c(0, 1)
#   }else{
#     X1 <- c(lambda[1] - CC[y, y], CC[x, y])
#     X2 <- c(lambda[2] - CC[y, y], CC[x, y])
#   }
  #compute eigen value (for a, b) and eigen vector (for angle)
  res <- eigen(CC)
  lambda <- res[["values"]]
  X1 <- res[["vectors"]][,1]
  if(X1[1] == 0){
    theta <- pi/2
  }else{
    theta <- atan(X1[2]/X1[1])
  }

  a <- sqrt(lambda[1] * D ^ 2)
  b <- sqrt(lambda[2] * D ^ 2)

  #get coordinates of centred antipodal points
  antipod1 <- c(a * cos(theta), a * sin(theta))
  antipod2 <- c(b * sin(theta), - b * cos(theta))
  # browser()
  #shift to mu
  antipod1 <- antipod1 + mu
  antipod2 <- antipod2 + mu
  names(antipod1) <- dims
  names(antipod2) <- dims
  #transform the respective dim of antipods
  antipod1[param] <- trans(antipod1[param])
  antipod2[param] <- trans(antipod2[param])

  # transform to get new mu
  mu[param] <- trans(mu[param])
  #shift to new center
  antipod1 <- antipod1 - mu
  antipod2 <- antipod2 - mu
  #compute the new a, b
  a <- sqrt(sum(antipod1 ^ 2))
  b <- sqrt(sum(antipod2 ^ 2))
  #convert it back to the inverse covaiance mat

  CC.inv <- CC
  CC.inv[x, x] <- cos(theta) ^ 2 / a ^ 2 + sin(theta) ^ 2 / b ^ 2
  CC.inv[y, y] <- sin(theta) ^ 2 / a ^ 2 + cos(theta) ^ 2 / b ^ 2
  CC.inv[x, y] <- CC.inv[y, x] <- sin(theta) * cos(theta) * (1/a^2 - 1/b^2)
  CC <- solve(CC.inv)


  gate1 <- gate
  gate1@cov <- CC
  gate1@mean <- mu
  # browser()
  gate1

}

#' @export
#' @rdname transform-gate
transform_gate.rectangleGate <- function(gate, trans, param){

  min <- gate@min[[param]]
  if(!is.infinite(min))
    gate@min[[param]] <- trans(min)

  max <- gate@max[[param]]
  if(!is.infinite(max))
    gate@max[[param]] <- trans(max)

  gate

}

#' @export
#' @rdname transform-gate
transform_gate.quadGate <- function(gate, trans, param){

  boundary <- gate@boundary[[param]]
  if(!is.infinite(boundary))
    gate@boundary[[param]] <- trans(boundary)

  gate

}
