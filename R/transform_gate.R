
setMethod("transform", signature = c("polygonGate"), function(`_data`, ...){
  .transform.polygonGate(`_data`, ...)
})
.transform.polygonGate <- function(gate, trans.fun, param){
  gate@boundaries[, param] <- trans.fun(gate@boundaries[, param])
  gate
}

setMethod("transform", signature = c("ellipsoidGate"), function(`_data`, ...){
  # .transform.ellipsoidGate(`_data`, ...)
  transform(as(`_data`, "polygonGate"), ...)
})
# somehow ellips shape is not well perseved after transforming the two antipods and mean
.transform.ellipsoidGate <- function(gate, trans.fun, param){
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
  antipod1[param] <- trans.fun(antipod1[param])
  antipod2[param] <- trans.fun(antipod2[param])

  # transform to get new mu
  mu[param] <- trans.fun(mu[param])
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

setMethod("transform", signature = c("rectangleGate"), function(`_data`, ...){
  .transform.rectangleGate(`_data`, ...)
})
.transform.rectangleGate <- function(gate, trans.fun, param){

  min <- gate@min[[param]]
  if(!is.infinite(min))
    gate@min[[param]] <- trans.fun(min)

  max <- gate@max[[param]]
  if(!is.infinite(max))
    gate@max[[param]] <- trans.fun(max)

  gate

}
setMethod("transform", signature = c("quadGate"), function(`_data`, ...){
  .transform.quadGate(`_data`, ...)
})
.transform.quadGate <- function(gate, trans.fun, param){

  boundary <- gate@boundary[[param]]
  if(!is.infinite(boundary))
    gate@boundary[[param]] <- trans.fun(boundary)

  gate

}
