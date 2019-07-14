grnn.predict <- function(net, x) {
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (ncol(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)

  return(Reduce(c, lapply(split(x, seq(nrow(x))), function(x_) grnn.predone(net, x_))))
}
