grnn.predone<- function(net, x, type = 1) {
  ### CHECK INPUT X VECTOR ###
  if (is.vector(x) == F) stop("x needs to be a vector.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (length(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)
  ### CHECK INPUT TYPE (CURRENTLY SUPPORTING 1 / 2) ###
  if (!(type %in% c(1, 2))) stop("the type is not supported.", call. = F)

  if (type == 1) {
  ### EUCLIDEAN DISTANCE BY DEFAULT ###
    num <- sum(net$w * net$y * exp(-Reduce(c, lapply(split(net$x, seq(nrow(net$x))), function(xi) sum((x - xi) ^ 2))) / (2 * (net$sigma ^ 2))))
    den <- sum(net$w * exp(-Reduce(c, lapply(split(net$x, seq(nrow(net$x))), function(xi) sum((x - xi) ^ 2))) / (2 * (net$sigma ^ 2))))
  } else if (type == 2) {
  ### MANHATTAN DISTANCE ###
    num <- sum(net$w * net$y * exp(-Reduce(c, lapply(split(net$x, seq(nrow(net$x))), function(xi) sum(abs(x - xi)))) / net$sigma))
    den <- sum(net$w * exp(-Reduce(c, lapply(split(net$x, seq(nrow(net$x))), function(xi) sum(abs(x - xi)))) / net$sigma ))
  }
  return(num / den)
}
