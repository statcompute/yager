grnn.parpred <- function(net, x) {
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (ncol(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)

  cls <- parallel::makeCluster(min(floor(nrow(x) / 3), parallel::detectCores() - 1), type = "PSOCK")
  parallel::clusterExport(cls, c("net", "grnn.predict", "grnn.predone", "x"), envir = environment())
  spx <- parallel::parLapplyLB(cls, parallel::clusterSplit(cls, seq(nrow(x))),
                              function(c_) x[c_, ])
  rst <- parallel::parLapplyLB(cls, spx, function(x_) grnn.predict(net, x_))
  parallel::stopCluster(cls)
  return(Reduce(c, rst))
}
