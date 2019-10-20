grnn.pfi <- function(net, ntry = 100, seed = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)

  cls <- parallel::makeCluster(min(ncol(net$x), parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("net", "grnn.fit", "grnn.predone", "grnn.predict", "grnn.x_pfi", "ntry", "seed")
  parallel::clusterExport(cls, obj,  envir = environment())
  rst1 <- data.frame(idx = seq(ncol(net$x)), 
                     Reduce(rbind, parallel::parLapply(cls, seq(ncol(net$x)), function(i) grnn.x_pfi(net, i, ntry = ntry, seed = seed))))
  parallel::stopCluster(cls)
  rst2 <- rst1[with(rst1, order(-pfi)), ]
  row.names(rst2) <- NULL
  return(rst2)
}
