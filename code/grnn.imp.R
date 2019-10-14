grnn.imp <- function(net) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)

  cls <- parallel::makeCluster(min(ncol(net$x), parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("net", "grnn.fit", "grnn.predone", "grnn.predict", "grnn.x_imp")
  parallel::clusterExport(cls, obj,  envir = environment())
  rst1 <- data.frame(idx = seq(ncol(net$x)), 
  	                 Reduce(rbind, parallel::parLapply(cls, seq(ncol(net$x)), function(i) grnn.x_imp(net, i))))
  parallel::stopCluster(cls)
  rst2 <- rst1[with(rst1, order(-imp1, -imp2)), ]
  row.names(rst2) <- NULL
  return(rst2)
}
