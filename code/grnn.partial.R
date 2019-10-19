grnn.partial <- function(net, i, plot = T) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  
  xname <- colnames(net$x)[i]
  xi <- sort(unique(net$x[, i]))
  partial <- function(x_i) {
    x <- net$x
    x[, i] <-  rep(x_i, length(net$y))
    return(data.frame(x = x_i, p = mean(grnn.predict(net, x))))
  }
  cls <- parallel::makeCluster(min(length(xi), parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("net", "grnn.fit", "grnn.predone", "grnn.predict")
  parallel::clusterExport(cls, obj,  envir = environment())
  rst <- Reduce(rbind, parallel::parLapply(cls, xi, partial))
  parallel::stopCluster(cls)
  if (plot == T) {
    plot(rst[, 1], rst[, 2], type = "b", lty = 4, lwd = 3, ylab = '', xlab = xname, 
         main = "Partial Dependence", pch = 16, cex = 1.5, col = "royalblue", cex.main = 1, cex.lab = 1, yaxt = 'n')

  } else {
    return(rst)  	
  }
}
