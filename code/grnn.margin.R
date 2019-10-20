grnn.margin <- function(net, i, plot = T) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  xname <- colnames(net$x)[i]
  n <- length(unique(net$x[, i]))
  x <- matrix(rep(colMeans(net$x), n), nrow = n, byrow = T)
  x[, i] <- sort(unique(net$x[, i]))
  rst <- data.frame(x = x[, i], p = grnn.parpred(net, x))
  if (plot == T) {
    plot(rst[, 1], rst[, 2], type = "b", lty = 4, lwd = 3, ylab = '', xlab = xname, 
         main = "Marginal Effect", pch = 16, cex = 1.5, col = "red", cex.main = 1, cex.lab = 1, yaxt = 'n')
  } else {
    return(rst)
  }
}	
