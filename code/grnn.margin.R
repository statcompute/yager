grnn.margin <- function(net, i) {
  n <- length(unique(net$x[, i]))
  x <- matrix(rep(colMeans(net$x), n), nrow = n, byrow = T)
  x[, i] <- unique(net$x[, i])
  x.pred <- grnn.parpred(net, x)
  xname <- colnames(net$x)[i]
  plot(sort(x[, i]), x.pred[order(x[, i])], type = "b", lty = 4, lwd = 3, ylab = '', xlab = xname, 
       main = "Marginal Effect", pch = 16, cex = 1.5, col = "red", cex.main = 1, cex.lab = 1, yaxt = 'n')
}	
