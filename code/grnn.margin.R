grnn.margin <- function(net, n) {
  x <- matrix(0, nrow = length(unique(net$x[, n])), ncol = dim(net$x)[2])
  x[, n] <- unique(net$x[, n])
  x.pred <- grnn.parpred(net, x)
  xname <- colnames(net$x)[n]
  plot(sort(x[, n]), x.pred[order(x[, n])], type = "b", lty = 4, lwd = 3, ylab = '', xlab = xname, 
       main = "Marginal Effect", pch = 16, cex = 1.5, col = "red", cex.main = 1, cex.lab = 1, yaxt = 'n')
}	
