grnn.predict <- function(nn, x) {
  c <- parallel::detectCores() - 1
  return(do.call(rbind, 
                 parallel::mcMap(function(i) grnn::guess(nn, as.matrix(x[i, ])),
                                 1:nrow(x), mc.cores = c))[,1])
}
