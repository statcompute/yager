grnn.x_pfi <- function(net, i, ntry = 1e3, seed = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)

  xname <- colnames(net$x)[i]
  set.seed(seed)
  seeds <- round(runif(ntry) * 1e8, 0)
  ol <- lapply(seeds, function(s) with(set.seed(s), sample(seq(nrow(net$x)), nrow(net$x), replace = F)))
  cl <- Reduce(c, lapply(ol, function(o) abs(cor(seq(nrow(net$x)), o))))
  x <- net$x
  x[, i] <-  net$x[ol[[which(cl == min(cl))]], i]
  auc0 <- MLmetrics::AUC(y_pred = grnn.predict(net, net$x), y_true = net$y)
  auc1 <- MLmetrics::AUC(y_pred = grnn.predict(net, x), y_true = net$y)
  return(data.frame(var = xname, pfi = round(max(0, 1 - auc1 / auc0), 8)))
}
