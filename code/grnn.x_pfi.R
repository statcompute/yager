grnn.x_imp <- function(net, i) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (i > ncol(net$x)) stop("the selected variable is out of bound.", call. = F)

  xname <- colnames(net$x)[i]
  x <- net$x
  x[, i] <-  rep(mean(net$x[, i]), length(net$y))
  auc0 <- MLmetrics::AUC(y_pred = grnn.predict(net, net$x), y_true = net$y)
  auc1 <- MLmetrics::AUC(y_pred = grnn.predict(net, x), y_true = net$y)
  auc2 <- MLmetrics::AUC(y_pred = grnn.predict(grnn.fit(x = x[, -i], y = net$y, sigma = net$sigma), x[, -i]), y_true = net$y)
  return(data.frame(var = xname, imp1 = round(max(0, 1 - auc1 / auc0), 8), imp2 = round(max(0, 1 - auc2 / auc0), 8)))
}
