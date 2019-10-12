grnn.search_auc <- function(net, sigmas, nfolds = 4, seed = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  set.seed(seed)
  fd <- caret::createFolds(seq(nrow(net$x)), k = nfolds)

  cv <- function(s) {
    rs <- Reduce(rbind,
                 lapply(fd,
                        function(f) data.frame(ya = net$y[unlist(f)],
                                               yp = grnn.predict(grnn.fit(net$x[unlist(-f), ], net$y[unlist(-f)],  sigma = s),
                                                                 net$x[unlist(f), ]))))
    return(data.frame(sigma = s, auc = MLmetrics::AUC(y_pred = rs$yp, y_true = rs$ya)))
  }
  cls <- parallel::makeCluster(min(nfolds, parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("fd", "net", "grnn.fit", "grnn.predone", "grnn.predict")
  parallel::clusterExport(cls, obj,  envir = environment())
  rst <- Reduce(rbind, parallel::parLapply(cls, sigmas, cv))
  parallel::stopCluster(cls)
  return(list(test = rst, best = rst[rst$auc == max(rst$auc), ]))
}
