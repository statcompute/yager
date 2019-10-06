grnn.optim_auc <- function(net, lower = 0, upper, nfolds = 4, seed = 1, method = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  set.seed(seed)
  fd <- caret::createFolds(seq(nrow(net$x)), k = nfolds)

  cv_auc <- function(s) {
    cls <- parallel::makeCluster(min(nfolds, parallel::detectCores() - 1), type = "PSOCK")
    obj <- c("fd", "net", "grnn.fit", "grnn.predone", "grnn.predict")
    parallel::clusterExport(cls, obj,  envir = environment())
    rs <- Reduce(rbind,
            parallel::parLapply(cls, fd,
              function(f) data.frame(ya = net$y[unlist(f)],
                                     yp = grnn.predict(grnn.fit(net$x[unlist(-f), ], net$y[unlist(-f)],  sigma = s),
                                                       net$x[unlist(f), ]))))
    parallel::stopCluster(cls)
    return(MLmetrics::AUC(y_pred = rs$yp, y_true = rs$ya))
  }

  if (method == 1) {
    rst <- optimize(f = cv_auc, interval = c(lower, upper), maximum = T)
  } else if (method == 2) {
    rst <- optim(par = mean(lower, upper), fn = cv_auc, lower = lower, upper = upper, 
                 method = "Brent", control = list(fnscale = -1))
  }  
  return(list(sigma = rst[[1]], auc = rst[[2]]))
}
