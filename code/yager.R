###############################################################################
# PACKAGE NAME: Yet Another General Regression (YAG[e]R) Neural Network
# AUTHOR      : WENSUI LIU
# DISCLAIMER  : THIS IS MY WEEKEND PROJECT AND NOT RELATED TO MY CURRENT WORK WITH MY EMPLOYER
#               IT IS FREE (AS FREE BEER) TO USE AND DISTRIBUTE
###############################################################################

grnn.fit <- function(x, y, w = rep(1, length(y)), sigma = 1) {
  ### CHECK X MATRIX ###
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  ### CHECK Y VECTOR ###
  if (is.vector(y) == F) stop("y needs to be a vector.", call. = F)
  if (anyNA(y) == T) stop("NA found in y.", call. = F)
  if (length(y) != nrow(x)) stop("x and y need to share the same length.", call. = F)
  ### CHECK W VECTOR ###
  if (is.vector(w) == F) stop("w needs to be a vector.", call. = F)
  if (anyNA(w) == T) stop("NA found in w.", call. = F)
  if (length(w) != nrow(x)) stop("x and w need to share the same length.", call. = F)
  ### CHECK SIGMA ###
  if (sigma <= 0) stop("sigma needs to be positive", call. = F)

  gn <- structure(list(), class = "General Regression Neural Net")
  gn$x <- x
  gn$y <- y
  gn$w <- w
  gn$sigma <- sigma
  return(gn)
}

grnn.predone<- function(net, x, type = 1) {
  ### CHECK INPUT X VECTOR ###
  if (is.vector(x) == F) stop("x needs to be a vector.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (length(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)
  ### CHECK INPUT TYPE (CURRENTLY SUPPORTING 1 / 2) ###
  if (!(type %in% c(1, 2))) stop("the type is not supported.", call. = F)

  if (type == 1) {
  ### EUCLIDEAN DISTANCE BY DEFAULT ###
    num <- sum(net$w * net$y * exp(-Reduce(c, lapply(split(net$x, seq(nrow(net$x))), function(xi) sum((x - xi) ^ 2))) / (2 * (net$sigma ^ 2))))
    den <- sum(net$w * exp(-Reduce(c, lapply(split(net$x, seq(nrow(net$x))), function(xi) sum((x - xi) ^ 2))) / (2 * (net$sigma ^ 2))))
  } else if (type == 2) {
  ### MANHATTAN DISTANCE ###
    num <- sum(net$w * net$y * exp(-Reduce(c, lapply(split(net$x, seq(nrow(net$x))), function(xi) sum(abs(x - xi)))) / net$sigma))
    den <- sum(net$w * exp(-Reduce(c, lapply(split(net$x, seq(nrow(net$x))), function(xi) sum(abs(x - xi)))) / net$sigma ))
  }
  return(num / den)
}

grnn.predict <- function(net, x) {
  ### CHECK INPUT X MATRIX ###
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (ncol(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)

  return(Reduce(c, lapply(split(x, seq(nrow(x))), function(x_) grnn.predone(net, x_))))
}

grnn.parpred <- function(net, x) {
  ### CHECK INPUT X MATRIX ###
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (ncol(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)

  cls <- parallel::makeCluster(min(floor(nrow(x) / 3), parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("net", "x", "grnn.predone", "grnn.predict")
  parallel::clusterExport(cls, obj, envir = environment())
  spx <- parallel::parLapplyLB(cls, parallel::clusterSplit(cls, seq(nrow(x))),
                               function(c_) x[c_, ])
  rst <- parallel::parLapplyLB(cls, spx, function(x_) grnn.predict(net, x_))
  parallel::stopCluster(cls)
  return(Reduce(c, rst))
}

grnn.search_rsq <- function(net, sigmas, nfolds = 3, seed = 1) {
  set.seed(seed)
  fd <- caret::createFolds(seq(nrow(net$x)), k = nfolds)

  cv <- function(s) {
    rs <- Reduce(rbind,
                 lapply(fd,
                        function(f) data.frame(ya = net$y[unlist(f)],
                                               yp = grnn.predict(grnn.fit(net$x[unlist(-f), ], net$y[unlist(-f)],  sigma = s),
                                                                 net$x[unlist(f), ]))))
    return(data.frame(sigma = s, r2 = MLmetrics::R2_Score(y_pred = rs$yp, y_true = rs$ya)))
  }
  cls <- parallel::makeCluster(min(nfolds, parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("fd", "net", "grnn.fit", "grnn.predone", "grnn.predict")
  parallel::clusterExport(cls, obj,  envir = environment())
  rst <- Reduce(rbind, parallel::parLapply(cls, sigmas, cv))
  parallel::stopCluster(cls)
  return(list(test = rst, best = rst[rst$r2 == max(rst$r2), ]))
}

grnn.search_auc <- function(net, sigmas, nfolds = 3, seed = 1) {
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

grnn.optmiz_auc <- function(net, lower = 0, upper, nfolds = 3, seed = 1, method = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  set.seed(seed)
  fd <- caret::createFolds(seq(nrow(net$x)), k = nfolds)

  cv <- function(s) {
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
    rst <- optimize(f = cv, interval = c(lower, upper), maximum = T)
  } else if (method == 2) {
    rst <- optim(par = mean(lower, upper), fn = cv, lower = lower, upper = upper,
                 method = "Brent", control = list(fnscale = -1))
  }
  return(data.frame(sigma = rst[[1]], auc = rst[[2]]))
}                       
                        
grnn.margin <- function(net, i) {
  n <- length(unique(net$x[, i]))
  x <- matrix(rep(colMeans(net$x), n), nrow = n, byrow = T)
  x[, i] <- unique(net$x[, i])
  x.pred <- grnn.parpred(net, x)
  xname <- colnames(net$x)[i]
  plot(sort(x[, i]), x.pred[order(x[, i])], type = "b", lty = 4, lwd = 3, ylab = '', xlab = xname, 
       main = "Marginal Effect", pch = 16, cex = 1.5, col = "red", cex.main = 1, cex.lab = 1, yaxt = 'n')
}	                        

gen_unifm <- function(min = 0, max = 1, n, seed = 1) {
  set.seed(seed)
  return(round(min + (max - min) * runif(n), 8))
}

gen_sobol <- function(min = 0, max = 1, n, seed = 1) {
  return(round(min + (max - min) * randtoolbox::sobol(n, dim = 1, scrambling = 3, seed = seed), 8))
}

gen_latin <- function(min = 0, max = 1, n, seed = 1) {
  set.seed(seed)
  return(round(min + (max - min) * c(lhs::randomLHS(n, k = 1)), 8))
}
