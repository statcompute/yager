###########################################################################
# PACKAGE NAME: YAGeR (YET ANOTHER GENERAL REGRESSION NEURAL NETWORK)     #
# AUTHOR      : WENSUI LIU                                                #
# DISCLAIMER  : THIS IS MY WEEKEND PROJECT AND NOT RELATED TO MY          #
#               CURRENT WORK WITH MY EMPLOYER                             #
#               IT IS FREE (AS FREE BEER) TO USE AND DISTRIBUTE           #
###########################################################################  

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

folds <- function(idx, n, seed = 1) {
  g <- with(set.seed(seed), sample(idx, length(idx))) %% n + 1
  r <- split(idx, g)
  names(r) <- paste('Fold', seq(n), sep = '')
  return(r)
}

###########################################################################  

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

###########################################################################

grnn.predone <- function(net, x, type = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  ### CHECK INPUT X VECTOR ###
  if (is.vector(x) == F) stop("x needs to be a vector.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (length(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)
  ### CHECK INPUT TYPE (CURRENTLY SUPPORTING 1 / 2) ###
  if (!(type %in% c(1, 2))) stop("the type is not supported.", call. = F)

  xl <- split(net$x, seq(nrow(net$x)))
  if (type == 1) {
  ### EUCLIDEAN DISTANCE BY DEFAULT ###
    num <- sum(net$w * net$y * exp(-Reduce(c, lapply(xl, function(xi) sum((x - xi) ^ 2))) / (2 * (net$sigma ^ 2))))
    den <- sum(net$w * exp(-Reduce(c, lapply(xl, function(xi) sum((x - xi) ^ 2))) / (2 * (net$sigma ^ 2))))
  } else if (type == 2) {
  ### MANHATTAN DISTANCE ###
    num <- sum(net$w * net$y * exp(-Reduce(c, lapply(xl, function(xi) sum(abs(x - xi)))) / net$sigma))
    den <- sum(net$w * exp(-Reduce(c, lapply(xl, function(xi) sum(abs(x - xi)))) / net$sigma))
  }
  return(num / den)
}

###########################################################################  

grnn.predict <- function(net, x) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (ncol(x) != ncol(net$x)) stop("x dimension is not consistent with grnn.", call. = F)
  return(Reduce(c, lapply(split(x, seq(nrow(x))), function(x_) grnn.predone(net, x_))))
}

###########################################################################  

grnn.parpred <- function(net, x) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
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

###########################################################################  

grnn.search_rsq <- function(net, sigmas, nfolds = 4, seed = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (is.vector(sigmas) != T) stop("sigmas needs to be a vector.", call. = F)

  fd <- folds(seq(nrow(net$x)), n = nfolds, seed = seed)

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

###########################################################################  

grnn.search_auc <- function(net, sigmas, nfolds = 4, seed = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (is.vector(sigmas) != T) stop("sigmas needs to be a vector.", call. = F)

  fd <- folds(seq(nrow(net$x)), n = nfolds, seed = seed)

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

###########################################################################  

grnn.optmiz_auc <- function(net, lower = 0, upper, nfolds = 4, seed = 1, method = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)

  fd <- folds(seq(nrow(net$x)), n = nfolds, seed = seed)

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

###########################################################################  

grnn.margin <- function(net, i, plot = T) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (i > ncol(net$x)) stop("the selected variable is out of bound.", call. = F)
  if (!(plot %in% c(T, F))) stop("the plot input is not correct.", call. = F)

  xname <- colnames(net$x)[i]
  n <- length(unique(net$x[, i]))
  x <- matrix(rep(colMeans(net$x), n), nrow = n, byrow = T)
  x[, i] <- sort(unique(net$x[, i]))
  rst <- data.frame(x = x[, i], p = grnn.predict(net, x))
  if (plot == T) {
    plot(rst[, 1], rst[, 2], type = "b", lty = 4, lwd = 3, ylab = '', xlab = xname, 
         main = "Marginal Effect", pch = 16, cex = 1.5, col = "red", cex.main = 1, cex.lab = 1, yaxt = 'n')
    rug(rst[, 1], col = 'green4', ticksize = 0.03, lwd = 3)
  } else {
    return(rst)
  }
}

###########################################################################  

grnn.partial <- function(net, i, plot = T) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (i > ncol(net$x)) stop("the selected variable is out of bound.", call. = F)
  if (!(plot %in% c(T, F))) stop("the plot input is not correct.", call. = F)

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
    rug(rst[, 1], col = 'green4', ticksize = 0.03, lwd = 3)
  } else {
    return(rst) 
  }
}

###########################################################################  

grnn.x_imp <- function(net, i, class = F) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (i > ncol(net$x)) stop("the selected variable is out of bound.", call. = F)
  if (!(class %in% c(T, F))) stop("the class input is not correct.", call. = F)

  xname <- colnames(net$x)[i]
  x <- net$x
  x[, i] <-  rep(mean(net$x[, i]), length(net$y))
  if (class == T) {
    auc0 <- MLmetrics::AUC(grnn.predict(net, net$x), net$y)
    auc1 <- MLmetrics::AUC(grnn.predict(net, x), net$y)
    auc2 <- MLmetrics::AUC(grnn.predict(grnn.fit(x = x[, -i], y = net$y, sigma = net$sigma), x[, -i]), net$y)
    imp1 <- round(max(0, 1 - auc1 / auc0), 8)
    imp2 <- round(max(0, 1 - auc2 / auc0), 8)
  } else {
    rsq0 <- MLmetrics::R2_Score(grnn.predict(net, net$x), net$y)
    rsq1 <- MLmetrics::R2_Score(grnn.predict(net, x), net$y)
    rsq2 <- MLmetrics::R2_Score(grnn.predict(grnn.fit(x = x[, -i], y = net$y, sigma = net$sigma), x[, -i]), net$y)
    imp1 <- round(max(0, 1 - rsq1 / rsq0), 8)
    imp2 <- round(max(0, 1 - rsq2 / rsq0), 8)
  }
  return(data.frame(var = xname, imp1 = imp1, imp2 = imp2))
}

###########################################################################  

grnn.x_pfi <- function(net, i, class = F, ntry = 1e3, seed = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (!(class %in% c(T, F))) stop("the class input is not correct.", call. = F)
  
  xname <- colnames(net$x)[i]
  set.seed(seed)
  seeds <- floor(runif(ntry) * 1e8)  
  ol <- lapply(seeds, function(s) with(set.seed(s), sample(seq(nrow(net$x)), nrow(net$x), replace = F)))
  cl <- Reduce(c, lapply(ol, function(o) abs(cor(seq(nrow(net$x)), o))))
  x <- net$x
  x[, i] <-  net$x[ol[[which(cl == min(cl))]], i]
  if (class == T) {
    auc0 <- MLmetrics::AUC(grnn.predict(net, net$x), net$y)
    auc1 <- MLmetrics::AUC(grnn.predict(net, x), net$y)
    pfi  <- round(max(0, 1 - auc1 / auc0), 8)
  } else {
    rsq0 <- MLmetrics::R2_Score(grnn.predict(net, net$x), net$y)
    rsq1 <- MLmetrics::R2_Score(grnn.predict(net, x), net$y)
    pfi  <- round(max(0, 1 - rsq1 / rsq0), 8)
  }
  return(data.frame(var = xname, pfi = pfi))
}

###########################################################################  

grnn.imp <- function(net, class = F) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (!(class %in% c(T, F))) stop("the class input is not correct.", call. = F)

  cls <- parallel::makeCluster(min(ncol(net$x), parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("net", "class", "grnn.fit", "grnn.predone", "grnn.predict", "grnn.x_imp")
  parallel::clusterExport(cls, obj,  envir = environment())
  rst1 <- data.frame(idx = seq(ncol(net$x)),
                     Reduce(rbind, parallel::parLapply(cls, seq(ncol(net$x)), function(i) grnn.x_imp(net, i, class = class))))
  parallel::stopCluster(cls)
  rst2 <- rst1[with(rst1, order(-imp1, -imp2)), ]
  row.names(rst2) <- NULL
  return(rst2)
}

###########################################################################  

grnn.pfi <- function(net, class = F, ntry = 1e3, seed = 1) {
  if (class(net) != "General Regression Neural Net") stop("net needs to be a GRNN.", call. = F)
  if (!(class %in% c(T, F))) stop("the class input is not correct.", call. = F)

  cls <- parallel::makeCluster(min(ncol(net$x), parallel::detectCores() - 1), type = "PSOCK")
  obj <- c("net", "class", "grnn.fit", "grnn.predone", "grnn.predict", "grnn.x_pfi", "ntry", "seed")
  parallel::clusterExport(cls, obj,  envir = environment())
  rst1 <- data.frame(idx = seq(ncol(net$x)), 
                     Reduce(rbind, parallel::parLapply(cls, seq(ncol(net$x)), 
                                                       function(i) grnn.x_pfi(net, i, class = class, ntry = ntry, seed = seed))))
  parallel::stopCluster(cls)
  rst2 <- rst1[with(rst1, order(-pfi)), ]
  row.names(rst2) <- NULL
  return(rst2)
}
