grnn.cv <- function(nn, sigmas, nfolds, seed) {
  dt <- nn$set
  set.seed(seed)
  folds <- caret::createFolds(1:nrow(dt), k = nfolds, list = FALSE)
  cv <- function(s) {
    r <- do.call(rbind,
                 lapply(1:nfolds,
                        function(i) data.frame(Ya = nn$Ya[folds == i],
                                               Yp = grnn.predict(grnn.fit(nn$Xa[folds != i, ], nn$Ya[folds != i], s),
                                                                 data.frame(nn$Xa[folds == i,])))))
    return(data.frame(sigma = s, R2 = r2(r$Ya, r$Yp)))
  }
  r2_lst <- Reduce(rbind, Map(cv, sigmas))
  return(r2_lst[r2_lst$R2 == max(r2_lst$R2), ])
}
