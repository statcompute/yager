r2 <- function(act, pre) { 
  rss <- sum((pre - act) ^ 2)
  tss <- sum((act - mean(act)) ^ 2)
  return(1 - rss / tss)
}
