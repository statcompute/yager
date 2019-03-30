grnn.fit <- function(x, y, sigma) {
  return(grnn::smooth(grnn::learn(data.frame(y, x)), sigma))
}
