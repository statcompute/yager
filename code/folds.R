#' Create a list of N-fold index
#' 
#' @param idx  A vector of index
#' @param n    The value of N folds
#' @param seed The seed value with the default = 1
#' @return A list of N-fold index
#' @examples
#' folds(1:10, 3)
#' 
folds <- function(idx, n, seed = 1) {
  g <- with(set.seed(seed), sample(idx, length(idx))) %% n + 1
  r <- split(idx, g)
  names(r) <- paste('Fold', seq(n), sep = '')
  return(r)
}
