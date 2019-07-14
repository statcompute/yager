grnn.fit <- function(x, y, sigma = 1) {
  if (is.matrix(x) == F) stop("x needs to be a matrix.", call. = F)
  if (anyNA(x) == T) stop("NA found in x.", call. = F)
  if (is.vector(y) == F) stop("y needs to be a vector.", call. = F)
  if (anyNA(y) == T) stop("NA found in y.", call. = F)
  if (length(y) != nrow(x)) stop("x and y need to share the same length.", call. = F)
  if (sigma <= 0) stop("sigma needs to be positive", call. = F)

  gn <- structure(list(), class = "general regression neural net")
  gn$x <- x
  gn$y <- y
  gn$sigma <- sigma
  return(gn)
}
