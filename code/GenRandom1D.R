gen_unifm <- function(min = 0, max = 1, n, seed) {
  set.seed(seed)
  return(round(min + (max - min) * runif(n), 8))
}

gen_sobol <- function(min = 0, max = 1, n, seed) {
  return(round(min + (max - min) * randtoolbox::sobol(n, dim = 1, scrambling = 3, seed = seed), 8))
}

gen_latin <- function(min = 0, max = 1, n, seed) {
  set.seed(seed)
  return(round(min + (max - min) * c(lhs::randomLHS(n, k = 1)), 8))
}
