gen_sobol <- function(min, max, n, seed) {
  return(round(min + (max - min) * randtoolbox::sobol(n, dim = 1, scrambling = 1, seed = seed), 4))
}

gen_unifm <- function(min, max, n, seed) {
  set.seed(seed)
  return(round(min + (max - min) * runif(n), 4))
}

gen_haltn <- function(min, max, n) {
  return(round(min + (max - min) * randtoolbox::halton(n, dim = 1, usetime = T)))
}

gen_torus <- function(min, max, n) {
  return(round(min + (max - min) * randtoolbox::torus(n, dim = 1, usetime = T)))
}
