initialize_unif = function(n, dims, range) {
  matrix(replicate(n, runif(dims, range[1], range[2])), ncol=dims, byrow = TRUE)
}

select_best = function(p, N) {
  rep(which.best(p[[2]]), N)
}

select_rand = function(p, N) {
  sample(nrow(p[[1]]), N, TRUE)
}

crossover_bin = function(x, y, cr) {
  mod = matrix(runif(ncol(x)*nrow(x)) > cr, nrow=nrow(x))
  mod*x + (1-mod)*y
}

crossover_exp = function(x, y, cr) {
  mod = t(replicate(nrow(x), cummin(runif(ncol(x)) < cr)))
  mod*x + (1-mod)*y
}

crossover_exp_perm = function(x, y, cr) {
  mod = t(replicate(nrow(x), cummin(runif(ncol(x)) < cr)))
  mod = sample(mod)
  mod*x + (1-mod)*y
}

range_fit_truncate = function(p, range) {
  pmin(pmax(p, range[1]), range[2])
}

range_fit_reinitialize = function(p, range) {
  range_l = range[2] - range[1]
  mod = apply(p, 1, function(r) { sum(p < range[1] || p > range[2]) > 0 })
  reinit = initialize_unif(nrow(p), ncol(p), range)
  reinit*mod + p*(1-mod)
}

range_fit_mirror = function(p, range) {
  range_l = range[2] - range[1]
  mod_min = p > range[1]
  mirrored_min = range[1] + ((range[1] - p)%%range_l)
  p = mod_min*p + (1 - mod_min)*mirrored_min
  mod_max = p < range[2]
  mirrored_max = range[2] - ((p - range[2])%%range_l)
  p = mod_max*p + (1 - mod_max)*mirrored_max
  return(p);
}

range_fit_roll = function(p, range) {
  range_l = range[2] - range[1]
  mod_min = p > range[1]
  rolled_min = range[2] - ((range[1] - p)%%range_l)
  p = mod_min*p + (1 - mod_min)*rolled_min
  mod_max = p < range[2]
  rolled_max = range[1] + ((p - range[2])%%range_l)
  p = mod_max*p + (1 - mod_max)*rolled_max
}

diff_vector = function(pop, N, forbidden) {
  pairs = replicate(N, sample(nrow(pop) - 1, 2, FALSE))
  forbidden = matrix(rep(forbidden, 2), nrow=2, byrow=T)
  pairs = pairs + (pairs >= forbidden)
  x1 = pop[pairs[1,],]
  x2 = pop[pairs[2,],]
  x1 - x2
}
