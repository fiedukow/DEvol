library("RUnit")
source("multidim.r")

test.select_best = function() {
  POP = list()
  POP[[1]] = matrix(1:10, ncol=2, byrow=TRUE)
  POP[[2]] = c(1:2,-3:-1)
  R = matrix(c(5,6), ncol=2, nrow=2, byrow=TRUE)
  checkEquals(select_best(POP, 2), R)

  POP[[2]] = c(-5:-1)
  R = matrix(c(1,2), ncol=2, nrow=20, byrow=TRUE)
  checkEquals(select_best(POP, 20), R)

  POP[[2]] = c(5:1)
  R = matrix(c(9,10), ncol=2, nrow=1, byrow=TRUE)
  checkEquals(select_best(POP, 1), R)
}

test.select_rand = function() {
  POP = list()
  POP[[1]] = matrix(1:10, ncol=2, byrow=TRUE)
  POP[[2]] = c(1:2,-3:-1)
  result = select_rand(POP, 1)
  checkTrue(is.vector(result))
  checkEquals(length(result), 2)
  checkEquals(dim(select_rand(POP, 2)), c(2,2))
  checkEquals(dim(select_rand(POP, 20)),c(20, 2))
}

test.initialize_unif = function() {
  pop = initialize_unif(10, 2, c(-1,1))
  checkEquals(dim(pop), c(10,2))
  checkEquals(sum((pop < -1) + (pop > 1)), 0)

  pop = initialize_unif(1, 1, c(0,0))
  checkEquals(dim(matrix(pop)), c(1,1))
  checkEquals(sum(pop),0)

  pop = initialize_unif(10,20, c(0,0))
  checkEquals(sum(pop),0)
}

test.diff_vector = function() {
  # FIXME IMPLEMENT THIS
}

test.crossover_bin = function() {
  # FIXME IMPLEMENT THIS
}

test.crossover_exp = function() {
  # FIXME IMPLEMENT THIS
}

test.range_fit_truncate = function() {
  # FIXME IMPLEMENT THIS
}

test.range_fit_mirror = function() {
  # FIXME IMPLEMENT THIS
}

test.range_fit_roll = function() {
  # FIXME IMPLEMENT THIS
}

test.de = function() {
  # FIXME IMPLEMENT THIS
}

test.runExperiment = function() {
  # FIXME IMPLEMENT THIS
}
