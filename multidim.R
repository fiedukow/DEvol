library(mvtnorm)
#library(lattice)
library(distr)
library(cec2005benchmark)

# MINIMIZE OR MAXIMIZE
best = min
which.best = which.min

# Distribution for exponential selection - just to make it faster
expdist = function(dims, cr) {
  p = cr^(0:(dims-1)) * (1-cr)
  p[dims+1] = 1 - sum(p)
  DiscreteDistribution(0:dims, p)
}

rexpdist = function(dims, cr) {
  r(expdist(dims, cr))(1)
}

my_quality = function(x, y) {
  m1 = c(2.5, 2.5)
  m2 = c(2.5, 2.5)
  m3 = c(4.0, 3.0)
  
  cv1 = matrix(c(1.00, 0.25,
                 0.25, 1.50), nrow=2)
  cv2 = diag(2)*1.1
  cv3 = matrix(c(2.0, 0.0,
                 0.0, 1.0), nrow =2)
  
  matrix(dmvnorm(expand.grid(x,y), m1, solve(cv1)) -
           dmvnorm(expand.grid(x,y), m2, solve(cv2)) +
           dmvnorm(expand.grid(x,y), m3, solve(cv3)) * 0.1,
         nrow = length(x))
}

initialize_unif = function(n, dims, range) {
  matrix(replicate(n, runif(dims, range[1], range[2])), ncol=dims)
}

initialize = function(n = 1, dims = 1, range, method = "unif") {
  switch(method,
         unif = initialize_unif(n, dims, range))
}

real_select = function(p, quality_function, method = "rand") {
  switch(method,
         rand = matrix(p[sample(nrow(p), 1), ], ncol=ncol(p)),
         best = matrix(p[which.best(quality_function(p)), ], ncol=ncol(p)))
}

select = function(p, quality_function, method = "rand", best_element = NA) {
  switch(method,
         rand = real_select(p, quality_function, method),
         best = best_element)
}

differentiator = function(element, pair, factor = 1.0) {
  element + factor*(pair[1,] - pair[2,])
}

crossover_bin = function(x, y, dims, cr) {
  ## TODO
}

crossover_exp = function(x, y, dims, cr) {
  pos = rexpdist(dims, cr)
  z = c(x[0:pos], y[(pos+1):max(dims,pos+1)])
  # NA are added if dims = pos
  na.omit(z)
}

crossover = function(x, y, dims=2, cr=0.5, method="bin") {
  switch(method,
         bin = crossover_bin(x, y, dims, cr),
         exp = crossover_exp(x, y, dims, cr))
}

better = function(x, y, qual) {
  p = matrix(c(x,y), ncol=length(x), byrow=TRUE)  
  real_select(p, qual, "best")
}

range_fit = function(X, range) {
  sapply(X, function(xi) { min(max(xi, range[1]), range[2]) } )  
}

each_x = function(xi,
                  pop,
                  dims,
                  diff_factor,
                  init_type,
                  select_type,
                  crossover_type,
                  cr,
                  qual,
                  range,
                  best) { #This is passed as optimalization only 
  x = select(pop, qual, select_type, best)
  pair = pop[sample(nrow(pop), 2), ]
  y = differentiator(x, pair, diff_factor)
  z = crossover(xi, y, dims, cr, crossover_type)
  z = range_fit(z, range)
  better(xi, z, qual)
}

draw_population = function(pop, range, qual) {
  x = seq(from = range[1], to = range[2], by = 0.01)
  y = seq(from = range[1], to = range[2], by = 0.01)
  contour(x, y, matrix(qual(as.matrix(expand.grid(x,y))),
                       nrow=length(x)))
  points(pop, col="blue")
  points(real_select(pop, qual, "best"), col="red", lw=2)
}

de = function(dims, range, pop_size, diff_factor,
              init_type, select_type, crossover_type,
              cr, qual, best_possiblle, generations, near_enough) {
  pop = initialize(pop_size, dims, range, init_type)
  result = list()
  result$values = c()
  
  begin = Sys.time()
  for(i in 1:generations) {    
    best = real_select(pop, qual, "best") # it will be passed for some of selection methods for optymalization 
                                          # it gives about 40% when using X/best/X
    if (abs(qual(best) - best_possiblle) < near_enough) {
      print(paste("Found good enough in ", i, " generation."))
      break;
    }
    result$values[i] = qual(best)
    pop_next = t(apply(pop, 1, function(x) { each_x(x,
                                                    pop,
                                                    dims,
                                                    diff_factor,
                                                    init_type,
                                                    select_type,
                                                    crossover_type,
                                                    cr,
                                                    qual,
                                                    range,
                                                    best)
    }))  
    pop = pop_next
  }
  result$pop = pop
  result$generation = length(result$values)
  result$best_element = real_select(pop, qual, "best")
  result$best_qual = qual(result$best_element)
  result$time_taken = as.numeric(Sys.time())-as.numeric(begin)
  return(result)
}

visalize = function(de_result) {
  plot(de_result$values, type="l")
  print(de_result$best_qual)
  print(paste("Time taken: ", de_result$time_taken))
  #if (dims == 2) draw_population(pop, range, qual)
  print("Best object:")
  print(as.vector(de_result$best_element))
  print(paste("Best result: ", de_result$best_qual))
  print(paste("Generations: ", de_result$generation))
}

ex = list()
ex[[1]] = de(dims = 50,
             range = c(-5,5),
             pop_size = 100,
             diff_factor = 1.0,
             init_type = "unif",
             select_type = "best",
             crossover_type = "exp",
             cr = 0.75,
             qual = function(p) { cec2005benchmark(10, p) },
             best_possiblle = -330,
             generations = 100,
             near_enough = 0.001)
ex[[2]] = de(dims = 50,
             range = c(-5,5),
             pop_size = 1000,
             diff_factor = 1.0,
             init_type = "unif",
             select_type = "best",
             crossover_type = "exp",
             cr = 0.75,
             qual = function(p) { cec2005benchmark(10, p) },
             best_possiblle = -330,
             generations = 100,
             near_enough = 0.001)
ex[[3]] = de(dims = 50,
             range = c(-5,5),
             pop_size = 100,
             diff_factor = 1.0,
             init_type = "unif",
             select_type = "best",
             crossover_type = "exp",
             cr = 0.75,
             qual = function(p) { cec2005benchmark(10, p) },
             best_possiblle = -330,
             generations = 1000,
             near_enough = 0.001)

visalize(ex[[1]])
visalize(ex[[2]])
visalize(ex[[3]])
