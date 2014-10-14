library(mvtnorm)
library(distr)
library(cec2005benchmark)
library(cec2013)

# MINIMIZE OR MAXIMIZE
best = min
which.best = which.min

# Cache of distribution randomizer function
# ~40% DE/X/X/EXP optimalization
rexpdist_r = NULL
rexpdist_cr = -1
rexpdist_dims = -1

# Distribution for exponential selection - just to make it faster
expdist = function(dims, cr) {
  p = cr^(0:(dims-1)) * (1-cr)
  p[dims+1] = 1 - sum(p)
  DiscreteDistribution(0:dims, p)
}

rexpdist = function(dims, cr) {
  if (cr != rexpdist_cr || dims != rexpdist_dims) {
    rexpdist_r <<- r(expdist(dims, cr))
    rexpdist_cr <<- cr
    rexpdist_dims <<- dims
  }
  rexpdist_r(1)
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

# Optimalization of selection - best is passed as argument
# it allows not to search for it every time
# TODO: Probably moving best_element to external variable will be better
#       as it will keep hacks consistent and interfaces as clear as possible.
select = function(p, quality_function, method = "rand", best_element = NA) {
  switch(method,
         rand = real_select(p, quality_function, method),
         best = best_element)
}

differentiator = function(element, pair, factor = 1.0) {
  element + factor*(pair[1,] - pair[2,])
}

crossover_bin = function(x, y, dims, cr) {
  mod = (runif(dims) < cr)
  mod*x + (1-mod)*y
}

crossover_exp = function(x, y, dims, cr) {
  #pos = rexpdist(dims, cr)
  #z = c(x[0:pos], y[(pos+1):max(dims,pos+1)])
  # NA are added if dims = pos
  #na.omit(z)
  mod = (runif(dims) < cr)
  mod = cummax(mod)
  mod*x + (1-mod)*y
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

# TODO: fitting into range should have more option then just
#       truncating the element into range.
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

# This is only valid for two demensions
draw_population = function(pop, range, qual) {
  if (length(pop[1,]) != 2) {
    warning("Draw population is undefined for object dimensions size other then 2")
    return(NA)
  }
  x = seq(from = range[1], to = range[2], by = 0.01)
  y = seq(from = range[1], to = range[2], by = 0.01)
  contour(x, y, matrix(qual(as.matrix(expand.grid(x,y))),
                       nrow=length(x)))
  points(pop, col="blue")
  points(real_select(pop, qual, "best"), col="red", lw=2)
}

de = function(dims, range, pop_size, diff_factor,
              init_type, select_type, crossover_type,
              cr, qual, best_possiblle, generations, near_enough,
              diff_size) {
  pop = initialize(pop_size, dims, range, init_type)
  result = list()
  result$values = c()
  
  begin = Sys.time()
  for(i in 1:generations) {    
    best = real_select(pop, qual, "best") # it will be passed for some of selection methods for optymalization 
                                          # it gives about 40% when using DE/best/X/X
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
  result$generation = length(result$values)
  result$generation_max = generations
  result$best_element = real_select(pop, qual, "best")
  result$best_qual = qual(result$best_element)
  result$time_taken = as.numeric(Sys.time())-as.numeric(begin)
  result$select_type = select_type
  result$crossover_type = crossover_type
  result$init_type = init_type
  result$diff_size = diff_size
  result$cr = cr
  result$diff_factor = diff_factor
  result$best_possible = best_possiblle
  result$qual = qual
  result$pop = pop
  result$near_enough = near_enough
  result$range = range
  return(result)
}

save_results = function(de_result, experiment_name, quality_function_description) {
  dir.create("./results", showWarnings = FALSE)
  png(filename = paste("./results/", experiment_name, ".png", sep=""),
      width = 600, height = 600, units = "px", pointsize = 12,
      bg = "white")
  plot(de_result$values, type="l",
       main=paste("DE", de_result$select_type, de_result$diff_size, de_result$crossover_type, sep="/"),
       xlab="generation",
       ylab="best quality function value",
       col="blue",
       lwd=3)
  dev.off()  
  fileConn<-file(paste("./results/", experiment_name, ".txt", sep=""))
  writeLines(paste(
    paste("Experiment: ", experiment_name, "; ",
          paste("DE", de_result$select_type, de_result$diff_size, de_result$crossover_type, sep="/"),
          sep=""),
    "-----------------------------------------",
    paste("_", as.character.Date(Sys.time()), "_", sep = ""),
    paste("Time taken: __",de_result$time_taken, "__", sep = ""),
    paste(""),
    paste(" * Dimensions = ", length(de_result$pop[1,]), sep=""),    
    paste(" * Range = [", de_result$range[1], ", ", de_result$range[2], "]", sep=""),
    paste(" * Cr = ", de_result$cr, sep = ""),
    paste(" * μ = ", nrow(de_result$pop), sep = ""),
    paste(" * F = ", de_result$diff_factor, sep = ""),
    paste(" * generations = ", de_result$generation, sep = ""),    
    paste(" * generationsMax = ", de_result$generation_max, sep = ""),
    paste(" * Init Method = ", de_result$init_type, sep = ""),
    paste(" * Best Found Value = ", de_result$best_qual, sep = ""),
    paste(" * Perfect Known Value = ", de_result$best_possible, sep = ""),
    paste(" * Good Enough Range = ", de_result$near_enough, sep = ""),
    paste(" * Quality Function: ", quality_function_description, sep=""),
    paste(""),
    paste("![](./", experiment_name, ".png)", sep=""),
    paste(""),
    paste("---------------------------------------"),
    paste(""),
    paste(" * [Raw Form Raport](./", experiment_name, ".txt)", sep=""),
    paste(" * [Best Object](./", experiment_name, "_best.txt)", sep=""),
    paste(" * [Final Population](./", experiment_name, "_pop.txt)", sep=""),
    paste(" * [best(generation)](./", experiment_name, "_values.txt)", sep=""),
  sep="\n"), fileConn)
  close(fileConn)
  
  write(de_result$best_element, file=paste("./results/", experiment_name, "_best.txt", sep=""))
  write(de_result$pop, file=paste("./results/", experiment_name, "_pop.txt", sep=""))
  write(de_result$values, file=paste("./results/", experiment_name, "_values.txt", sep=""))
  system(paste("./gen_html_report.sh ", experiment_name, sep=""))
}

runExperiment = function(experiment_name, dims, range, pop_size, diff_factor,
                         init_type, select_type, crossover_type,
                         cr, qual, qual_description, best_possiblle,
                         generations, near_enough, diff_size) {
  result = de(dims,
              range,
              pop_size,
              diff_factor,
              init_type,
              select_type,
              crossover_type,
              cr,
              qual,
              best_possiblle,
              generations,
              near_enough,
              diff_size)
  save_results(result, experiment_name, qual_description)  
}
