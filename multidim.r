library(mvtnorm)
library(distr)
library(cec2005benchmark)
library(cec2013)

# MINIMIZE OR MAXIMIZE
best = min
which.best = which.min

initialize_unif = function(n, dims, range) {
  matrix(replicate(n, runif(dims, range[1], range[2])), ncol=dims)
}

select_best_real = function(p, quality_function, best_element = NA) {
  matrix(p[which.best(quality_function(p)), ], ncol=ncol(p))
}

select_best = function(p, quality_function, best_element) {
  best_element
}

select_rand = function(p, quality_function, rand_element = NA) {
  matrix(p[sample(nrow(p), 1), ], ncol=ncol(p))
}

differentiator = function(element, pair, factor = 1.0) {
  element + factor*(pair[1,] - pair[2,])
}

crossover_bin = function(x, y, dims, cr) {
  mod = (runif(dims) < cr)
  mod*x + (1-mod)*y
}

crossover_exp = function(x, y, dims, cr) {
  mod = (runif(dims) < cr)
  mod = cummax(mod)
  mod*x + (1-mod)*y
}

better = function(x, y, qual) {
  p = matrix(c(x,y), ncol=length(x), byrow=TRUE)  
  select_best_real(p, qual)
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
                  init,
                  select,
                  crossover,
                  cr,
                  qual,
                  range,
                  best) { #This is passed as optimalization only 
  x = select(pop, qual, best)
  pair = pop[sample(nrow(pop), 2), ]
  y = differentiator(x, pair, diff_factor)
  z = crossover(xi, y, dims, cr)
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
  points(select_best_real(pop, qual), col="red", lw=2)
}

de = function(dims, range, pop_size, diff_factor,
              init, select, crossover,
              cr, qual, best_possiblle, generations, near_enough,
              diff_size) {
  pop = init(pop_size, dims, range)
  result = list()
  result$values = c()
  
  begin = Sys.time()
  for(i in 1:generations) {    
    best = select_best_real(pop, qual) # it will be passed for some of selection methods for optymalization 
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
                                                    init,
                                                    select,
                                                    crossover,
                                                    cr,
                                                    qual,
                                                    range,
                                                    best)
    }))  
    pop = pop_next
  }  
  result$generation = length(result$values)
  result$generation_max = generations
  result$best_element = select_best_real(pop, qual)
  result$best_qual = qual(result$best_element)
  result$time_taken = as.numeric(Sys.time())-as.numeric(begin)
  #result$qual = qual
  result$pop = pop

  return(result)
}

save_results = function(de_result) {
  dir.create("./results", showWarnings = FALSE)
  png(filename = paste("./results/", de_result$experiment_name, ".png", sep=""),
      width = 600, height = 600, units = "px", pointsize = 12,
      bg = "white")
  plot(de_result$values, type="l",
       main=paste("DE", de_result$select_type, de_result$diff_size, de_result$crossover_type, sep="/"),
       xlab="generation",
       ylab="best quality function value",
       col="blue",
       lwd=3)
  dev.off()  
  fileConn<-file(paste("./results/", de_result$experiment_name, ".txt", sep=""))
  writeLines(paste(
    paste("Experiment: ", de_result$experiment_name, "; ",
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
    paste(" * Quality Function: ", de_result$qual_description, sep=""),
    paste(""),
    paste("![](./", de_result$experiment_name, ".png)", sep=""),
    paste(""),
    paste("---------------------------------------"),
    paste(""),
    paste(" * [Raw Form Raport](./", de_result$experiment_name, ".txt)", sep=""),
    paste(" * [Best Object](./", de_result$experiment_name, "_best.txt)", sep=""),
    paste(" * [Final Population](./", de_result$experiment_name, "_pop.txt)", sep=""),
    paste(" * [best(generation)](./", de_result$experiment_name, "_values.txt)", sep=""),
  sep="\n"), fileConn)
  close(fileConn)
  
  write(de_result$best_element, file=paste("./results/", de_result$experiment_name, "_best.txt", sep=""))
  write(de_result$pop, file=paste("./results/", de_result$experiment_name, "_pop.txt", sep=""))
  write(de_result$values, file=paste("./results/", de_result$experiment_name, "_values.txt", sep=""))
  system(paste("./gen_html_report.sh ", de_result$experiment_name, sep=""))
}

runExperiment = function(experiment_name, dims, range, pop_size, diff_factor,
                         init, select, crossover,
                         cr, qual, best_possiblle,
                         generations, near_enough, diff_size) {
  result = de(dims,
              range,
              pop_size,
              diff_factor,
              init[[1]],
              select[[1]],
              crossover[[1]],
              cr,
              qual[[1]],
              best_possiblle,
              generations,
              near_enough,
              diff_size)

  # appending params to result to generate report from that
  result$select_type = select[[2]]
  result$crossover_type = crossover[[2]]
  result$init_type = init[[2]]
  result$diff_size = diff_size
  result$cr = cr
  result$diff_factor = diff_factor
  result$best_possible = best_possiblle
  result$qual_description = qual[[2]]
  result$near_enough = near_enough
  result$range = range
  result$experiment_name = experiment_name

  save_results(result)

  return(result)
}
