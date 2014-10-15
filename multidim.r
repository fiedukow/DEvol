# MINIMIZE OR MAXIMIZE
best = min
which.best = which.min
better = `<`

initialize_unif = function(n, dims, range) {
  matrix(replicate(n, runif(dims, range[1], range[2])), ncol=dims)
}

select_best = function(p) {
  matrix(p[[1]][which.best(p[[2]]), ], nrow(p[[1]]), ncol(p[[1]]), TRUE)
}

select_rand = function(p) {
  N = nrow(p[[1]])
  p[[1]][sample(N, N, TRUE), ]
}

diff_vector = function(pop) {
  x1 = select_rand(list(pop))
  x2 = select_rand(list(pop))
  x1 - x2
}

crossover_bin = function(x, y, cr) {
  mod = t(replicate(nrow(x), (runif(ncol(x)) < cr)))
  mod*x + (1-mod)*y
}

crossover_exp = function(x, y, cr) {
  mod = t(replicate(nrow(x), cummin(runif(ncol(x)) > cr)))
  mod*x + (1-mod)*y
}

# TODO: fitting into range should have more option then just
#       truncating the element into range.
range_fit = function(p, range) {
  pmin(pmax(p, range[1]), range[2])
}

# This is only valid for two demensions
draw_population = function(pop, range, qual) {
  # FIXME: This will not work after "Improving select_best_real to acctually use already calculated qual values."
  if (length(pop[1,]) != 2) {
    warning("Draw population is undefined for object dimensions size other then 2")
    return(NA)
  }
  x = seq(from = range[1], to = range[2], by = 0.01)
  y = seq(from = range[1], to = range[2], by = 0.01)
  contour(x, y, matrix(qual(as.matrix(expand.grid(x,y))),
                       nrow=length(x)))
  points(pop, col="blue")
  points(select_best(pop), col="red", lw=2)
}

de = function(dims, range, pop_size, diff_factor, init, select, crossover,
              cr, qual, generations, diff_size, best_possible = NA, near_enough = NA) {
  pop = list()
  pop_next = list()
  pop[[1]] = init(pop_size, dims, range)
  pop[[2]] = qual(pop[[1]])

  result = list()
  result$values = c()

  begin = Sys.time()
  for(i in 1:generations) {
    best_value = qual(pop[[1]][which.best(pop[[2]]), ])
    result$values[i] = best_value
    if (!is.na(best_possible) && !is.na(near_enough) &&
        abs(qual(best_value) - best_possible) < near_enough) {
      print(paste("Found good enough in ", i, " generation."))
      break;
    }
    pop_next[[1]] = select(pop) + diff_factor*diff_vector(pop[[1]])
    pop_next[[1]] = range_fit(crossover(pop[[1]], pop_next[[1]], cr), range)
    pop_next[[2]] = qual(pop_next[[1]])

    # TODO better turnament then simple selecting better.
    mod = better(pop[[2]], pop_next[[2]])
    pop[[1]] = pop[[1]]*mod + pop_next[[1]]*(1-mod)
    pop[[2]] = pop[[2]]*mod + pop_next[[2]]*(1-mod)
  }
  result$generation = length(result$values)
  result$generation_max = generations
  result$best_element = pop[[1]][which.best(pop[[2]]), ]
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
    paste(" * Dimensions = ", length(de_result$pop[[1]][1,]), sep=""),
    paste(" * Range = [", de_result$range[1], ", ", de_result$range[2], "]", sep=""),
    paste(" * Cr = ", de_result$cr, sep = ""),
    paste(" * μ = ", nrow(de_result$pop[[1]]), sep = ""),
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
  write(de_result$pop[[1]], file=paste("./results/", de_result$experiment_name, "_pop.txt", sep="")) # TODO
                                                                                        # Maybe dump results as well?
  write(de_result$values, file=paste("./results/", de_result$experiment_name, "_values.txt", sep=""))
  system(paste("./gen_html_report.sh ", de_result$experiment_name, sep=""))
}

runExperiment = function(experiment_name, dims, range, pop_size, diff_factor, init, select, crossover,
                         cr, qual, generations, diff_size, best_possible = NA, near_enough = NA) {
  result = de(dims,
              range,
              pop_size,
              diff_factor,
              init[[1]],
              select[[1]],
              crossover[[1]],
              cr,
              qual[[1]],
              generations,
              diff_size,
              best_possible,
              near_enough)

  # appending params to result to generate report from that
  result$select_type = select[[2]]
  result$crossover_type = crossover[[2]]
  result$init_type = init[[2]]
  result$diff_size = diff_size
  result$cr = cr
  result$diff_factor = diff_factor
  result$best_possible = best_possible
  result$qual_description = qual[[2]]
  result$near_enough = near_enough
  result$range = range
  result$experiment_name = experiment_name

  save_results(result)

  return(result)
}
