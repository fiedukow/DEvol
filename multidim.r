library(stringr) #str_replace to support markdown bug

# MINIMIZE OR MAXIMIZE
best = min
which.best = which.min
better = `<`
worst = Inf

initialize_unif = function(n, dims, range) {
  matrix(replicate(n, runif(dims, range[1], range[2])), ncol=dims, byrow = TRUE)
}

select_best = function(p, N) {
  matrix(p[[1]][which.best(p[[2]]), ], N, ncol(p[[1]]), TRUE)
}

select_rand = function(p, N) {
  p[[1]][sample(nrow(p[[1]]), N, TRUE), ]
}

diff_vector = function(pop, N) {
  x1 = select_rand(list(pop), N)
  x2 = select_rand(list(pop), N)
  x1 - x2
}

crossover_bin = function(x, y, cr) {
  mod = t(replicate(nrow(x), (runif(ncol(x)) > cr)))
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

range_fit_mirror = function(p, range) {
  range_l = range[2] - range[1]
  mod_min = p > range[1]
  mirrored_min = range[1] + ((range[1] - p)%%range_l)
  p = mod_min*p + (1 - mod_min)*mirrored_min
  mod_max = p < range[2]
  mirrored_max = range[2] - ((p - range[2])%%range_l)
  p = mod_max*p + (1 - mod_max)*mirrored_max
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

# This is only valid for two demensions
draw_population = function(pop, pop_next, archive, mid, range, qual, no, name = "") {
  if (length(pop[[1]][1,]) != 2) {
    warning("Draw population is undefined for object dimensions size other then 2")
    return(NA)
  }
  png(file = paste0("img/", name, no, ".png"), width=800, height=800, bg = "white")
  x = seq(from = range[1], to = range[2], by = 1)
  y = seq(from = range[1], to = range[2], by = 1)
  contour(x, y, matrix(qual(as.matrix(expand.grid(x,y))),
                       nrow=length(x)))
  points(archive, col="gray", lwd=1)
  points(pop[[1]], col="blue", lwd=2, pch="O")
  points(pop_next, col="purple", lwd=2)
  points(select_best(pop, 2), col="red", lw=3)
  points(mid, col="green", pch="x", lw=3)
  dev.off()
}

de = function(dims, range, pop_size, diff_factor, init, select, crossover,
              cr, qual, generations, diff_size, range_fit = range_fit_mirror,
              N_history = pop_size, noise_sd = 1, best_possible = NA, near_enough = NA, name="") {
  if (N_history < pop_size)
    stop("History must be at least 1 population long")
  if (N_history %% pop_size != 0)
    stop("For now de() is well defined only for N_history being multiple of pop_size")

  last_candidats = matrix()
  pop_prev = list()
  pop_next = list()
  H_norm = list()
  pop_prev[[1]] = init(pop_size, dims, range)
  pop_prev[[2]] = qual(pop_prev[[1]])
  mean_pop_prev = mean(dist(pop_prev[[1]]))
  col_means = colMeans(pop_prev[[1]])
  col_means_matrix = matrix(col_means, nrow=pop_size, ncol=dims, byrow=TRUE)
  H_norm[[1]] = (pop_prev[[1]] - col_means_matrix) / mean_pop_prev
  N_runif = pop_size*dims

  result = list()
  result$values = c()
  result$mid_values = c()
  result$init_pop = pop_prev[[1]]
  result$record_value = worst
  result$record_mid_value = worst

  begin = Sys.time()
  for(i in 1:generations) {
    ##### DETERMINE GENERATION WIDE VALUES
    mid_point = matrix(colMeans(pop_prev[[1]]), ncol=dims)
    H_unnorm = t(t(H_norm[[1]]*mean_pop_prev)+as.vector(mid_point))

    ##### SAVING GENERATION DETAILS
    result$values[i] = qual(pop_prev[[1]][which.best(pop_prev[[2]]), ])
    result$record_value = best(result$record_value, result$values[i])
    result$mid_values[i] = qual(mid_point)
    result$record_mid_value = best(result$record_mid_value, result$mid_values[i])

    ##### CHECKING IF STOP IS POSSIBLE (GOAL REACHED)
    if (!is.na(best_possible) && !is.na(near_enough) &&
        abs(qual(result$values[i]) - best_possible) < near_enough) {
      print(paste("Found good enough in ", i, " generation."))
      break;
    }

    ##### DRAWING GENERATION IF POSSIBLE
    if (dims == 2)
      draw_population(pop_prev, last_candidats, H_unnorm, mid_point, range, qual, i, name)

    ##### GENERATING NEW POPULATION
    pop_next[[1]] = select(pop_prev, pop_size) +
                    diff_factor*diff_vector(H_unnorm, pop_size) +
                    rnorm(N_runif, mean = 0, sd = noise_sd)
    pop_next[[1]] = crossover(pop_prev[[1]], pop_next[[1]], cr)
    pop_next_fit = range_fit(pop_next[[1]], range)
    pop_next[[2]] = qual(pop_next_fit)
    last_candidats = pop_next[[1]]

    ###### SELECTING BETTER FROM PAIRS
    # TODO better turnament then simple selecting better.
    mod = better(pop_prev[[2]], pop_next[[2]])
    pop_next[[1]] = pop_prev[[1]]*mod + pop_next[[1]]*(1-mod)
    pop_next_fit = pop_prev[[1]]*mod + pop_next_fit*(1-mod)
    pop_next[[2]] = pop_prev[[2]]*mod + pop_next[[2]]*(1-mod)

    ###### UPDATING NORMALIZED ARCHIVE
    H_norm_next = t(t(pop_next[[1]]) - as.vector(mid_point))
    mean_pop_next = mean(dist(pop_next[[1]]))
    H_norm[[1]] = rbind(H_norm[[1]],
                        H_norm_next/mean_pop_next)

    ###### SHIFTING HISTORY WINDOW
    N = nrow(H_norm[[1]])
    H_norm[[1]] = H_norm[[1]][max(1, N - N_history + 1):N, ]

    ###### NEXT LOOP PREPARATION
    pop_next[[1]] = pop_next_fit
    pop_prev = pop_next

    ####### ITS HEAVY SO WE ARE AVOIDING CALCULATING IT AGAIN AT THE BEGINING OF THE LOOP
    mean_pop_prev = mean_pop_next
  }
  result$generation = length(result$values)
  result$generation_max = generations
  result$best_element = pop_prev[[1]][which.best(pop_prev[[2]]), ]
  result$best_qual = qual(result$best_element)
  result$time_taken = as.numeric(Sys.time())-as.numeric(begin)
  result$H_norm = H_norm
  result$last_pop = pop_prev
  result$middle = mid_point
  result$middle_qual = qual(result$middle)

  return(result)
}

save_results = function(de_result) {
  png(filename = paste("./EXPERIMENTS/",de_result$experiment_suite,"/single_results/", de_result$experiment_name, ".png", sep=""),
      width = 800, height = 600, units = "px", pointsize = 12,
      bg = "white")
  par(cex=1.2)
  plot(de_result$values, type="l",
       main=paste("DE", de_result$select[[2]], de_result$diff_size, de_result$crossover[[2]], sep="/"),
       xlab="generation",
       ylab="best quality function value",
       col="blue",
       ylim = c(min(c(de_result$values, de_result$mid_values)),
                max(c(de_result$values, de_result$mid_values))),
       lwd=4)
  lines(de_result$mid_values, col="red", lwd=2)
  par(par(xpd=TRUE))
  legend("top", c("best element in generation", "middle element of generation"),
         lty=c(1,1),
         lwd=c(4,2),
         col=c("blue","red"),
         bty="n")
  dev.off()
  fileConn<-file(paste("./EXPERIMENTS/",de_result$experiment_suite,"/single_results/", de_result$experiment_name, ".txt", sep=""))
  writeLines(paste(
    paste("Experiment: ", de_result$experiment_name, "; ",
          paste("DE", de_result$select[[2]], de_result$diff_size, de_result$crossover[[2]], sep="/"),
          sep=""),
    "-----------------------------------------",
    paste("Finished on: __", as.character.Date(Sys.time()), "__", sep = ""),
    paste(""),
    paste("Time taken: __",de_result$time_taken, "__ seconds", sep = ""),
    paste(""),
    paste("---------------------------------------"),
    paste(""),
    paste("__Quality function:__"),
    paste(""),
    paste(" * ", de_result$qual[[2]], " - ", de_result$qual[[3]]),
    paste(""),
    paste("---------------------------------------"),
    paste(""),
    paste("__Operators used:__"),
    paste(""),
    paste(" * Initialization: ", de_result$init[[2]], " - ", de_result$init[[3]]),
    paste(" * Selection: ", de_result$select[[2]], " - ", de_result$select[[3]]),
    paste(" * Crossover: ", de_result$crossover[[2]], " - ", de_result$crossover[[3]]),
    paste(" * Fitting into range: ", de_result$range_fit[[2]], " - ", de_result$range_fit[[3]]),
    paste(""),
    paste("---------------------------------------"),
    paste(""),
    paste("__Parameters:__"),
    paste(""),
    paste(" * Dimensions = ", length(de_result$H[[1]][1,]), sep=""),
    paste(" * Range = [", de_result$range[1], ", ", de_result$range[2], "]", sep=""),
    paste(" * Cr = ", de_result$cr, sep = ""),
    paste(" * μ = ", nrow(de_result$last_pop[[1]]), sep = ""),
    paste(" * μ_h = ", nrow(de_result$H[[1]]), sep = ""),
    paste(" * F = ", de_result$diff_factor, sep = ""),
    paste(" * generations = ", de_result$generation, sep = ""),
    paste(" * generationsMax = ", de_result$generation_max, sep = ""),
    paste(" * Last Generation Best = ", de_result$best_qual, sep = ""),
    paste(" * Record element value = ", de_result$record_value, sep = ""),
    paste(" * Record mid value = ", de_result$record_mid_value, sep = ""),
    paste(" * Perfect Known Value = ", de_result$best_possible, sep = ""),
    paste(" * Good Enough Range = ", de_result$near_enough, sep = ""),
    paste(""),
    paste("![](./", str_replace_all(de_result$experiment_name, " ", "%20"), ".png)", sep=""),
    paste(""),
    paste("---------------------------------------"),
    paste(""),
    paste("__Additional data:__"),
    paste(""),
    paste(" * [Raw Form Raport](./", de_result$experiment_name, ".txt)", sep=""),
    paste(" * [Best Object](./", de_result$experiment_name, "_best.txt)", sep=""),
    paste(" * [Final Population](./", de_result$experiment_name, "_pop.txt)", sep=""),
    paste(" * [best(generation)](./", de_result$experiment_name, "_values.txt)", sep=""),
    paste(" * [middle(generation)](./", de_result$experiment_name, "_middle.txt)", sep=""),
    paste(" * [Raw result dump](./", de_result$experiment_name, "_dump.r)", sep=""),
  sep="\n"), fileConn)
  close(fileConn)

  write(de_result$best_element, file=paste("./EXPERIMENTS/",de_result$experiment_suite,"/single_results/", de_result$experiment_name, "_best.txt", sep=""))
  write.table(de_result$last_pop[[1]], file=paste("./EXPERIMENTS/",de_result$experiment_suite,"/single_results/", de_result$experiment_name, "_pop.txt", sep=""),
              col.names=F, row.names=F)
  # Maybe dump quality function values as well?
  write(de_result$values, file=paste("./EXPERIMENTS/",de_result$experiment_suite,"/single_results/", de_result$experiment_name, "_values.txt", sep=""))
  write(de_result$mid_values, file=paste("./EXPERIMENTS/",de_result$experiment_suite,"/single_results/", de_result$experiment_name, "_middle.txt", sep=""))
  system(paste("./gen_html_report.sh \"", de_result$experiment_name, "\" \"",de_result$experiment_suite,"\"", sep=""))
  dump("de_result", file=paste("./EXPERIMENTS/",de_result$experiment_suite,"/single_results/", de_result$experiment_name, "_dump.r", sep=""))
}

runExperiment = function(experiment_name, dims, range, pop_size, diff_factor, init, select, crossover,
                         cr, qual, generations, diff_size, range_fit, N_history = pop_size,
                         noise_sd = 1, best_possible = NA, near_enough = NA, experiment_suite="unsuited") {
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
              range_fit[[1]],
              N_history,
              noise_sd,
              best_possible,
              near_enough,
              name=experiment_name);

  # appending params to result to generate report from that
  result$select = select
  result$crossover = crossover
  result$init = init
  result$diff_size = diff_size
  result$cr = cr
  result$diff_factor = diff_factor
  result$best_possible = best_possible
  result$qual = qual
  result$near_enough = near_enough
  result$range = range
  result$experiment_name = experiment_name
  result$range_fit = range_fit
  result$experiment_suite = experiment_suite

  save_results(result)

  return(result)
}
