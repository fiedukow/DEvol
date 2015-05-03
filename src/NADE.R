library(parallel)
source("src/sql_support.R")
source("src/CONST.R")

de = function(dims, range, pop_size, diff_factor, init, select, crossover,
              cr, qual, generations, diff_size, range_fit = range_fit_mirror,
              N_history = pop_size, noise_sd = 1, best_possible = NA, near_enough = NA) {
  if (N_history < pop_size)
    stop("History must be at least 1 population long")
  if (N_history %% pop_size != 0)
    stop("For now de() is well defined only for N_history being multiple of pop_size")

  N_runif = pop_size*dims

  pop_prev = list()
  pop_next = list()
  H_norm = list()
  pop_prev[[1]] = init(pop_size, dims, range)
  pop_prev[[2]] = qual(pop_prev[[1]])
  mean_pop_prev = mean(dist(pop_prev[[1]]))
  col_means = colMeans(pop_prev[[1]])
  col_means_matrix = matrix(col_means, nrow=pop_size, ncol=dims, byrow=TRUE)
  H_norm[[1]] = (pop_prev[[1]] - col_means_matrix) / mean_pop_prev

  result = list()
  result$best_values = c()
  result$mid_values = c()
  result$best_elements = c()
  result$mid_elements = c()
  result$adaptive_diff_factor = c()
  result$init_pop = pop_prev[[1]]

  for(i in 1:(generations+1)) {
    ##### SORT POP_PREV JUST TO MAKE CODE SIMPLER
    pop_order = order(pop_prev[[2]])
    pop_prev[[1]] = pop_prev[[1]][pop_order,]
    pop_prev[[2]] = pop_prev[[2]][pop_order]

    ##### DETERMINE GENERATION WIDE VALUES
    mid_point = matrix(colMeans(pop_prev[[1]]), ncol=dims)
    H_unnorm = pop_prev[[1]]#t(t(H_norm[[1]]*mean_pop_prev) + as.vector(mid_point))

    ##### SAVING GENERATION DETAILS
    result$best_values[i] = pop_prev[[2]][1] - best_possible
    result$best_elements = rbind(result$best_elements, pop_prev[[1]][1,])
    result$mid_values[i] = qual(mid_point) - best_possible
    result$mid_elements = rbind(result$mid_elements, mid_point)
    result$adaptive_diff_factor[i] = diff_factor

    if (i == generations)
      break; # We were here just to save final result

    ##### CHECKING IF STOP IS POSSIBLE (GOAL REACHED)
    if (!is.na(best_possible) && !is.na(near_enough) &&
        abs(qual(result$values[i]) - best_possible) < near_enough) {
      print(paste("Found good enough in ", i, " generation."))
      break;
    }

    ##### GENERATING NEW POPULATION
    selected = select(pop_prev, pop_size)
    vectors = diff_vector(H_unnorm, pop_size, selected)

    pop_next[[1]] = pop_prev[[1]][selected,] +
                    diff_factor * vectors +
                    rnorm(N_runif, mean = 0, sd = noise_sd)
    pop_next[[1]] = crossover(pop_prev[[1]], pop_next[[1]], cr)
    pop_next_fit = range_fit(pop_next[[1]], range)
    pop_next[[2]] = qual(pop_next_fit)

    ###### SELECTING BETTER FROM PAIRS
    mod = better(pop_prev[[2]], pop_next[[2]])
    pop_next[[1]] = pop_prev[[1]]*mod + pop_next[[1]]*(1-mod)
    pop_next_fit = pop_prev[[1]]*mod + pop_next_fit*(1-mod)
    pop_next[[2]] = pop_prev[[2]]*mod + pop_next[[2]]*(1-mod)

    new_mid_point = matrix(colMeans(pop_next[[1]]), ncol=dims)
    improved = which(mod == 0)
    any_improvement = (sum(1-mod) != 0)

    ###### UPDATING NORMALIZED ARCHIVE
    mean_pop_next = mean(dist(pop_next[[1]]))
    if (any_improvement) {
      H_norm_next = t(t(pop_next[[1]][improved,]) - as.vector(new_mid_point))
      if (length(improved) == 1)
        H_norm_next = t(H_norm_next) ### TODO: Remove this ugly hack
      H_norm[[1]] = rbind(H_norm[[1]],
                          H_norm_next/mean_pop_next)
    }

    ###### SHIFTING HISTORY WINDOW
    N = nrow(H_norm[[1]])
    H_norm[[1]] = H_norm[[1]][max(1, N - N_history + 1):N, ]

    ###### NEXT LOOP PREPARATION
    pop_next[[1]] = pop_next_fit
    pop_prev = pop_next

    ####### ITS HEAVY SO WE ARE AVOIDING CALCULATING IT AGAIN AT THE BEGINING OF THE LOOP
    mean_pop_prev = mean_pop_next
  }

  return(result)
}

runRun = function(experiment_id, dims, range, pop_size, diff_factor, init, select, crossover,
                  cr, qual, generations, diff_size, range_fit, N_history = pop_size,
                  noise_sd = 1, best_possible = NA, near_enough = NA) {
  conn = Connect(sql_host, sql_db, sql_user, sql_password)
  run_id = OpenRun(conn, experiment_id)

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
              near_enough);

  CloseRun(conn, run_id)

  #AddSeries(conn, run_id, "adaptive F", NA, result$adaptive_diff_factor)
  AddSeries(conn, run_id, "best values", NA, result$best_values)
  AddSeries(conn, run_id, "mid values", NA, result$mid_values)
  AddSeries(conn, run_id, "final best point", result$best_elements[length(result$best_elements)], NA)
  AddSeries(conn, run_id, "final mid point", result$mid_elements[length(result$mid_elements)], NA)

  Disconnect(conn)
  return(result)
}


runExperiment = function(suite_id, dims, range, pop_size, diff_factor, init, select,
                         crossover, cr, qual, generations, diff_size, range_fit, N_history = pop_size,
                         noise_sd = 1, near_enough = NA, times = 1) {
  conn = Connect(sql_host, sql_db, sql_user, sql_password)
  experiment_id = OpenExperiment(conn, suite_id)
  AddExperimentParameter(conn, experiment_id, "fitness function", value_numeric = "NULL", value_text = qual[[2]])
  AddExperimentParameter(conn, experiment_id, "dim",                  dims,        ""                           )
  AddExperimentParameter(conn, experiment_id, "population size",      pop_size,    ""                           )
  AddExperimentParameter(conn, experiment_id, "max generations",      generations, ""                           )
  AddExperimentParameter(conn, experiment_id, "range",                NA,          paste(range, collapse = ", "))
  AddExperimentParameter(conn, experiment_id, "history size",         N_history,   ""                           )

  Disconnect(conn)

  runCode = function(id)
  {
    runRun(experiment_id,
           dims,
           range,
           pop_size,
           diff_factor,
           init,
           select,
           crossover,
           cr,
           qual,
           generations,
           diff_size,
           range_fit,
           N_history,
           noise_sd,
           qual[[4]],
           near_enough)
  }
  mclapply(1:times, runCode, mc.cores=13)

  conn = Connect(sql_host, sql_db, sql_user, sql_password)
  CloseExperiment(conn, experiment_id)
  Disconnect(conn)
}

