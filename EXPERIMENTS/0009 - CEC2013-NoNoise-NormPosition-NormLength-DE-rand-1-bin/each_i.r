each_i = function(i) {
  ONE_K = 1000
  result = list()
  source("multidim.r")
  source("named_functions.r")
  INIT = I_UNIF
  smaller_pop = FALSE
  for(N_hist_factor in N_hist_factors) {
    N_hist_factor_n = as.numeric(N_hist_factor)
    if (N_hist_factor_n == 0) {
      smaller_pop = TRUE
    } else {
      if (smaller_pop) {
        N_hist_factor = paste0("S", N_hist_factor)
        result[[N_hist_factor]] = list()
        print(paste("Small Population Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""))
        result_full =
          runExperiment(experiment_name = paste("Small Population Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""),
                        dims = dim,
                        range = c(-100,100),
                        pop_size = (10*dim)/abs(N_hist_factor_n), #10
                        diff_factor = 0.9,
                        init = INIT,
                        select = S_RAND,
                        crossover = C_BIN,
                        cr = 0.9,
                        qual = qual,
                        generations = ONE_K*abs(N_hist_factor_n), #1000
                        diff_size = 1,
                        range_fit = RF_MIRROR,
                        N_history = ((10*dim)/abs(N_hist_factor_n))*max(1,N_hist_factor_n), #10
                        noise_sd = 0.0,
                        experiment_suite=SUITE)
        result[[N_hist_factor]][["BEST"]] = result_full$record_value
        result[[N_hist_factor]][["BEST_MID"]] = result_full$record_mid_value
        result_full$init_pop
        INIT = c(function(n, dims, range) { last_init_pop[1:n,] }, "Reuse last init.", "Use same values as for previous experiment")
      } else {
        result[[N_hist_factor]] = list()
        print(paste("Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""))
        result_full =
          runExperiment(experiment_name = paste("Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""),
                        dims = dim,
                        range = c(-100,100),
                        pop_size = 10*dim, #10
                        diff_factor = 0.9,
                        init = INIT,
                        select = S_RAND,
                        crossover = C_BIN,
                        cr = 0.9,
                        qual = qual,
                        generations = ONE_K, #1000
                        diff_size = 1,
                        range_fit = RF_MIRROR,
                        N_history = 10*dim*N_hist_factor_n, #10
                        noise_sd = 0,
                        experiment_suite=SUITE)
        result[[N_hist_factor]][["BEST"]] = result_full$record_value
        result[[N_hist_factor]][["BEST_MID"]] = result_full$record_mid_value
        last_init_pop = result_full$init_pop
        INIT = c(function(n, dims, range) { last_init_pop[1:n,] }, "Reuse last init.", "Use same values as for previous experiment")
      }
    }
  }
  result
}
