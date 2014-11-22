SUITE="0001 - SD0 NormPosition NormLength"
setwd("/home//fiedukow/repo/DEvol/")

source("multidim.r")
source("named_functions.r")

qual_funs = list(CEC_2013_1, CEC_2013_2, CEC_2013_9, CEC_2013_10)
dims = c(10,30,50)
N_hist_factors = c("1","2","10","0","-2","2","-10","10") ##### LOWEST ABSOLUTE VALUE FIRST!
smaller_pop = FALSE

result = list()
last_init_pop = c()

for(qual in qual_funs) {
  result[[qual[[2]]]] = list()
for(dim in dims) {
  result[[qual[[2]]]][[dim]] = list()
  for(N_hist_factor in N_hist_factors) {
    result[[qual[[2]]]][[dim]][[N_hist_factor]] = list()
    result[[qual[[2]]]][[dim]][[paste0("S",N_hist_factor)]] = list()
    result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]] = list()
    result[[qual[[2]]]][[dim]][[N_hist_factor]][["COLLECT"]] = list()
    result[[qual[[2]]]][[dim]][[paste0("S",N_hist_factor)]][["FULL"]] = list()
    result[[qual[[2]]]][[dim]][[paste0("S",N_hist_factor)]][["COLLECT"]] = list()
  }
for (i in 1:25) {
  INIT = I_UNIF
  smaller_pop = FALSE
for(N_hist_factor in N_hist_factors) {
  N_hist_factor_n = as.numeric(N_hist_factor)
  if (N_hist_factor_n == 0) {
    smaller_pop = TRUE
  } else {
    if (smaller_pop) {
      N_hist_factor = paste0("S", N_hist_factor)
      print(paste("Small Population Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""))
      result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]] =
        runExperiment(experiment_name = paste("Small Population Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""),
                      dims = dim,
                      range = c(-100,100),
                      pop_size = (2*dim)/abs(N_hist_factor_n), #10
                      diff_factor = 0.9,
                      init = INIT,
                      select = S_RAND,
                      crossover = C_BIN,
                      cr = 0.9,
                      qual = qual,
                      generations = 10*abs(N_hist_factor_n), #1000
                      diff_size = 1,
                      range_fit = RF_MIRROR,
                      N_history = ((2*dim)/abs(N_hist_factor_n))*max(1,N_hist_factor_n), #10
                      noise_sd = 0,
                      experiment_suite=SUITE)
      result[[qual[[2]]]][[dim]][[N_hist_factor]][["COLLECT"]][["BEST_SERIES"]][i] =
        result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$record_value
      result[[qual[[2]]]][[dim]][[N_hist_factor]][["COLLECT"]][["BEST_MID_SERIES"]][i] =
      result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$record_mid_value
      last_init_pop = result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$init_pop
      INIT = c(function(n, dims, range) { last_init_pop[1:n,] }, "Reuse last init.", "Use same values as for previous experiment")
    } else {
      print(paste("Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""))
      result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]] =
        runExperiment(experiment_name = paste("Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""),
                      dims = dim,
                      range = c(-100,100),
                      pop_size = 2*dim, #10
                      diff_factor = 0.9,
                      init = INIT,
                      select = S_RAND,
                      crossover = C_BIN,
                      cr = 0.9,
                      qual = qual,
                      generations = 10, #1000
                      diff_size = 1,
                      range_fit = RF_MIRROR,
                      N_history = 2*dim*N_hist_factor_n, #10
                      noise_sd = 0,
                      experiment_suite=SUITE)
      result[[qual[[2]]]][[dim]][[N_hist_factor]][["COLLECT"]][["BEST_SERIES"]][i] =
        result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$record_value
      result[[qual[[2]]]][[dim]][[N_hist_factor]][["COLLECT"]][["BEST_MID_SERIES"]][i] =
        result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$record_mid_value
      last_init_pop = result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$init_pop
      INIT = c(function(n, dims, range) { last_init_pop[1:n,] }, "Reuse last init.", "Use same values as for previous experiment")
    }
  }
}
}
}
}

save("result", file=paste0("EXPERIMENTS/", SUITE, "/RESULT.dmp"))

