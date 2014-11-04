setwd("/home//fiedukow/repo/DEvol/")

source("multidim.r")
source("named_functions.r")

qual_funs = list(CEC_2013_1, CEC_2013_2, CEC_2013_9, CEC_2013_10)
dims = c(10,30,50)
N_hist_factors = c(1,2,10)

result = list()
last_init_pop = c()

for(qual in qual_funs) {
  result[[qual[[2]]]] = list()
for(dim in dims) {
  result[[qual[[2]]]][[dim]] = list()
  for(N_hist_factor in N_hist_factors) {
    result[[qual[[2]]]][[dim]][[N_hist_factor]] = list()
    result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]] = list()
    result[[qual[[2]]]][[dim]][[N_hist_factor]][["COLLECT"]] = list()
  }
for (i in 1:25) {
  INIT = I_UNIF
for(N_hist_factor in N_hist_factors) {
  result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]] =
    runExperiment(experiment_name = paste(".", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor, "; [", i, "]", sep=""),
                  dims = dim,
                  range = c(-100,100),
                  pop_size = 10*dim,
                  diff_factor = 0.9,
                  init = INIT,
                  select = S_RAND,
                  crossover = C_BIN,
                  cr = 0.9,
                  qual = qual,
                  generations = 1000,
                  diff_size = 1,
                  range_fit = RF_MIRROR,
                  N_history = 10*dim*N_hist_factor,
                  noise_sd = 0.33)
  result[[qual[[2]]]][[dim]][[N_hist_factor]][["COLLECT"]][["BEST_SERIES"]][i] =
    result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$record_value
  result[[qual[[2]]]][[dim]][[N_hist_factor]][["COLLECT"]][["BEST_MID_SERIES"]][i] =
    result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$record_mid_value
  last_init_pop = result[[qual[[2]]]][[dim]][[N_hist_factor]][["FULL"]][[i]]$init_pop
  INIT = c(function(n, dims, range) { last_init_pop }, "Reuse last init.", "Use same values as for previous experiment")
}
}
}
}

dump("result", "planed_experiments_result.r")

