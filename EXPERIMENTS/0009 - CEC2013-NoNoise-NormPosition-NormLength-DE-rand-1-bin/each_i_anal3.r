each_i_anal3 = function(i) {
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
        val = read.csv2(paste("EXPERIMENTS/", SUITE, "/single_results/Small Population Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]_values.txt", sep=""),sep = " ", header=FALSE)
        mid = read.csv2(paste("EXPERIMENTS/", SUITE, "/single_results/Small Population Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]_middle.txt", sep=""),sep = " ", header=FALSE)
        val = as.double(c(as.matrix(val)))
        mid = as.double(c(as.matrix(mid)))
        final_pop = (10000*dim)/(((10*dim)/abs(N_hist_factor_n))+1)
        final_pop = min(final_pop, length(val))
        result[[N_hist_factor]][["BEST"]] = min(val[1:round(final_pop)])
        result[[N_hist_factor]][["BEST_MID"]] = min(mid[1:round(final_pop)])
      } else {
        result[[N_hist_factor]] = list()
        print(paste("Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]", sep=""))
        val = read.csv2(paste("EXPERIMENTS/", SUITE, "/single_results/Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]_values.txt", sep=""),sep = " ", header=FALSE)
        mid = read.csv2(paste("EXPERIMENTS/", SUITE, "/single_results/Mean Norm - ", qual[[3]], " DIM = ", dim, "; Hfactor = ", N_hist_factor_n, "; [", i, "]_middle.txt", sep=""),sep = " ", header=FALSE)
        val = as.double(c(as.matrix(val)))
        mid = as.double(c(as.matrix(mid)))
        final_pop = (10000*dim)/((10*dim)+1)
        final_pop = min(final_pop, length(val))
        #print(final_pop)
        result[[N_hist_factor]][["BEST"]] = min(val[1:round(final_pop)])
        result[[N_hist_factor]][["BEST_MID"]] = min(mid[1:round(final_pop)])
      }
    }
  }
  result
}
