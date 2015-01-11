SUITE="0008 - NoNoise NormPosition NormLength"
setwd("/home//fiedukow/repo/DEvol/")

library("foreach")
library("parallel")
library("doParallel")
library("cec2013")

source("multidim.r")
source("named_functions.r")
source(paste0("EXPERIMENTS/", SUITE, "/each_i.r"))

qual_funs = list(CEC_2013_1, CEC_2013_2, CEC_2013_9, CEC_2013_10)
dims = c(10,30,50)
N_hist_factors = c("1","2","10","100","0","-2","2","-10","10") ##### LOWEST ABSOLUTE VALUE FIRST!
N_hist_factors_no_zero = N_hist_factors[which(N_hist_factors != 0)]
smaller_pop = FALSE

result = list()
result_matrix = list()
last_init_pop = c()

for(qual in qual_funs) {
  result[[qual[[2]]]] = list()
  result_matrix[[qual[[2]]]] = list()
  for(dim in dims) {
    result[[qual[[2]]]][[dim]] = list()
    result_matrix[[qual[[2]]]][[dim]] = list()
    result[[qual[[2]]]][[dim]] = mclapply(1:25, each_i, mc.cores=9)

    unlisted = unlist(result[[qual[[2]]]][[dim]])
    ncolumns = (length(N_hist_factors_no_zero)*2)

    result_matrix[[qual[[2]]]][[dim]] =
      matrix(unlisted, ncol=ncolumns, byrow=T,
             dimnames = list(c(), names(unlisted)[1:ncolumns]))
  }
}

save("result", file=paste0("EXPERIMENTS/", SUITE, "/RESULT_pure.dmp"))
save("result_matrix", file=paste0("EXPERIMENTS/", SUITE, "/RESULT.dmp"))


