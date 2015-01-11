setwd("/home//fiedukow/repo/DEvol/")
load("EXPERIMENTS/0001 - SD0 NormPosition NormLength/RESULT.dmp")
source("named_functions.r")
qual_funs = list(CEC_2013_1, CEC_2013_2, CEC_2013_9, CEC_2013_10)
dims = c(10,30,50)
N_hist_factors = c("1","2","10","100")

for(H in c("2","10")) {
dec = c()
for(qual in qual_funs) {
  c1 = c()
  c2 = c()
  for(dim in dims) {
    if (median(result[[qual[[2]]]][[dim]][[paste0("S-",H)]][["COLLECT"]][["BEST_SERIES"]]) ==
        median(result[[qual[[2]]]][[dim]][[paste0("S",H)]][["COLLECT"]][["BEST_SERIES"]]))
      c1 = c(c1, 17.0)
    else
      c1 = c(c1, round(wilcox.test(result[[qual[[2]]]][[dim]][[paste0("S-",H)]][["COLLECT"]][["BEST_SERIES"]],
                                   result[[qual[[2]]]][[dim]][[paste0("S",H)]][["COLLECT"]][["BEST_SERIES"]],
                                   "greater",
                                   paired = TRUE)$p.value, 5))
    if (median(result[[qual[[2]]]][[dim]][[1]][["COLLECT"]][["BEST_SERIES"]]) ==
        median(result[[qual[[2]]]][[dim]][[H]][["COLLECT"]][["BEST_SERIES"]]))
      c2 = c(c2, 17.0)
    else
      c2 = c(c2, round(wilcox.test(result[[qual[[2]]]][[dim]][[paste0("S-",H)]][["COLLECT"]][["BEST_MID_SERIES"]],
                                   result[[qual[[2]]]][[dim]][[paste0("S",H)]][["COLLECT"]][["BEST_MID_SERIES"]],
                                   "greater",
                                   paired = TRUE)$p.value, 5))
  }
  dec = cbind(dec, c1)
  dec = cbind(dec, c2)
}

write.csv(format(dec, nsmall=5), paste("EXPERIMENTS/0001 - SD0 NormPosition NormLength/Wilcoxon-",H,"vsS", H,".txt", sep=""));
}

