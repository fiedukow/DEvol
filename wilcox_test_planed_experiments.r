source(planed_experiments_result.r)
qual_funs = list(CEC_2013_1, CEC_2013_2, CEC_2013_9, CEC_2013_10)
dims = c(10,30,50)
N_hist_factors = c(1,2,10)

for(H in c(2,10)) {
dec = c()
for(qual in qual_funs) {
  c1 = c()
  c2 = c()
  for(dim in dims) {
    c1 = c(c1, wilcox.test(result[[qual[[2]]]][[dim]][[1]]$values,
                           result[[qual[[2]]]][[dim]][[H]]$values,
                           "greater",
                           paired = TRUE)$p.value);
    c2 = c(c2, wilcox.test(result[[qual[[2]]]][[dim]][[1]]$mid_values,
                           result[[qual[[2]]]][[dim]][[H]]$mid_values,
                           "greater",
                           paired = TRUE)$p.value);
  }
  dec = cbind(dec, c1)
  dec = cbind(dec, c2)
}

write.csv(dec, paste("WilcoxonH=", H,".csv", sep=""));
}

