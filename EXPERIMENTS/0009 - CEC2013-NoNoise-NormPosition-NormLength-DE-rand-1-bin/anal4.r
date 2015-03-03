SUITE="0009 - CEC2013-NoNoise-NormPosition-NormLength-DE-rand-1-bin"
setwd("/home//fiedukow/repo/DEvol/")

load(paste0("EXPERIMENTS/", SUITE, "/RESULT.dmp"))
source("named_functions.r")

qual_funs = list(CEC_2013_1, CEC_2013_2, CEC_2013_3, CEC_2013_4,
                 CEC_2013_5, CEC_2013_6, CEC_2013_7, CEC_2013_8,
                 CEC_2013_9, CEC_2013_10, CEC_2013_11, CEC_2013_12,
                 CEC_2013_13, CEC_2013_14, CEC_2013_15, CEC_2013_16,
                 CEC_2013_17, CEC_2013_18, CEC_2013_19, CEC_2013_20,
                 CEC_2013_21, CEC_2013_22, CEC_2013_23, CEC_2013_24,
                 CEC_2013_25, CEC_2013_26, CEC_2013_27, CEC_2013_28)
dims = c(10,30,50)
l = list()
for(dim in dims) {
  r = c()
  for(qual in qual_funs) {
    s = c()
    s = cbind(s, min(result_matrix[[qual[[2]]]][[dim]][,paste0("S10.BEST")]) - qual[[4]])
    s = rbind(s, mean(result_matrix[[qual[[2]]]][[dim]][,paste0("S10.BEST")]) - qual[[4]])
    s = rbind(s, max(result_matrix[[qual[[2]]]][[dim]][,paste0("S10.BEST")]) - qual[[4]])
    r = rbind(r, t(s))
  }
  l[[dim]] = r
}

X = read.csv("knowledge/ExternalData/DEVariants-CEC2013-Results.csv")
d = as.numeric(format(l[[30]][,2], digits=3, scientific=TRUE))
X[,"DEArch"] = d
apply(apply(X, 1, rank), 1, mean)
