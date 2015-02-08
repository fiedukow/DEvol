SUITE="0009 - CEC2013-NoNoise-NormPosition-NormLength-DE-rand-1-bin"
setwd("/home//fiedukow/repo/DEvol/")

load(paste0("EXPERIMENTS/", SUITE, "/a3_RESULT.dmp"))
result_matrix_a3 = result_matrix
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

for (E in 1:4) {
  H_l = c(2,10)
  if (E == 1) H_l = c(2, 10, 100)
  if (E == 4) H_l = c(1)
  for (H in H_l) {
    dec = c()
    wdec = c()
    for(qual in qual_funs) {
      c1 = c()
      c2 = c()
      wc1 = c()
      wc2 = c()
      for(dim in dims) {
        if (E == 1) {
          P2 = ""
          V2 = H
        } else if (E == 2) {
          P2 = "S"
          V2 = H
        } else if (E == 3) {
          P2 = "S-"
          V2 = H
        } else if (E == 4) {
          P2 = ""
          V2 = "1"
        }

        ids1 = paste0(P1,V1);
        ids2 = paste0(P2,V2); #IDS2 is always with modifications.
        s1  = round(result_matrix[[qual[[2]]]][[dim]][,paste0(ids2,".BEST")], 4)
        s2  = result_matrix_a3[[qual[[2]]]][[dim]][,paste0(ids2,".BEST")]
        s2m = result_matrix_a3[[qual[[2]]]][[dim]][,paste0(ids2,".BEST_MID")]
        mod_s2 = (s2 < s2m)
        s2 = as.double(s2)*mod_s2 + as.double(s2m)*(1-mod_s2)
        c1  = c(c1,  round(wilcox.test(s1,  s2,  "greater", paired=TRUE)$p.value, 5))
        wc1 = c(wc1, round(wilcox.test(s1,  s2,  "less",   paired=TRUE)$p.value, 5))
      }
      dec = cbind(dec, c1)
      wdec = cbind(wdec, wc1)
    }
    r = dec * 0
    r[which(dec  <= 0.05)] = "+"
    r[which(wdec <= 0.05)] = "-"
    r[which(r    == 0   )] = "="
    write.csv(format(dec, nsmall=5), paste0("EXPERIMENTS/", SUITE, "/ANAL3/","A",E,".Wilcoxon=", H,".csv"));
    write.csv(r , paste0("EXPERIMENTS/", SUITE, "/ANAL3/","RA",E,".Wilcoxon=", H,".csv"))
  }
}
