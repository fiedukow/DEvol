SUITE="0008 - NoNoise NormPosition NormLength"
setwd("/home//fiedukow/repo/DEvol/")

load(paste0("EXPERIMENTS/", SUITE, "/RESULT.dmp"))
source("named_functions.r")

qual_funs = list(CEC_2013_1, CEC_2013_2, CEC_2013_9, CEC_2013_10)
dims = c(10,30,50)

for (E in 1:3) {
  H_l = c(2,10)
  if (E == 1) H_l = c(2, 10, 100)
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
          P1 = ""
          V1 = "1"
          P2 = ""
          V2 = H
        } else if (E == 2) {
          P1 = ""
          V1 = "1"
          P2 = "S"
          V2 = H
        } else if (E == 3) {
          P1 = "S-"
          V1 = H
          P2 = "S"
          V2 = H
        }

        ids1 = paste0(P1,V1);
        ids2 = paste0(P2,V2); #IDS2 is always with modifications.
        s1  = result_matrix[[qual[[2]]]][[dim]][,paste0(ids1,".BEST")]
        s2  = result_matrix[[qual[[2]]]][[dim]][,paste0(ids2,".BEST")]
        s1m = result_matrix[[qual[[2]]]][[dim]][,paste0(ids1,".BEST_MID")]
        s2m = result_matrix[[qual[[2]]]][[dim]][,paste0(ids2,".BEST_MID")]

        c1  = c(c1,  round(wilcox.test(s1,  s2,  "greater", paired=TRUE)$p.value, 5))
        c2  = c(c2,  round(wilcox.test(s1m, s2m, "greater", paired=TRUE)$p.value, 5))

        wc1 = c(wc1, round(wilcox.test(s1,  s2,  "less",   paired=TRUE)$p.value, 5))
        wc2 = c(wc2, round(wilcox.test(s1m, s2m, "less",   paired=TRUE)$p.value, 5))
      }
      dec = cbind(dec, c1)
      dec = cbind(dec, c2)
      wdec = cbind(wdec, wc1)
      wdec = cbind(wdec, wc2)
    }
    r = dec * 0
    r[which(dec  <= 0.05)] = "+"
    r[which(wdec <= 0.05)] = "-"
    r[which(r    == 0   )] = "="
    write.csv(format(dec, nsmall=5), paste0("EXPERIMENTS/", SUITE, "/ANAL/","A",E,".Wilcoxon=", H,".csv"));
    write.csv(r , paste0("EXPERIMENTS/", SUITE, "/ANAL/","RA",E,".Wilcoxon=", H,".csv"))
  }
}
