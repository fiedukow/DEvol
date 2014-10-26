source("multidim.r")
library(cec2005benchmark)
library(cec2013)

C_EXP       = c(crossover_exp,      "exp",      "Standard exponential crossover function."                             )
C_EXP_PERM  = c(crossover_exp_perm, "exp_p",    "Standard exponential crossover function with mask permutation."       )
C_BIN       = c(crossover_bin,      "bin",      "Standard binary crossover function."                                  )
I_UNIF      = c(initialize_unif,    "unif",     "Standard uniform initialization function."                            )
S_BEST      = c(select_best,        "best",     "Standard best selection function."                                    )
S_RAND      = c(select_rand,        "rand",     "Standard rand selection function."                                    )
RF_MIRROR   = c(range_fit_mirror,   "mirror",   "Standard function mirroring outranged point back into it."            )
RF_ROLL     = c(range_fit_roll,     "roll"  ,   "Standard function rolling outranged point to the other side of range.")
RF_TRUNCATE = c(range_fit_truncate, "truncate", "Standard function putting outrange point to the end of the range."    )

CEC_2013_1  = c(function(p) { cec2013(1,  p) }, "CEC 2013 #1",  "CEC 2013 #1 benchmark function." )
CEC_2013_2  = c(function(p) { cec2013(2,  p) }, "CEC 2013 #2",  "CEC 2013 #2 benchmark function." )
CEC_2013_3  = c(function(p) { cec2013(3,  p) }, "CEC 2013 #3",  "CEC 2013 #3 benchmark function." )
CEC_2013_4  = c(function(p) { cec2013(4,  p) }, "CEC 2013 #4",  "CEC 2013 #4 benchmark function." )
CEC_2013_5  = c(function(p) { cec2013(5,  p) }, "CEC 2013 #5",  "CEC 2013 #5 benchmark function." )
CEC_2013_6  = c(function(p) { cec2013(6,  p) }, "CEC 2013 #6",  "CEC 2013 #6 benchmark function." )
CEC_2013_7  = c(function(p) { cec2013(7,  p) }, "CEC 2013 #7",  "CEC 2013 #7 benchmark function." )
CEC_2013_8  = c(function(p) { cec2013(8,  p) }, "CEC 2013 #8",  "CEC 2013 #8 benchmark function." )
CEC_2013_9  = c(function(p) { cec2013(9,  p) }, "CEC 2013 #9",  "CEC 2013 #9 benchmark function." )
CEC_2013_10 = c(function(p) { cec2013(10, p) }, "CEC 2013 #10", "CEC 2013 #10 benchmark function.")
CEC_2013_11 = c(function(p) { cec2013(11, p) }, "CEC 2013 #11", "CEC 2013 #11 benchmark function.")
CEC_2013_12 = c(function(p) { cec2013(12, p) }, "CEC 2013 #12", "CEC 2013 #12 benchmark function.")
CEC_2013_13 = c(function(p) { cec2013(13, p) }, "CEC 2013 #13", "CEC 2013 #13 benchmark function.")
CEC_2013_14 = c(function(p) { cec2013(14, p) }, "CEC 2013 #14", "CEC 2013 #14 benchmark function.")
CEC_2013_15 = c(function(p) { cec2013(15, p) }, "CEC 2013 #15", "CEC 2013 #15 benchmark function.")
CEC_2013_16 = c(function(p) { cec2013(16, p) }, "CEC 2013 #16", "CEC 2013 #16 benchmark function.")
CEC_2013_17 = c(function(p) { cec2013(17, p) }, "CEC 2013 #17", "CEC 2013 #17 benchmark function.")
CEC_2013_18 = c(function(p) { cec2013(18, p) }, "CEC 2013 #18", "CEC 2013 #18 benchmark function.")
CEC_2013_19 = c(function(p) { cec2013(19, p) }, "CEC 2013 #19", "CEC 2013 #19 benchmark function.")
CEC_2013_20 = c(function(p) { cec2013(20, p) }, "CEC 2013 #20", "CEC 2013 #20 benchmark function.")
CEC_2013_21 = c(function(p) { cec2013(21, p) }, "CEC 2013 #21", "CEC 2013 #21 benchmark function.")
CEC_2013_22 = c(function(p) { cec2013(22, p) }, "CEC 2013 #22", "CEC 2013 #22 benchmark function.")
CEC_2013_23 = c(function(p) { cec2013(23, p) }, "CEC 2013 #23", "CEC 2013 #23 benchmark function.")
CEC_2013_24 = c(function(p) { cec2013(24, p) }, "CEC 2013 #24", "CEC 2013 #24 benchmark function.")
CEC_2013_25 = c(function(p) { cec2013(25, p) }, "CEC 2013 #25", "CEC 2013 #25 benchmark function.")
CEC_2013_26 = c(function(p) { cec2013(26, p) }, "CEC 2013 #26", "CEC 2013 #26 benchmark function.")
CEC_2013_27 = c(function(p) { cec2013(27, p) }, "CEC 2013 #27", "CEC 2013 #27 benchmark function.")
CEC_2013_28 = c(function(p) { cec2013(28, p) }, "CEC 2013 #28", "CEC 2013 #28 benchmark function.")
