source("multidim.r")
source("named_functions.r")


setwd("/home//fiedukow/repo/DEvol/")
runExperiment(experiment_name = "testrun",
                               dims = 5,
                               range = c(-5,5),
                               pop_size = 100,
                               diff_factor = 1.0,
                               init = I_UNIF,
                               select = S_BEST,
                               crossover = C_EXP,
                               cr = 0.75,
                               qual = CEC_2013_10,
                               best_possiblle = -330,
                               generations = 100,
                               near_enough = 0.001,
                               diff_size = 1)
