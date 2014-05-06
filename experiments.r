source("multidim.r")

setwd("~/repo/mgr")
runExperiment(experiment_name = "TEST",
              dims = 50,
              range = c(-5,5),
              pop_size = 10,
              diff_factor = 1.0,
              init_type = "unif",
              select_type = "best",
              crossover_type = "exp",
              cr = 0.75,
              qual = function(p) { cec2005benchmark(10, p) },
              qual_description = "CEC 2005 #10",
              best_possiblle = -330,
              generations = 3,
              near_enough = 0.001,
              diff_size = 1)