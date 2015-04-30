source("src/NADE.R")
source("src/named_functions.r")

smoketest_runExperiment = function()
{
  SUITE_NAME = "DE/rand/1/bin"
  SUITE_DESCRIPTION = "Quick DE/rand/1/bin test on CEC 2013 #13."
  DIM = 30
  RANGE = c(-100,100)
  POP_SIZE = 10 * DIM
  DIFF_FACTOR = 0.9
  INIT = I_UNIF
  SELECT = S_RAND
  CROSSOVER = C_BIN
  CR = 0.9
  GENERATIONS = (10000 * DIM)/POP_SIZE
  DIFF_SIZE = 1
  RANGE_FIT = RF_MIRROR
  N_HISTORY = POP_SIZE
  NOISE_SD = 0
  TIMES = 51

  conn = Connect(sql_host, sql_db, sql_user, sql_password)
  suite_id = CreateSuite(conn, SUITE_NAME, SUITE_DESCRIPTION)

  AddSuiteParameter(conn, suite_id, "dim",                  DIM,         ""                           );
  AddSuiteParameter(conn, suite_id, "range",                NA,          paste(RANGE, collapse = ", "));
  AddSuiteParameter(conn, suite_id, "population size",      POP_SIZE,    ""                           );
  AddSuiteParameter(conn, suite_id, "F",                    DIFF_FACTOR, ""                           );
  AddSuiteParameter(conn, suite_id, "init operator",        NA,          INIT[[2]]                    );
  AddSuiteParameter(conn, suite_id, "select operator",      NA,          SELECT[[2]]                  );
  AddSuiteParameter(conn, suite_id, "crossover operator",   NA,          CROSSOVER[[2]]               );
  AddSuiteParameter(conn, suite_id, "range fit operator",   NA,          RANGE_FIT[[2]]               );
  AddSuiteParameter(conn, suite_id, "Cr",                   CR,          ""                           );
  AddSuiteParameter(conn, suite_id, "max generations",      GENERATIONS, ""                           );
  AddSuiteParameter(conn, suite_id, "diff pairs",           DIFF_SIZE,   ""                           );
  AddSuiteParameter(conn, suite_id, "history size",         N_HISTORY,   ""                           );
  AddSuiteParameter(conn, suite_id, "mutation noise sd",    NOISE_SD,    ""                           );
  AddSuiteParameter(conn, suite_id, "runs per experiment",  TIMES,       ""                           );


  Disconnect(conn)


  QUAL = CEC_2013_13
  runExperiment(suite_id,
                dims = DIM,
                range = RANGE,
                pop_size = POP_SIZE,
                diff_factor = DIFF_FACTOR,
                init = INIT,
                select = SELECT,
                crossover = CROSSOVER,
                cr = CR,
                qual = QUAL,
                generations = GENERATIONS,
                diff_size = DIFF_SIZE,
                range_fit = RANGE_FIT,
                N_history = N_HISTORY,
                noise_sd = NOISE_SD,
                times = TIMES)
}
