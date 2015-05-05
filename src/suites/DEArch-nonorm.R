source("src/DEArch.R")
source("src/named_functions.R")

runSuite = function()
{
  SUITE_NAME = "DEArch/rand/1/bin - basic setup"
  SUITE_DESCRIPTION = "Ten zestaw sprawdza działanie algorytmu DEArch (bez znormalizowanych punktów populacji, dla H_factor = 2,3,5)"
  DIFF_FACTOR = 0.5
  INIT = I_UNIF
  SELECT = S_RAND
  CROSSOVER = C_BIN
  CR = 0.9
  DIFF_SIZE = 1
  RANGE_FIT = RF_MIRROR
  NOISE_SD = 0
  TIMES = 51

  conn = Connect(sql_host, sql_db, sql_user, sql_password)
  suite_id = CreateSuite(conn, SUITE_NAME, SUITE_DESCRIPTION)

  AddSuiteParameter(conn, suite_id, "F",                    DIFF_FACTOR, ""                           );
  AddSuiteParameter(conn, suite_id, "init operator",        NA,          INIT[[2]]                    );
  AddSuiteParameter(conn, suite_id, "select operator",      NA,          SELECT[[2]]                  );
  AddSuiteParameter(conn, suite_id, "crossover operator",   NA,          CROSSOVER[[2]]               );
  AddSuiteParameter(conn, suite_id, "range fit operator",   NA,          RANGE_FIT[[2]]               );
  AddSuiteParameter(conn, suite_id, "Cr",                   CR,          ""                           );
  AddSuiteParameter(conn, suite_id, "diff pairs",           DIFF_SIZE,   ""                           );
  AddSuiteParameter(conn, suite_id, "mutation noise sd",    NOISE_SD,    ""                           );
  AddSuiteParameter(conn, suite_id, "runs per experiment",  TIMES,       ""                           );


  Disconnect(conn)

  dim_list = list(2, 10, 30, 50)
  h_factor_list = list(1.5, 2, 3, 5)
  q_list = list(CEC_2013_1,  CEC_2013_2,  CEC_2013_3,  CEC_2013_4,  CEC_2013_5,  CEC_2013_6,  CEC_2013_7,   CEC_2013_8,
                CEC_2013_9,  CEC_2013_10, CEC_2013_11, CEC_2013_12, CEC_2013_13, CEC_2013_14, CEC_2013_15,  CEC_2013_16,
                CEC_2013_17, CEC_2013_18, CEC_2013_19, CEC_2013_20, CEC_2013_21, CEC_2013_22, CEC_2013_23,  CEC_2013_24,
                CEC_2013_25, CEC_2013_26, CEC_2013_27, CEC_2013_28)

  for (DIM in dim_list)
  {
    for (H_FACTOR in h_factor_list)
    {
      RANGE = c(-100,100)
      POP_SIZE = round(max(DIM + 1, max(50, 2 * DIM)/H_FACTOR))
      GENERATIONS = round((10000 * DIM)/POP_SIZE)
      N_HISTORY = POP_SIZE * H_FACTOR

      for(QUAL in q_list)
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
  }
}
