source("src/sql_support.R")

smoketest_sql_support = function() {
  conn = Connect("localhost", "DEvol", "DEvol", "devol")
  id_suite = CreateSuite(conn, name = "TestSuite", description = "Ciekawy suite testowy")
  AddSuiteParameter(conn, id_suite, "dane suita", 17.17, "test_value");
  id_experiment = OpenExperiment(conn, id_suite)
  AddExperimentParameter(conn, id_experiment, "dane_experymentu", 18.18, "test_value 2")
  for (i in 1:10)
  {
    run_id = OpenRun(conn, id_experiment)
    Sys.sleep(3)
    CloseRun(conn, run_id)
  }
  run_id = OpenRun(conn, id_experiment)
  AddSeries(conn, run_id, "tylko tekst", c("ala","ma","kota"), NA)
  AddSeries(conn, run_id, "tylko numeric", NA, rnorm(20))
  AddSeries(conn, run_id, "oba", rep("ala",20), rnorm(20))
  AddSeries(conn, run_id, "punkty", matrix(rnorm(12), nrow=3), NA)
  AddSeries(conn, run_id, "punkty and numeric", matrix(rnorm(36), nrow=12), 1:12+rnorm(12))
  CloseRun(conn, run_id)
  CloseExperiment(conn, id_experiment)
  Disconnect(conn)
}
