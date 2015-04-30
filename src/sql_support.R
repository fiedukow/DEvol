library(RMySQL)

# returns connection
Connect = function(host_, db, login, pass) {
  dbConnect(RMySQL::MySQL(), host = host_, dbname = db, username = login, password = pass)
}

Disconnect = function(conn) {
  dbClearResult(dbListResults(conn)[[1]])
  dbDisconnect(conn)
}

# Returns ID of suite
CreateSuite = function(conn, name, description) {
  Query = paste0(
    "INSERT INTO `Suite` (`name`, `description`) VALUES (\"",name,"\", \"",description,"\");"
  )
  dbSendQuery(conn, Query)

  Query = "SELECT LAST_INSERT_ID();"
  res = dbSendQuery(conn, Query)
  dbFetch(res)[1,1]
}

AddSuiteParameter = function(conn, suite_id, key, value_numeric, value_text) {
  if (is.na(value_numeric))
    value_numeric = "NULL"

  Query = paste0(
    "INSERT INTO `SuiteParameter` (`suite_id`, `name`, `value_numeric`, `value_text`) VALUES ",
    "(", suite_id, ", \"",key,"\", ",value_numeric,", \"", value_text, "\");"
  )
  dbSendQuery(conn, Query)
}

# Returns ID of experiment
OpenExperiment = function(conn, suite_id) {
  Query = paste0(
    "INSERT INTO `Experiment` (`suite_id`, `start_timestamp`, `end_timestamp`) VALUES ",
    "(\"",suite_id,"\", UNIX_TIMESTAMP(NOW()), UNIX_TIMESTAMP(NOW()));"
  )
  dbSendQuery(conn, Query)

  Query = "SELECT LAST_INSERT_ID();"
  res = dbSendQuery(conn, Query)
  dbFetch(res)[1,1]
}

# end time is set
CloseExperiment = function(conn, experiment_id) {
  Query = paste0(
    "UPDATE `Experiment` SET `end_timestamp`=UNIX_TIMESTAMP(NOW()) WHERE `id`=", experiment_id, ";"
  )
  dbSendQuery(conn, Query)
}

AddExperimentParameter = function(conn, experiment_id, key, value_numeric, value_text) {
  Query = paste0(
    "INSERT INTO `ExperimentParameter` (`experiment_id`, `name`, `value_numeric`, `value_text`) VALUES ",
    "(", experiment_id, ", \"",key,"\", ",value_numeric,", \"", value_text, "\");"
  )
  dbSendQuery(conn, Query)
}

# Returns ID of run
OpenRun = function(conn, experiment_id) {
  Query = paste0(
    "INSERT INTO `Run` (`experiment_id`, `start_timestamp`, `end_timestamp`) VALUES ",
    "(\"",experiment_id,"\", UNIX_TIMESTAMP(NOW()), UNIX_TIMESTAMP(NOW()));"
  )
  dbSendQuery(conn, Query)

  Query = "SELECT LAST_INSERT_ID();"
  res = dbSendQuery(conn, Query)
  dbFetch(res)[1,1]
}

# final data are set
CloseRun = function(conn, run_id) {
  Query = paste0(
    "UPDATE `Run` SET `end_timestamp`=UNIX_TIMESTAMP(NOW()) WHERE `id`=", run_id, ";"
  )
  dbSendQuery(conn, Query)
}

#data_string may be matrix - each row will be stringified
AddSeries = function(conn, run_id, name, data_string, data_double) {
  if (is.matrix(data_string)) {
    data_string = MatrixToString(data_string)
  }
  l = max(length(data_string), length(data_double))
  if (length(data_double) > 1 || is.na(data_double))
    data_double = rep(NA, l)
  if (length(data_string) > 1 || is.na(data_string))
    data_string = rep("", l)
  if (length(data_string) != length(data_double))
    stop("It shouldn't happen")

  for (i in 1:l)
  {
    double = data_double[i]
    if (is.na(double))
      double ="NULL"
    Query = paste0(
      "INSERT INTO `Series` (`run_id`, `name`, `value_numeric`, `value_text`, `order`) VALUES ",
      "(", run_id, ", \"", name, "\", ", double, ", \"", data_string[i],"\", ", i, ");"
    )
    dbSendQuery(conn, Query)
  }
}

MatrixToString = function(matrix)
{
  apply(matrix, 1, function(x) { paste(x, collapse=", ") })
}
