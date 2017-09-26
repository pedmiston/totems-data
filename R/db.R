#' Connect to the Totems MySQL DB.
#' @export
connect_db <- function() {
  password <- Sys.getenv("MYSQL_EXPERIMENTER_PASSWORD")
  if (password == "") stop("Set MYSQL_EXPERIMENT_PASSWORD environment variable")
  DBI::dbConnect(RMySQL::MySQL(),
                 dbname = "Totems",
                 host = "128.104.130.116",
                 port = 3306,
                 user = "experimenter",
                 password = password)
}
