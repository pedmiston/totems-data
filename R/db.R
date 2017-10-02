#' Connect to the Totems MySQL DB.
#' @param config_file Config file in yaml format. Defaults to "config.yml".
#' @export
connect_db <- function(config_file = "config.yml") {
  config <- yaml::yaml.load_file(config_file)
  DBI::dbConnect(RMySQL::MySQL(),
                 dbname = "Totems",
                 host = config$host,
                 port = config$port,
                 user = config$user,
                 password = config$password)
}
