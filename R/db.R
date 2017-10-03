#' Connect to the Totems MySQL DB.
#' @param config_file Name of config file in yaml format. Defaults to "config.yml".
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

#' Create a blank config file.
#' @inheritParams connect_db
#' @import magrittr
create_blank_config <- function(config_file = "config.yml") {
  list(host="", port="", user="", password="") %>%
    yaml::as.yaml() %>%
    writeLines(con = config_file)
}
